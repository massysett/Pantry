module Pantry.Session ( serverMain ) where

import Pantry.Radio.Server (
  getListener, getRequest, processBag, Listener,
  serverLogFileName )
import Pantry.Radio ( pantryDir,
                      PantryDirInfo(IsDefaultDir, NotDefaultDir) )
import Pantry.Radio.Messages ( clientCurrDir )
import Pantry.Bag(Bag, emptyBag)
import Pantry.Parser ( getConveyor )
import qualified System.Posix.Process as P
import qualified System.Posix.IO as IO
import qualified System.Directory as D
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT
import Control.Monad ( void, when )
import qualified System.Console.OptParse.OptParse as O
import System.Console.OptParse.SimpleParser ( SimpleErr ( SimpleErr ) )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Data.Text ( Text, pack, unpack )
import Control.Exception ( bracket )

data Opts = Opts { daemon :: Bool
                 , help :: Bool }

defaultOpts :: Opts
defaultOpts = Opts { daemon = True
                   , help = False }

optDescs :: [O.OptDesc Opts err]
optDescs = [
  let argDesc = O.Flag (\o -> return o { daemon = False })
  in O.OptDesc "f" [pack "--foreground"] argDesc

  , let argDesc = O.Flag f
        f o = return o { help = True, daemon = False }
    in O.OptDesc "h" [pack "--help"] argDesc
  ]

posArgParser :: [(Opts, Text)] -> Either SimpleErr ()
posArgParser ps = let
  err = pack "pantryd does not accept non-option arguments."
  in case ps of
    [] -> Right ()
    _ -> Left (SimpleErr err)

errorExit :: String -> IO a
errorExit s = do
  putStrLn $ "pantryd: error: " ++ s
  putStrLn $ "Run \"pantryd --help\" for help."
  exitFailure

displayHelp :: IO ()
displayHelp = do
  putStrLn $ "pantryd - start the Pantry server"
  putStrLn $ "usage: pantryd [options]"
  putStrLn $ "Options:"
  putStrLn $ "  -f | --foreground - Do not become a daemon"
  putStrLn $ "  -h | --help - show this help"
  putStrLn $ "Not starting the server, exiting successfully."

parseCommandLine :: IO Opts
parseCommandLine = do
  asStr <- getArgs
  let as = map pack asStr
  let ei = O.parseOptsArgs optDescs defaultOpts posArgParser as
  case ei of
    (Left (SimpleErr err)) -> errorExit (unpack err)
    (Right (opts, ())) -> return opts

-- | Runs the server. Acquires the listening socket first; the main
-- session loop then sets up a bracket that catches any exceptions.
serverMain :: IO ()
serverMain = do
  opts <- parseCommandLine
  when (help opts) (displayHelp >> exitSuccess)
  if daemon opts
    then launchDaemon
    else bracket
         getListener
         (\(f, _) -> D.removeFile f)
         (\(_, l) -> sessionLoop emptyBag l)

sessionLoop :: Bag
               -> Listener
               -> IO ()
sessionLoop b l = do
  r <- getRequest l
  case r of
    Nothing -> sessionLoop b l
    (Just m) -> do
      let conveyor = getConveyor m
          cd = clientCurrDir m
      maybeNewBag <- processBag b cd conveyor
      case maybeNewBag of
        Nothing -> return ()
        (Just newBag) -> sessionLoop newBag l

-- | Makes the .pantry directory, but only if the directory is the
-- default. Otherwise, let the user go and create it.
makePantryDir :: IO ()
makePantryDir = do
  (dir, def) <- pantryDir
  case def of
    IsDefaultDir -> D.createDirectoryIfMissing False dir
    NotDefaultDir -> return ()
  
rwrr :: PT.FileMode
rwrr = PF.nullFileMode
       `PF.unionFileModes` PF.ownerReadMode
       `PF.unionFileModes` PF.ownerWriteMode
       `PF.unionFileModes` PF.groupReadMode
       `PF.unionFileModes` PF.otherReadMode

daemonizeForkAction :: (String, Listener) -> IO ()
daemonizeForkAction p = bracket acq rel use where
  acq = return p
  rel (f, _) = D.removeFile f
  use (_, l) = do
    D.setCurrentDirectory "/"
    IO.closeFd IO.stdInput
    IO.closeFd IO.stdOutput
    IO.closeFd IO.stdError
    -- reopen standard input, output, error
    void $ IO.openFd "/dev/null" IO.ReadOnly Nothing IO.defaultFileFlags
    makePantryDir
    logfile <- serverLogFileName
    let flags = IO.defaultFileFlags { IO.append = True }
    -- Do it twice - once for stdout, once for stderr
    _ <- IO.openFd logfile IO.WriteOnly (Just rwrr) flags
    _ <- IO.openFd logfile IO.WriteOnly (Just rwrr) flags
    sessionLoop emptyBag l
  
-- | Starts a session by daemonizing. Simply dies if anything goes
-- wrong. See the documentation for System.Posix.Process.forkProcess;
-- it might not work in the way you think it does. Acquires the
-- listening socket first, before forking, to aid in reporting common
-- errors such as the socket filename already existing.
launchDaemon :: IO ()
launchDaemon = do
  l <- getListener
  P.forkProcess (startSessionDaemon l) >> return ()

startSessionDaemon :: (String, Listener) -> IO ()
startSessionDaemon l = 
  P.createSession
  >> P.forkProcess (daemonizeForkAction l)
  >> return ()

{-

From http://www.faqs.org/faqs/unix-faq/programmer/faq/
Section 1.7 describes how to daemonize:

  1. `fork()' so the parent can exit, this returns control to the command
     line or shell invoking your program.  This step is required so that
     the new process is guaranteed not to be a process group leader. The
     next step, `setsid()', fails if you're a process group leader.

  2. `setsid()' to become a process group and session group leader. Since a
     controlling terminal is associated with a session, and this new
     session has not yet acquired a controlling terminal our process now
     has no controlling terminal, which is a Good Thing for daemons.

  3. `fork()' again so the parent, (the session group leader), can exit.
     This means that we, as a non-session group leader, can never regain a
     controlling terminal.

  4. `chdir("/")' to ensure that our process doesn't keep any directory in
     use. Failure to do this could make it so that an administrator
     couldn't unmount a filesystem, because it was our current directory.

     [Equivalently, we could change to any directory containing files
     important to the daemon's operation.]

  5. `umask(0)' so that we have complete control over the permissions of
     anything we write. We don't know what umask we may have inherited.

     [This step is optional]

  6. `close()' fds 0, 1, and 2. This releases the standard in, out, and
     error we inherited from our parent process. We have no way of knowing
     where these fds might have been redirected to. Note that many daemons
     use `sysconf()' to determine the limit `_SC_OPEN_MAX'.  `_SC_OPEN_MAX'
     tells you the maximun open files/process. Then in a loop, the daemon
     can close all possible file descriptors. You have to decide if you
     need to do this or not.  If you think that there might be
     file-descriptors open you should close them, since there's a limit on
     number of concurrent file descriptors.

  7. Establish new open descriptors for stdin, stdout and stderr. Even if
     you don't plan to use them, it is still a good idea to have them open.
     The precise handling of these is a matter of taste; if you have a
     logfile, for example, you might wish to open it as stdout or stderr,
     and open `/dev/null' as stdin; alternatively, you could open
     `/dev/console' as stderr and/or stdout, and `/dev/null' as stdin, or
     any other combination that makes sense for your particular daemon.

-}
