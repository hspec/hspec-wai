
module Test.Hspec.Wai.Server (
  withApplication,
  openFreePort,
) where

import           Control.Concurrent
import           Control.Exception
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp

data App
  = App {
    appThread :: ThreadId,
    appWaitForKilled :: IO (),
    appExceptionMVar :: MVar (Maybe SomeException),
    appPort :: Int
  }

-- | Allows to test 'Application's over a real network port.
--
-- Runs the given 'Application' on a free port. Passes the free port to the
-- given operation and executes it.
withApplication :: IO Application -> (Port -> IO a) -> IO a
withApplication mkApp action = do
  app <- mkApp
  bracket (acquire app) free (\ runningApp -> action (appPort runningApp))
  where
    acquire :: Application -> IO App
    acquire app = do
      start <- mkWaiter
      killed <- mkWaiter
      exceptionMVar_ <- newMVar Nothing
      thread <- forkIO $ do
        (port, sock) <- openFreePort
        let settings =
              setBeforeMainLoop (notify start port)
              defaultSettings
        runSettingsSocket settings sock (handleApp exceptionMVar_ app)
          `finally` notify killed ()
      port <- waitFor start
      return $ App thread (waitFor killed) exceptionMVar_ port

    free :: App -> IO ()
    free runningApp = do
      killThread $ appThread runningApp
      appWaitForKilled runningApp
      exception <- readMVar (appExceptionMVar runningApp)
      case exception of
        Nothing -> return ()
        Just e -> throwIO e

handleApp :: MVar (Maybe SomeException) -> Application -> Application
handleApp mvar app request respond = do
  catch (app request respond) $ \ e -> do
    modifyMVar_ mvar $ \ _ ->
      return (Just e)
    throwIO e

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- | Opens a socket on a free port returns both port and socket.
openFreePort :: IO (Port, Socket)
openFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
