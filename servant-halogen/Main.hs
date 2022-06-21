{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified Lucid as L
import Lucid.Base
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import RIO
import Servant
import Servant.Server.Internal
import System.Environment (lookupEnv)
import qualified System.IO as IO

main :: IO.IO ()
main = do
  port <- fmap (fromMaybe 8080 . join . fmap readMaybe) $ lookupEnv "PORT"
  staticFilePath <- fromMaybe "/var/www" <$> lookupEnv "STATIC_FILE_PATH"
  IO.hPutStrLn IO.stderr $ "Running on port " <> (show port) <> "..."
  run port $ logStdout (compress $ app staticFilePath)
  where
    compress = gzip def {gzipFiles = GzipCompress}

-- | API type
type API = ("static" :> Raw)

myAPI :: Proxy API
myAPI = Proxy

app :: FilePath -> Application
app path = serve myAPI (static)
  where
    static = serveDirectoryWith (defaultWebAppSettings path)
