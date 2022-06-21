module Main where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified Lucid as L
import Lucid.Base
import Network.HTTP.Types hiding (Header)
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Server.Internal
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3002..."
  run 3002 $ logStdout (compress app)
  where
    compress = gzip def {gzipFiles = GzipCompress}

-- | API type
type API = ("static" :> Raw)

app :: Application
app = serve (Proxy @ API) (static)
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")
