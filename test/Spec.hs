import Hadruki.Logger (withHandle, Config(..), Verbosity(..))
import qualified Hadruki.Logger as LOG

main :: IO ()
main = do
  putStrLn "Start test..."
  let c = mempty
  withHandle c $ \h -> do
    LOG.error h "error"
    LOG.warning h "warning"
    LOG.info h "info"
    LOG.debug h "debug"
