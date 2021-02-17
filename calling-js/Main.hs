import Control.Monad (replicateM_)
import Asterius.Types (JSVal)

foreign import javascript "Math.random()" js_random :: IO Double
foreign import javascript "console.log($1)" js_print_double :: Double -> IO ()
foreign import javascript "new Date()" js_current_time :: IO JSVal
foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()

main :: IO ()
main = do
  replicateM_ 5 $ js_random >>= js_print_double
  js_current_time >>= js_print
