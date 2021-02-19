import Asterius.Types (JSVal)

foreign import javascript "wrapper" js_makeCallback :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript "console.log($1)" js_log :: JSVal -> IO ()
foreign import javascript "document.addEventListener(\"click\", $1)" js_addEventListener :: JSVal -> IO ()

eventListener :: String -> JSVal -> IO ()
eventListener msg e = do
  putStrLn msg
  js_log e

main :: IO ()
main = do
  callback <- js_makeCallback $ eventListener "Click!\n/From Haskell"
  js_addEventListener callback
