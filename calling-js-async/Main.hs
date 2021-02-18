import System.IO (hFlush, stdout)

foreign import javascript safe "new Promise(r => setTimeout(r, $1))" js_wait :: Int -> IO ()

main :: IO ()
main = do
  putStrLn "Printed immediately"
  hFlush stdout
  js_wait 2500
  putStrLn "Printed after 2500 ms"
  hFlush stdout
