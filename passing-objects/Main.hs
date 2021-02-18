{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Asterius.Aeson (jsonFromJSVal, jsonToJSVal)
import Asterius.Types (JSVal)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, object, (.:), (.=))

data Coordinate = Coordinate {
    x :: Double
  , y :: Double
  }

instance FromJSON Coordinate where
  parseJSON = withObject "Coordinate" $ \o -> do
    x <- o .: "x"
    y <- o .: "y"
    return $ Coordinate{x, y}

instance ToJSON Coordinate where
  toJSON Coordinate{x, y} = object [
      "x" .= x
    , "y" .= y
    ]

foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()
foreign import javascript "{ x: 42.0, y: 13.37 }" js_coordinate :: IO JSVal

main :: IO ()
main = do
  let coord = Coordinate { x = 12.0, y = 13.0 }
  js_print $ jsonToJSVal coord
  jsResult <- jsonFromJSVal <$> js_coordinate
  case jsResult of
    Left err      -> putStrLn $ "Could not parse: " ++ err
    Right jsCoord -> putStrLn $ "x value from JS is: " ++ (show $ x jsCoord)
