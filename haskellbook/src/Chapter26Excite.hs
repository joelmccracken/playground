import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans.Class

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "Say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "Moar excite"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)
