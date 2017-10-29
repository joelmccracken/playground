> module TestLib where

> testTrue :: String -> Bool -> IO ()
> testTrue str True  = putStrLn ("     OK " ++ str)
> testTrue str False = putStrLn ("!!!! ERROR " ++ str)
