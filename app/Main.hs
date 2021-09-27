module Main where

import LC.Ciphers

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

main :: IO ()
main = do
  secret <- readFile "secret"
  putStrLn "Hay un secreto" *> putStrLn secret
  putStrLn "Adivina una palabra de texto original para decifrar el secreto: "
  guess <- getLine
  let cand = candidates secret (normalize guess)
  putStrLn "Los candidatos son: " *> mapM_ putStrLn (showLn <$> cand)
  putStrLn "¿Alguno es el texto original?"
  putStrLn "¿De dónde viene el texto?"
