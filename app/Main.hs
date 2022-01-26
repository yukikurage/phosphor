module Main where

f x = case x of
  Nothing -> Nothing
  Just y  -> Just y

main :: IO ()
main = do
  pure ()
