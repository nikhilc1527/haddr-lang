import Data.Char

power x 0 = 1
power x a = x * (power x $ a - 1)

main = putStrLn $ show $ sum $ map (\t -> t - 48) $ map ord $ show $ power 2 1000
