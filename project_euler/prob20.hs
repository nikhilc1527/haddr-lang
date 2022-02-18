import Data.Char

fac 0 = 1
fac x = x * (fac $ x - 1)

main = putStrLn $ show $ sum $ map (\t -> t - 48) $ map ord $ show $ fac 100
