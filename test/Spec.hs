import Soothsayer ((***))

main :: IO ()
main = putStrLn $ "Hello {0}" *** ["world"]
