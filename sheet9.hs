sayHello :: String -> String
sayHello name = "Hi there " ++ name

greet :: IO ()
greet = getLine >>= (putStrLn . sayHello)

greet' :: IO ()
greet' = putStrLn "What is your name?" >> greet

getInt :: IO Int
getInt = getLine >>= (return . read)