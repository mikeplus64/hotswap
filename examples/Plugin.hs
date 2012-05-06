module Plugin where
inputHandler :: String -> String
inputHandler s = "stay on target: " ++ foobarinate s ++ "."

foobarinate :: String -> String
foobarinate = reverse . show
