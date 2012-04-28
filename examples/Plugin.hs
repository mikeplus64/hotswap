module Plugin where
inputHandler :: String -> String
inputHandler s = "DANGET BOBBY: " ++ foobarinate s

foobarinate :: String -> String
foobarinate = reverse . show
