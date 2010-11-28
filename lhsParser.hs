import Text.ParserCombinators.Parsec            
import Control.Monad (liftM)
import System(getArgs)

{-| 
    Makes the content of a lhs file to be 
    seen as a set of paragraphs separated 
    by a return character.
 -}
input :: GenParser Char st [[[Char]]]
input = sepBy paragraph (string "\n\n") 

{-| 
    Makes the content of each paragraph be 
    parsed as a set of lines separated by a
    return character.
 -}
paragraph :: GenParser Char st [[Char]]
paragraph = sepBy content (string "\n")
            
{-| 
    Each paragraph line can be parsed 
    as: a line of code, main header, secondary header, 
    or regular text. Note the use of try. This is useful 
    to treat blank spaces. Check out functions: @mHeader@,
    @codeLine@, @sHeader@, @reg_tex@.
  -}
content :: GenParser Char st [Char]
content = try (codeLine)
          <|> try (mHeader)
          <|> try (sHeader)
          <|> try (reg_tex)

{-| 
  It is used to parse regular text.
 -}
reg_tex :: GenParser Char st String
reg_tex = do
  many $ char ' '
  cLine <- liftM (sOut.concatMap escape) (many $ noneOf "\n\r")
  return cLine
  
{-| 
    Reads a set of blank characters until it finds the *
    character, in which case it takes off the left blank spaces
    and returns a line with the HTML main header tags.
 -}
mHeader :: GenParser Char st [Char]
mHeader = do
  many $ char ' '
  char '*'
  cLine <- liftM (sOut.concatMap escape) $ many $ noneOf "\n\r"
  return ("<h1>" ++ cLine ++ "</h1>")
  
{-| 
    It reads a set of blank characters until it finds the # character,
    in which case it takes off the left blank spaces and returns a line 
    containing the HTML secondary header tags.
 -}
sHeader :: GenParser Char st [Char]       
sHeader = do
  many $ char ' '
  char '#' 
  cLine <- liftM (sOut.concatMap escape) $ many $ noneOf "\n\r"
  return ("<h2>" ++ cLine ++ "</h2>")
  
{-| 
    It reads a set of blank character until it finds a 
    character followed by a blank space, in which case it takes off 
    the blank spaces and returns a line with the HTML code tags.
 -}
codeLine :: GenParser Char st [Char]
codeLine = do 
  many $ char ' '
  char '>'
  char ' '
  cLine <- liftM (sOut.concatMap escape) $ many $ noneOf "\n\r"
  return ("<code>" ++ cLine ++ "</code>")
  
{-| 
  Adds up HTML paragraph tags to each of the found paragraphs.
 -}
toParagraphs :: [[Char]] -> [Char] -> [Char] -> [Char]
toParagraphs (x:xs) acc res = 
    if x == ""
       then 
           let b = res ++ "<p>" ++ acc ++ "</p>"
               in toParagraphs xs [] b
    else 
        toParagraphs xs (acc ++ (x ++ "<br>")) res
toParagraphs [] _ res = res
                
{-| 
   By composing unwords and words we can get rid of 
   unnecessary blank spaces on the given input.
 -}
sOut :: String -> String
sOut = unwords . words
       
{-|
    Escapes special characters.
 -}
escape :: Char -> [Char]
escape x =  
    case x of 
      '&' -> "&#38"
      '<' -> "&#60"
      '>' -> "&#62"
      _   -> [x]
      
{-| 
    Main function that parses the document.
 -}
parseLHS :: SourceName -> [Char] -> Either ParseError [[[Char]]]
parseLHS = parse input
           
{-| 
    This function will go through the files and parse each 
    one of them generating the HTML files. 
 -}
go :: [FilePath] -> IO ()
go (x:[]) = do 
  parseIt x
go (x:xs) = do
  do parseIt x 
     go xs

{-| 
    Function that applies parsing and converts the output 
    in an HTML file. 
 -}
parseIt :: FilePath -> IO ()
parseIt x = do
  content <- readFile x 
  case parseLHS "error" content of
    Left c -> do putStrLn "Error: "
                 print c
    Right (r:rs) ->
            writeFile (x ++ ".html") (toParagraphs r [] [])
             
main = do 
 args <- getArgs
 go args
           
