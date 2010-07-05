{-# OPTIONS -fglasgow-exts #-}
module Pacman where
import System.Cmd.Utils
import Char
import Text.ParserCombinators.Parsec

import System.IO
import System.Exit
import Text.Printf

import Control.Monad.Error
import Control.Exception hiding (try)

data Package =
     Package {name :: Maybe String,
              version :: Maybe String,
              url ::Maybe  String,
              repository :: Maybe String,
              licenses :: Maybe [String],
              groups :: Maybe [String],
              provides ::Maybe [String],
              dependsOn ::Maybe [String],
              optionalDeps ::Maybe [String],
              conflictsWith ::Maybe [String],
              replaces ::Maybe [String],
              downloadSize ::Maybe Int,
              installedSize ::Maybe Int,
              packager ::Maybe String,
              architecture ::Maybe String,
              buildDate ::Maybe String,
              md5sum ::Maybe String,
              description ::Maybe String
              }
	deriving (Show)

parsePackages = do xss <- parsePackageList
    	 	   return (map getPkg xss)

parsePackageList :: Parser [[(String,[String])]]
parsePackageList =
	do xs <- parseLines
	   if xs == []
	    then return []
            else do {char '\n';xss <- parsePackageList; return (xs:xss)} <|> return [xs]

parseLines = do x <- parseLine <|> return ("",[""])
		if x == ("",[""]) 
                 then return [] 
                 else do {char '\n';ys <- parseLines; return (x:ys)} <|> return [x]

getPkg xs = Package {name = getHead "Name",
              version = getHead "Version",
              url = getHead "Url",
              repository = getHead "Repository",
              licenses = get "Licenses",
              groups = get "Groups",
              provides = get "Provides",
              dependsOn = get "Depends On",
              optionalDeps = get "Optional Deps",
              conflictsWith = get "Conflicts With",
              replaces = get "Replaces",
              downloadSize = getInt "Download Size",
              installedSize = getInt "Installed Size",
              packager = getHead "Packager",
              architecture = getHead "Architecture",
              buildDate = getHead "Build Date",
              md5sum = getHead "MD5 Sum",
              description = getHead "Description"
              }
	where
	getHead str = (fmap head) $ get str
	get str = handleNone $ (lookup str) xs
	getInt str = (fmap (read . (takeWhile isDigit))) $ getHead str
	handleNone (Just ["None"]) = Nothing
	handleNone Nothing = Nothing
	handleNone a = a 

parseLine :: Parser (String,[String])
parseLine = do 
		y <- try (do {x <- many1 (noneOf " \n"); 
			  space;
                          y <- many1 letter;
		       	  return (x ++ (' ':y))}) <|> many1 (noneOf " \n")
                many space
		string ": "
                x <- sepBy (many (noneOf "\n")) (string "  ")
                return (y, x)
		     
data PacError = FatalError | NotRootError | ParseError | OtherError String
	deriving Show

instance MonadError PacError Pacman where
	throwError = error . ("Pacman Error:" ++) . show

instance Error PacError where
	strMsg s = OtherError s

newtype Pacman a = Pacman {runPac :: ErrorT PacError IO a}
	deriving (Functor, Monad, MonadIO)
pacman =  runErrorT . runPac

parsePac :: Show a => Parser a -> String -> Pacman a
parsePac p input = case (parse p "" input) of
              Left err -> throwError ParseError
              Right x -> return x

si :: [String] -> Pacman [Package]
si xs = do
       (pid, output) <- liftIO (pipeFrom "pacman" ("-Si":xs))
       packages <- parsePac parsePackages output
       return packages
qs :: [String] -> Pacman [Package]
qs xs =	do
	(pid,output) <- liftIO (pipeFrom "pacman" ("-Qs":xs))
	(pid,pkgOut) <- liftIO (pipeFrom "pacman" ("-Qi":[tail $ takeWhile (/= ' ') (dropWhile (/= '/') output)]))
	x <- parsePac parsePackages pkgOut
	return x

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
              Left err -> do putStr "parse error at "
                             print err 
              Right x -> print x

