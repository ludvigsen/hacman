{-# OPTIONS -fglasgow-exts #-}
module Pacman where
import System.Cmd.Utils
import Char
import Text.ParserCombinators.Parsec

import System.IO
import System.Exit
import Text.Printf
import Data.List

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
	do xss <- parseLines `endBy` (char '\n')
	   return xss

parseLines = do
		xs <-  parseLine `endBy` (char '\n')
		return xs

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
		y <- sepEndBy (many1 $ noneOf " :\n") (char ' ')
                many (char ' ')
		string ": "
                x <- sepBy (sepEndBy (many1 $ noneOf " \n") (char ' ')) (char ' ')
                return (intercalate " " y, map (intercalate " ") x)
		     
data PacError = FatalError | NotRootError | ParseError | OtherError String
	deriving Show

instance Error PacError where
	strMsg s = OtherError s

newtype Pacman a = Pacman {runPac :: ErrorT PacError IO a}
	deriving (Functor, Monad, MonadIO, MonadError PacError)
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
