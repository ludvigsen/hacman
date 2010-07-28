{-# OPTIONS -fglasgow-exts #-}
--    Copyright (C) 2010 Eivind Jahren and Marius Ludvigsen    
--    This file is part of Hacman.
--
--    Hacman is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Hacman is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Hacman.  If not, see <http://www.gnu.org/licenses/>.

-- | The pacman module gives the Haskell bindings to pacman.
-- Here package attribute can be a package name, version
-- downloadSize, url and so on. A package is the collection of such
-- attributes. Pacman(packetManager) means pacman the software.
module Pacman
	(Package(..),
	 PacError(..),
	 pacman,
	 si,
	 qu,
	 qs)
	where
import System.Cmd.Utils
import Char
import Text.ParserCombinators.Parsec

import System.IO
import System.Exit
import Text.Printf
import Data.List

import Control.Monad.Error
import Control.Exception hiding (try)

-- | Package contains the information given by
--  pacman(packetManager) of a package. That is a list of attributes of
-- name, version, licenses, dependecies and so on... 
-- if an attribute is not found or listed with none by
-- pacman it's set to Nothing, otherwise Just a, where a 
-- is the attribute.
data Package =
     Package {name :: Maybe String,
              version :: Maybe String,
              url :: Maybe  String,
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

-- | given a list of pair of attribute name and
-- attruibute value (as a String), getPkg makes a package with those
-- attributes.
getPkg :: [([Char], [[Char]])] -> Package
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

-- | A parsec parser that parses a list of packages
-- as given by for instance
-- "pacman -Si" and "pacman -Qi"
parsePackages :: Parser [Package]
parsePackages = do xss <- parsePackageList
    	 	   return (map getPkg xss)

-- | A parsec parser that parses one package as given by pacman(packetManager).
parsePackageList :: Parser [[(String,[String])]]
parsePackageList =
	do xss <- parseLines `endBy` (char '\n')
	   return xss
	where
	parseLines = parseLine `endBy` (char '\n')

-- | A parsec parser that parses one line in one package given by pacman(packetManager).
--   Will also parse the nextLines that are indented.
parseLine :: Parser (String,[String])
parseLine = do 
		y <- sepEndBy (many1 $ noneOf " :\n") (char ' ')
                many (char ' ')
		string ": "
		spaces
                x <- sepBy (sepEndBy (many1 $ noneOf " \n") (char ' ')) (char ' ')
		z <- many $
			do
			try (string "\n ")
			spaces
			sepBy (sepEndBy (many1 $ noneOf " \n") (char ' ')) (char ' ')	
                return (intercalate " " y, map (intercalate " ") (x ++ (concat z)))
		     

-- | PacError is the errorType of Pacman as a MonadError.
data PacError = FatalError | NotRootError | ParseError String | OtherError String
	deriving Show

instance Error PacError where
	strMsg s = OtherError s

-- | Pacman is the MonadTransformer that pacman(packetManager) is wrapped in.
-- Its a MonadTransformer of ErrorT PacError IO a.
newtype Pacman a = Pacman {runPac :: ErrorT PacError IO a}
	deriving (Functor, Monad, MonadIO, MonadError PacError)

-- | pacman accesses the underlying IO monad of Pacman.
pacman :: Pacman a -> IO (Either PacError a)
pacman =  runErrorT . runPac

-- | parsePac lifts a parsec Parser to the Pacman monad. 
parsePac :: Show a => Parser a -> String -> Pacman a
parsePac p input = case (parse p "" input) of
              Left err -> throwError (ParseError ((show err) ++ "\ngiven Input: \n" ++ input))
              Right x -> return x

qu :: Pacman [Package]
qu = do
	(_, output) <- liftIO (pipeFrom "pacman" ["-Qu"])
	if output == "" 
	 then return []
	 else si (map (takeWhile (/= ' ')) $ lines output)

-- | does "pacman -Si" and returns the list of packages.
si :: [String] -> Pacman [Package]
si xs = do
       (_, output) <- liftIO (pipeFrom "pacman" ("-Si":xs))
       parsePac parsePackages output

-- | does "pacman -Qs" and returns the list of packages. That is returned
-- by that command.
-- Example: pacman qs ["pacman"] would return the pacman package that is
-- currently installed.
qs :: [String] -- ^ A list of arguments to qs
      -> Pacman [Package]
qs xs =	do
	(pid,output) <- liftIO (pipeFrom "pacman" ("-Qs":xs))
	(pid,pkgOut) <- liftIO (pipeFrom "pacman" ("-Qi":[tail $ takeWhile (/= ' ') (dropWhile (/= '/') output)]))
	parsePac parsePackages (pkgOut)
