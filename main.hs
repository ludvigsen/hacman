import Data.List.Utils(split)
import System.Cmd.Utils
import System

data Package =
     Package { name :: String, -- name of package
               version :: String, -- version of package
               url :: String, -- url to developers page
               repository :: String, -- which repo it exists in
               licenses :: String, -- which licens the package is
               groups :: String,
               provides :: String,
               dependsOn :: String,
               optionalDeps :: String,
               conflictsWith :: String,
               replaces :: String,
               downloadSize :: String,
               installedSize :: String,
               packager :: String,
               architecture :: String,
               buildDate :: String,
               md5sum :: String,
               description :: String
              }
              deriving (Eq, Show, Read)


-- Theese functions is supposed to return a attribute.
-- Needs some work..
splitAttribute xs = (map words (lines (filter (\x-> x /= ':') xs)))

getListThatContains :: String -> [[String]] -> String
getListThatContains x []         = ""
getListThatContains x ((a:k):xs) | x == a = head k
                                 | otherwise = getListThatContains x xs

getAttribute x xs = getListThatContains x (splitAttribute xs)
-----

extractPackage :: [Char] -> Package
extractPackage xs = Package { name          = getAttribute "Name" xs,
                              version       = getAttribute "Version" xs,
                              url           = getAttribute "URL" xs,
                              repository    = getAttribute "Repository" xs,
                              licenses      = getAttribute "Licenses" xs,
                              groups        = getAttribute "Groups" xs,
                              provides      = getAttribute "Provides" xs,
                              dependsOn     = getAttribute "Depends On" xs,
                              optionalDeps  = getAttribute "Optional Deps" xs,
                              conflictsWith = getAttribute "Conflicts With" xs,
                              replaces      = getAttribute "Replaces" xs,
                              downloadSize  = getAttribute "Download Size" xs,
                              installedSize = getAttribute "Installed Size" xs,
                              packager      = getAttribute "Packager" xs,
                              architecture  = getAttribute "Architecture" xs,
                              buildDate     = getAttribute "Build Date" xs,
                              md5sum        = getAttribute "MD5 Sum" xs,
                              description   = getAttribute "Description" xs
                            }

extractPackages :: [[Char]] -> [Package]
extractPackages []     = []
extractPackages (x:xs) = (extractPackage x):(extractPackages xs)

getPackages = do
    (pid, output) <- pipeFrom "pacman" ["-Si"]
--    packages <- split "\n\n" $ output
    return $ extractPackages (split "\n\n" output)

searchPackage x = do
              packages <- system ("pacman -Qs " ++ x)
              return packages
              

main = do
     system "pacman -Syu"