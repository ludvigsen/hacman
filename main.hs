import Data.List.Utils(split)
import Data.String.Utils(strip)
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
splitAttribute xs = map (map strip) (map (split " : ") (lines xs))

getListThatContains :: String -> [[String]] -> String
getListThatContains x []         = ""
getListThatContains x ((a:k):xs) | x == a = last k
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

printPackage p = "Name:            " ++ name p ++ "\n" ++
                 "Version:         " ++ version p ++ "\n" ++
                 "URL:             " ++ url p ++ "\n" ++
                 "Repository:      " ++ repository p ++ "\n" ++
                 "Licenses:        " ++ licenses p ++ "\n" ++
                 "Groups:          " ++ groups p ++ "\n" ++
                 "Provides:        " ++ provides p ++ "\n" ++
                 "Depends On:      " ++ dependsOn p ++ "\n" ++
                 "Optional Deps:   " ++ optionalDeps p ++ "\n" ++
                 "Conflicts With:  " ++ conflictsWith p ++ "\n" ++
                 "Replaces:        " ++ replaces p ++ "\n" ++
                 "Download Size:   " ++ downloadSize p ++ "\n" ++
                 "Installed Size:  " ++ installedSize p ++ "\n" ++
                 "Packager:        " ++ packager p ++ "\n" ++
                 "Architecture:    " ++ architecture p ++ "\n" ++
                 "Build Date:      " ++ buildDate p ++ "\n" ++
                 "MD5 Sum:         " ++ md5sum p ++ "\n" ++
                 "Description:     " ++ description p ++ "\n"

printPackages :: [Package] -> String                 
printPackages []     = ""                 
printPackages (p:xs) = printPackage p ++ "\n\n" ++ printPackages xs
                 
getPackages = do
    (pid, output) <- pipeFrom "pacman" ["-Si"]
--    packages <- split "\n\n" $ output
    putStrLn (printPackages (extractPackages (split "\n\n" output)))
--    return $ 

searchPackage x = do
              packages <- system ("pacman -Qs " ++ x)
              return packages
              

main = do
     system "pacman -Syu"