import Pacman 
import Control.Monad.Error

printPackage p = "Name:            " ++ (show $ name p) ++ "\n" ++
                 "Version:         " ++ (show $ version p) ++ "\n" ++
                 "URL:             " ++ (show $ url p) ++ "\n" ++
                 "Repository:      " ++ (show $ repository p) ++ "\n" ++
                 "Licenses:        " ++ (show $ licenses p) ++ "\n" ++
                 "Groups:          " ++ (show $ groups p) ++ "\n" ++
                 "Provides:        " ++ (show $ provides p) ++ "\n" ++
                 "Depends On:      " ++ (show $ dependsOn p) ++ "\n" ++
                 "Optional Deps:   " ++ (show $ optionalDeps p) ++ "\n" ++
                 "Conflicts With:  " ++ (show $ conflictsWith p) ++ "\n" ++
                 "Replaces:        " ++ (show $ replaces p) ++ "\n" ++
                 "Download Size:   " ++ (show $ downloadSize p) ++ "\n" ++
                 "Installed Size:  " ++ (show $ installedSize p) ++ "\n" ++
                 "Packager:        " ++ (show $ packager p) ++ "\n" ++
                 "Architecture:    " ++ (show $ architecture p) ++ "\n" ++
                 "Build Date:      " ++ (show $ buildDate p) ++ "\n" ++
                 "MD5 Sum:         " ++ (show $ md5sum p) ++ "\n" ++
                 "Description:     " ++ (show $ description p) ++ "\n"

main = do
	(Right pacmanPkg) <- (pacman (si ["pacman"])) `catchError` (\e -> error "fatalERROR§!!!!§!§")
	putStr $ printPackage $ head pacmanPkg
	print (name $ head pacmanPkg)
	print (licenses $ head pacmanPkg)
	(Right packy) <- (pacman (qs ["pacman-mirrorlist"])) `catchError` (\e -> error "MoaFATAL!!!!")
	print (licenses $ head packy)

