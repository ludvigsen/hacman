module Main where
import Pacman 
import Control.Monad.Error
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Glib.Signals (on)
import Data.List
import Data.Char ( toLower )
import Data.Maybe

getPackageValue getter package = fromMaybe "" (getter package)

getPackageValues :: (Package -> (Maybe [String])) -> Package -> String
getPackageValues getter package = case (getter package) of
				       Nothing -> ""
				       Just a  -> concat (intersperse " " a)

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

makeCell model view (name, getter) = 
	do
        col <- treeViewColumnNew
	treeViewColumnSetTitle col name
 	renderer <- cellRendererTextNew
	cellLayoutPackStart col renderer True
	cellLayoutSetAttributes col renderer model $ \row -> [ cellText := getter row ]
	treeViewAppendColumn view col

columnList =
	(map (\ (a,getter) -> (a,getPackageValue getter)) 
	[("Name",name),
	("Version", version),
	("URL", url),
	("Repository", repository)]) 
	++	
	[("Download Size", show . (fromMaybe 0) . downloadSize),
        ("Installed Size",show . (fromMaybe 0) . installedSize)]	
	++
	(map (\ (a,getter) -> (a,getPackageValues getter))
	[("Licenses", licenses),
	("Groups", groups),
	("Provides", provides),
	("Depends On", dependsOn),
	("Optional Deps", optionalDeps),
	("Conflicts With", conflictsWith),
	("Replaces", replaces)])

main = do
        -- GTK stuff
        initGUI
        Just xml <- xmlNew "hacman.glade"
        win <- xmlGetWidget xml castToWindow "window1"
        sw <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
        qb <- xmlGetWidget xml castToImageMenuItem "menuQuit"
        
        onDestroy win mainQuit -- Make program exit normally

        -- Exits program when user presses Quit button
        onActivateLeaf qb $ do
                  widgetDestroy win

        --(Right pacmanPkg) <- pacman qu
        (Right pacmanPkg) <- pacman (si [])

        model <- listStoreNew pacmanPkg
        view <- treeViewNewWithModel model

	sequence (map (makeCell model view) columnList)

--        treeViewSetHeadersVisible view True
        containerAdd sw view
        widgetShowAll win
        mainGUI        
