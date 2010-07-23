module Main where
import Pacman 
import Control.Monad.Error
import Graphics.UI.Gtk
import System.Glib.Signals (on)
import Data.List ( isPrefixOf )
import Data.Char ( toLower )
import Data.Maybe

mergeStringList :: [String] -> String
mergeStringList (x:xs) = x ++ " " ++ mergeStringList xs
mergeStringList [] = []

safeFromJustString :: (Maybe String) -> String
safeFromJustString (Just x) = x
safeFromJustString Nothing = ""

safeFromJustStrings :: (Maybe [String]) -> [String]
safeFromJustStrings (Just x) = x
safeFromJustStrings  Nothing = [""]


getPackageValue getter package = safeFromJustString (getter package)

getPackageValues :: (Package -> (Maybe [String])) -> Package -> String
getPackageValues getter package | (safeFromJustStrings value) == [""] = ""
                                | otherwise = mergeStringList (safeFromJustStrings value)
                 where value = getter package

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
        -- GTK stuff
        initGUI
        win <- windowNew
        onDestroy win mainQuit -- Make program exit normally
        
        (Right pacmanPkg) <- pacman ((si ["pacman"]) `catchError` (\(ParseError e) -> error e))
        
        model <- listStoreNew pacmanPkg

        view <- treeViewNewWithModel model
        
        treeViewSetHeadersVisible view True
        
        col1 <- treeViewColumnNew
        col2 <- treeViewColumnNew
        col3 <- treeViewColumnNew
        col4 <- treeViewColumnNew
        col5 <- treeViewColumnNew
        col6 <- treeViewColumnNew
        col7 <- treeViewColumnNew
        col8 <- treeViewColumnNew
        col9 <- treeViewColumnNew
        col10 <- treeViewColumnNew
        col11 <- treeViewColumnNew
        
        treeViewColumnSetTitle col1  "Name"
        treeViewColumnSetTitle col2  "Version"
        treeViewColumnSetTitle col3  "URL"
        treeViewColumnSetTitle col4  "Repository"
        treeViewColumnSetTitle col5  "Licenses"
        treeViewColumnSetTitle col6  "Groups"
        treeViewColumnSetTitle col7  "Provides"
        treeViewColumnSetTitle col8  "Depends On"
        treeViewColumnSetTitle col9  "Optional Deps"
        treeViewColumnSetTitle col10 "Conflicts With"
        treeViewColumnSetTitle col11 "Replaces"
        
        renderer1 <- cellRendererTextNew
        renderer2 <- cellRendererTextNew
        renderer3 <- cellRendererTextNew
        renderer4 <- cellRendererTextNew
        renderer5 <- cellRendererTextNew
        renderer6 <- cellRendererTextNew
        renderer7 <- cellRendererTextNew
        renderer8 <- cellRendererTextNew
        renderer9 <- cellRendererTextNew
        renderer10 <- cellRendererTextNew
        renderer11 <- cellRendererTextNew
        
        cellLayoutPackStart col1 renderer1 True
        cellLayoutPackStart col2 renderer2 True
        cellLayoutPackStart col3 renderer3 True
        cellLayoutPackStart col4 renderer4 True
        cellLayoutPackStart col5 renderer5 True
        cellLayoutPackStart col6 renderer6 True
        cellLayoutPackStart col7 renderer7 True
        cellLayoutPackStart col8 renderer8 True
        cellLayoutPackStart col9 renderer9 True
        cellLayoutPackStart col10 renderer10 True
        cellLayoutPackStart col11 renderer11 True
        
        cellLayoutSetAttributes col1 renderer1 model $ \row -> [ cellText := getPackageValue name row ]
        cellLayoutSetAttributes col2 renderer2 model $ \row -> [ cellText := getPackageValue version row ]
        cellLayoutSetAttributes col3 renderer3 model $ \row -> [ cellText := getPackageValue url row ]
        cellLayoutSetAttributes col4 renderer4 model $ \row -> [ cellText := getPackageValue repository row ]
        cellLayoutSetAttributes col5 renderer5 model $ \row -> [ cellText := getPackageValues licenses row ]
        cellLayoutSetAttributes col6 renderer6 model $ \row -> [ cellText := getPackageValues groups row ]
        cellLayoutSetAttributes col7 renderer7 model $ \row -> [ cellText := getPackageValues provides row ]
        cellLayoutSetAttributes col8 renderer8 model $ \row -> [ cellText := getPackageValues dependsOn row ]
        cellLayoutSetAttributes col9 renderer9 model $ \row -> [ cellText := getPackageValues optionalDeps row ]
        cellLayoutSetAttributes col10 renderer10 model $ \row -> [ cellText := getPackageValues conflictsWith row ]
        cellLayoutSetAttributes col11 renderer11 model $ \row -> [ cellText := getPackageValues replaces row ]
        
        treeViewAppendColumn view col1
        treeViewAppendColumn view col2
        treeViewAppendColumn view col3
        treeViewAppendColumn view col4
        treeViewAppendColumn view col5
        treeViewAppendColumn view col6
        treeViewAppendColumn view col7
        treeViewAppendColumn view col8
        treeViewAppendColumn view col9
        treeViewAppendColumn view col10
        treeViewAppendColumn view col11
        
        containerAdd win view
        
        widgetShowAll win

        mainGUI 
        
        
--	putStr $ printPackage $ head pacmanPkg
--	print (name $ head pacmanPkg)
--	print (licenses $ head pacmanPkg)
--        print (show $ pacmanPkg)
	--(Right packy) <- pacman (qs ["pacman-mirrorlist"] `catchError` (\(ParseError e) -> error e))
--	print (licenses $ head packy)
        


