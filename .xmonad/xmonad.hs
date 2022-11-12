{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hPutStrLn )
import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Control.Monad
import qualified Data.Map                      as M
import           Text.Printf                    ( printf )
import           GHC.IO.Handle                  ( Handle )
import           Graphics.X11.Xrandr
import           System.Directory               ( setCurrentDirectory
                                                , getHomeDirectory
                                                )

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
                                                ( (|||)
                                                , JumpToLayout(..)
                                                )

import           XMonad.StackSet         hiding ( workspaces )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.Run                ( safeSpawn
                                                , unsafeSpawn
                                                , runInTerm
                                                , spawnPipe
                                                )
import           XMonad.Config.Gnome            ( gnomeConfig
                                                , gnomeRun
                                                )
import           XMonad.Config.Xfce             ( xfceConfig )
-- helper functions for parsing keymaps
import           XMonad.Util.EZConfig           ( mkKeymap
                                                , additionalKeysP
                                                , checkKeymap
                                                , mkNamedKeymap
                                                )
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Hooks.StatusBar.PP      (filterOutWsPP)

import           XMonad.Prompt
import           XMonad.Prompt.Workspace        ( workspacePrompt )


import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers     ( isDialog
                                                , doFullFloat
                                                , doCenterFloat
                                                , transience'
                                                )
import           XMonad.Hooks.ManageDocks       ( AvoidStruts
                                                , avoidStruts
                                                , ToggleStruts(..)
                                                , manageDocks
                                                )
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.SetWMName         ( setWMName ) -- workaround for Java Swing/GUI apps not working

import           XMonad.Layout.IndependentScreens
                                                ( countScreens )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenFull
                                                , fullscreenSupport
                                                )
import           XMonad.Layout.Grid             ( Grid(..) )
import           XMonad.Layout.TwoPane          ( TwoPane(..) )
import           XMonad.Layout.Tabbed           ( simpleTabbed )
import           XMonad.Layout.Renamed          ( renamed
                                                , Rename(Replace)
                                                )

import           XMonad.Actions.DynamicWorkspaceGroups
                                                ( addRawWSGroup
                                                , addWSGroup
                                                , promptWSGroupView
                                                , viewWSGroup
                                                )
import           XMonad.Actions.PhysicalScreens ( PhysicalScreen(..)
                                                , getScreen
                                                , horizontalScreenOrderer
                                                , viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.WindowGo        ( ifWindows
                                                , ifWindow
                                                )

-- This visual system consists of the following:
-- xmonad = a basic dynamic tiling windonw manager
-- xmobar = a simple statusbar on top of xmonad
-- dmenu = a simple program launcher that is started with win+p
-- stalonetray = an area next to xmobar where running apps like dropbox show clickable icons.

main = do
  screenCount <- countScreens :: IO Int
  let overlayMyBaseSettings' = overlayMyBaseSettings screenCount
  let overlayAppGroups'      = overlayAppGroups screenCount myGroups
  let myXConfig =
        overlayAppGroups'
          . overlayKeys
          . overlayFullscreenSupport
          . overlayMyBaseSettings'
          $ xfceConfig
  myXConfig <- overlayXmobar myXConfig
  xmonad myXConfig


overlayFullscreenSupport baseConfig = fullscreenSupport baseConfig

overlayKeys baseConfig =
  addDescrKeys ((ctrlKey .|. winKey, xK_h), xMessage) myKeys baseConfig

overlayMyBaseSettings screenCount baseConfig = baseConfig
  { normalBorderColor  = myBlack
  , focusedBorderColor = myBlue
  , focusFollowsMouse  = False
  , layoutHook         = myLayouts screenCount
        -- Action to run when a new window is opened, <+> compoeses right to left
  , manageHook         = manageHook baseConfig <+> myManageHook
  , modMask            = winKey
  , terminal           = myTerminal
  , borderWidth        = 3
  , startupHook        = startupHook baseConfig <+> myStartupHook
  }



overlayXmobar
  :: LayoutClass l Window
  => XConfig l
  -> IO (XConfig (ModifiedLayout AvoidStruts l))
overlayXmobar baseConfig = multiStatusBar spawnXbar
                                          myXmobarPP
                                          toggleXmobarKey
                                          baseConfig

 where
  spawnXbar = printf "xmobar ~/.xmonad/xmobar.hs -x %s"
  toggleXmobarKey _baseConfig = (ctrlKey .|. winKey, xK_b)

-- | Modifies the given base configuration to launch the given status bar,
-- send status information to that bar, and allocate space on the screen edges
-- for the bar.
multiStatusBar
  :: LayoutClass l Window
  => (String -> String) -- ^ the command line to launch the status bar
  -> PP        -- ^ the pretty printing options
  -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ the desired key binding to toggle bar visibility
  -> XConfig l -- ^ the base config
  -> IO (XConfig (ModifiedLayout AvoidStruts l))
multiStatusBar barSpawnCmd prettyPrintConfig barToggleKey baseConfig = do
  screenCount <- countScreens
  let screenIds = map S [0 .. screenCount - 1]
  xmprocs <-
    mapM (\i -> spawnPipe . barSpawnCmd . show . unScreenId $ i) screenIds :: IO
      [Handle]
  return $ baseConfig
    { layoutHook = avoidStruts (layoutHook baseConfig)
    , logHook    = do
                     logHook baseConfig
                     let physIds = map P [0 .. screenCount - 1]
                     screenIds <- mapM phys2ScreenId physIds
                     let physIds' = backpermute screenIds physIds
                     let xmprocs' = zip physIds' xmprocs
                     mapM_
                       (\((P i), handle) -> dynamicLogWithPP $ prettyPrintConfig
                         { ppOutput = (hPutStrLn handle) . ((show i ++ " ") ++)
                         }
                       )
                       xmprocs'
    , manageHook = manageHook baseConfig <+> manageDocks
    , keys       = liftM2 M.union keys' (keys baseConfig)
    }
  where keys' = (`M.singleton` sendMessage ToggleStruts) . barToggleKey

backpermute :: Ord i => [i] -> [a] -> [a]
backpermute idxs list = map snd . sortOn fst $ zip idxs list

-- Define how xmonad-workspace-status is displayed.
-- Every bar has a textarea for displaying that.
myXmobarPP = filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
  { ppCurrent = xmobarColor myBlue "" . wrap "[" "]"  -- currently focused workspace
  , ppTitle   = xmobarColor myBlue "" . shorten 80  -- title of currently focused program
  , ppHidden  = xmobarColor myGray "" . wrap "(" ")" -- hidden workspaces but with windows
  }

myBlack = "#000000"
myBlue = "#0080FF"
myGray = "gray"

winKey = mod4Mask
ctrlKey = controlMask

myTerminal = "alacritty"

myXConfig = xfceConfig -- gnomeConfig

myStartupHook = do
  spawnOnce "/home/mahene/.xmonad/xmonad-start.sh"
  spawnOnce
    "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 20 --transparent true --tint 0x000000 &"
  -- workaround for Java Swing/GUI apps not working
  setWMName "LG3D"

--myStartupHook baseConfig = do
--  checkKeymap baseConfig (fileShortcuts ++ appShortcuts ++ mediaShortcuts ++ xmonadShortcuts baseConfig)


-- Action to run when a new window is opened.
-- Use "xprop" terminal command to find out properties of running programs.
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
myManageHook = composeAll
  [ className =? "stalonetray" --> doIgnore
  -- , title =? "Microsoft Teams"  --> doIgnore
  -- , resource =? "microsoft teams - preview"  --> doIgnore
  , isDialog --> doCenterFloat
  , namedScratchpadManageHook myScratchPads
  , groupManageHook' myGroups
  -- move transient windows like dialogs/alerts on top of their parents
  , transience'
  ]

myLayouts screenCount =
  rename "Single" Full
    ||| rename "Fullscreen" (noBorders Full) -- remove borders with single screen
  -- if isMultiScreen then (rename "Fullscreen" Full) else (rename "FullScreen" (noBorders Full)) -- remove borders with single screen
  -- rename "Fullscreen" (if isMultiScreen then Full else noBorders Full) -- remove borders with single screen
    ||| rename
          "Master"
          (Tall oneMasterWindow incStepSizePercent masterColumnSizePercent)
    ||| TwoPane incStepSizePercent masterColumnSizePercent
    ||| Grid -- all windows divided evenly
 where -- default tiling algorithm partitions the screen into two panes
  oneMasterWindow         = 1
  incStepSizePercent      = 10 / 100
  masterColumnSizePercent = 70 / 100
  rename name = renamed [Replace name]
  isMultiScreen = screenCount > 1

-- Define additional keymappings in compact emacs-string-style:
-- M- mod/win
-- C- Ctrl
-- S- Shift
-- M1-M5 for mod1-mod5 (find out which is which with "xmodmap")
myKeys baseConfig = concatMap
  ($ baseConfig)
  [ fileShortcuts
  , appShortcuts
  , scratchpadShortcuts
  , monitorShortcuts
  , mediaShortcuts
  , xmonadShortcuts
  ]

fileShortcuts baseConfig = [subtitle "Files"] ++ mkNamedKeymap
  baseConfig
  [ ("M-d " ++ key, spawn' command)
  | (key, command) <-
    [ ("t", "freeplane /home/mahene/Documents/todo.mm")
    , ("i", "freeplane /home/mahene/Documents/ideas.mm")
    , ("s", "freeplane /home/mahene/Documents/songs.mm")
    , ("d", "nautilus ~/Downloads/")
    , ("h", "nautilus ~/")
    , ("v", "nautilus ~/Videos/")
    , ("f", "nautilus ~/Photos/")
    , ("p", "nautilus ~/Documents/papers/")
    , ("m", "nautilus ~/Music/meditations")
    , ( "l"
      , "nautilus ~/Documents/read-lookup"
      ) -- lookup
    , ("o", "ranger") -- files
    ]
  ]

appShortcuts baseConfig = [subtitle "Apps"] ++ mkNamedKeymap
  baseConfig
  [ ("M-a " ++ key, addName description $ spawn command)
  | (key, description, command) <-
    [("S-s", "", "/opt/tor-browser_en-US/start-tor-browser")]
  ]

monitorShortcuts baseConfig = [subtitle "Monitor output"] ++ mkNamedKeymap
  baseConfig
  [ ("M-M1-m " ++ key, addName description $ spawn command)
  | (key, description, command) <-
    [ ("n", "Only on notebook display", notebookOnly)
    , ("a", "On all screens"          , all)
    ]
  ]
 where
  notebookDisplay   = "eDP-1" -- notebook monitor
  externDisplayMid  = "HDMI-1" -- external monitor
  externDisplayLeft = "DP-2" -- external monitor
  auto              = "auto" -- turn dipslay on and select highest resolution automatically
  off               = "off" -- turn Display off
  --xrandrTemplate ="xrandr --output %s --%s --primary --output %s --%s --left-of %s --output %s --%s --right-of %s --mode 1440x900"
  xrandrTemplate
    = "xrandr --output %s --%s --primary --output %s --%s --left-of %s --output %s --%s --right-of %s"
  xrandr (left, modeLeft, mid, modeMid, right, modeRight) =
    printf xrandrTemplate mid modeMid left modeLeft mid right modeRight mid
  notebookOnly = xrandr
    (externDisplayLeft, off, notebookDisplay, auto, externDisplayMid, off)
  all = xrandr
    (externDisplayLeft, auto, externDisplayMid, auto, notebookDisplay, auto)

mediaShortcuts baseConfig = [subtitle "Media"] ++ mkNamedKeymap
  baseConfig
  [ (key, addName description $ spawn command)
  | (key, description, command) <-
    [ ("M-[" , "Emulate previous song key", "xdotool key XF86AudioPrev")
    , ("M-]" , "Emulate next song key"    , "xdotool key XF86AudioNext")
    , ("M-\\", "Emulate Play/Pause key"   , "xdotool key XF86AudioPlay")
    , ( "M-u"
      , "Settings Ubuntu"
      , "unity-control-center"
      )
-- Volume Control
    , ("<XF86AudioMute>", "Mute", "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ( "<XF86AudioLowerVolume>"
      , "Lower volume"
      , "pactl set-sink-volume @DEFAULT_SINK@ -10%"
      )
    , ( "<XF86AudioRaiseVolume>"
      , "Increase volume"
      , "pactl set-sink-volume @DEFAULT_SINK@ +10%"
      )
-- Brightness Control
--, ("<XF86MonBrightnessUp>", "Brightness up", "lux -a 10%")
--, ("<XF86MonBrightnessDown>", "Brightness up", "lux -s 10%")
    ]
  ]

firefoxCmd :: String -> String
firefoxCmd profileName = firefoxCmd' "Firefox" profileName

firefoxCmd' :: String -> String -> String
firefoxCmd' className profileName =
  printf "firefox --class %s -new-instance -p \"%s\"" className profileName

-- Xmonad extensions
xmonadShortcuts baseConfig = [subtitle "Xmonad extensions"] ++ mkNamedKeymap
  baseConfig
  (  [ ( "M-<Return>"
       , addName "Terminal" . spawn $ terminal baseConfig
       ) -- add new key
 -- , ("M-p", addName "Yeganesh App launcher" $ spawn "$(yeganesh -x)")
     , ("M-S-p", addName "Xfce App launcher" $ spawn "xfce4-appfinder")
     , ( "M-q"
       , addName "Close app/window" kill
       ) -- override close focused window instead of logout
     , ( "M-S-q"
       , noName $ return ()
       ) -- override
     , ( "M-S-m"
       , addName "Make non-master window a master" $ windows swapMaster
       )
    -- restart xmonad
     , ("M-<Escape> r"  , addName "Restart Xmonad" $ spawn restartXMonadCmd)
     , ("M-<Escape> S-r", addName "Reboot" $ spawn rebootCmd)
     , ("M-<Escape> l"  , addName "Lock screen" $ spawn sessionLockCmd)
     , ("M-<Escape> S-l", addName "Logout User Account" $ spawn logoutCmd)
     , ("M-<Escape> s"  , addName "Shutdown" $ spawn shutdownCmd)
     , ("M-S-s"         , addName "Screenshot" $ spawn "xfce4-screenshoter")
     , ( "M-<Space> s"
       , addName "Single app Layout" . sendMessage $ JumpToLayout "Single"
       )
     , ( "M-<Space> f"
       , addName "Fullscreen Layout" . sendMessage $ JumpToLayout "Fullscreen"
       )
     , ( "M-<Space> m"
       , addName "Master Pane Layout" . sendMessage $ JumpToLayout "Master"
       )
     , ( "M-<Space> g"
       , addName "Grid Layout" . sendMessage $ JumpToLayout "Grid"
       )
  -- Layouts
     , ( "M-g"
       , addName "Go to workspace by name"
         $ workspacePrompt def (windows . lazyView)
       )
     ]
  ++
    -- Replacing greedyView with view
     [ ("M-" ++ otherModKey ++ [key], noName . windows $ action workspaceID)
     | (key, workspaceID) <- zip numberKeys workspaceIds
     , (otherModKey, action) <-
       [("", lazyView), ("C-", greedyView), ("S-", shift)]
     ]
  )
 where
  numberKeys       = "123456789"
  workspaceIds     = workspaces baseConfig
  restartXMonadCmd = "xmonad --recompile && xmonad --restart"
  -- Xfce specific
  rebootCmd        = "xfce4-session-logout --reboot"
  shutdownCmd      = "xfce4-session-logout --halt"
  hibernateCmd     = "xfce4-session-logout --hibernate"
  logoutCmd        = "xfce4-session-logout"
  sessionLockCmd   = "xflock4"

lazyView
  :: (Eq w, Eq sid)
  => w
  -> StackSet w lay win sid sd
  -> StackSet w lay win sid sd
lazyView workspaceId stackSet =
  if isVisible workspaceId stackSet then stackSet else view workspaceId stackSet

isVisible :: Eq w => w -> StackSet w lay win sid sd -> Bool
isVisible workspaceId stackSet =
  any ((workspaceId ==) . tag . workspace) (visible stackSet)


scratchpadShortcuts baseConfig = [subtitle "Scratchpads"] ++ mkNamedKeymap
  baseConfig
  (  [ ( "M1-b " ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <-
       [ ("w", "webScratch")
       , ( "s"
         , "separateScratch"
         )
-- , ("d", "devScratch")
       , ("m", "musicScratch")
       , ("t", "teamsScratch")
       ]
     ]
  ++ [ ( "M1-" ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <- -- "a" doesnt work
       [ ("w", "workScratch")
       , ("d", "devScratch")
       , ("p", "dbeaverScratch")
       ]
     ]
  ++ [ ( "M1-a " ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <-
       [ ("k", "keepassScratch")
       -- m doesn't work
       , ("a", "cmusScratch")
       , ("d", "discordScratch")
       , ("i", "ircScratch")
       , ("n", "matrixScratch")
       , ("l", "languageScratch")
       , ("e", "thunderbirdScratch")
       , ("j", "joplinScratch")
       , ("s", "signalScratch")
       , ("t", "todoScratch")
       ]
     ]
  )

-- Action to run when a new window is opened.
-- Use "xprop" terminal command to find out properties of running programs.
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
myScratchPads =
  [ NS "keepassScratch" "keepassxc" (resource =? "keepassxc") manageWindow
  , NS "cmusScratch"
       ("alacritty --class cmusplayer -e cmus")
       -- ("urxvt -name cmusplayer -e cmus")
       (resource =? "cmusplayer")
       manageWindow
  , NS "musicScratch"
       (firefoxCmd' "musicScratch" "music")
       (className =? "musicScratch")
       manageWindow
  , NS "webScratch"
       (firefoxCmd' "webScratch" "web")
       (className =? "webScratch")
       manageWindow
  , NS "workScratch"
       (firefoxCmd' "workScratch" "work")
       (className =? "workScratch")
       manageWindow
  , NS "devScratch"
       (firefoxCmd' "devScratch" "dev")
       (className =? "devScratch")
       manageWindow
  , NS "dbeaverScratch" "dbeaver" (resource =? "DBeaver") manageWindow
  , NS "terminalScratch"
       ("alacritty --class " ++ "terminalScratch")
       (resource =? "terminalScratch")
       manageWindow
  , NS "separateScratch"
       (firefoxCmd' "separateScratch" "separate")
       (className =? "separateScratch")
       manageWindow
  , NS "languageScratch"
       (firefoxCmd' "languageScratch" "language")
       (className =? "languageScratch")
       manageWindow
  , NS "thunderbirdScratch" "thunderbird" (resource =? "Mail") manageWindow
  , NS "joplinScratch" "joplin-desktop" (resource =? "joplin") manageWindow
  , NS "signalScratch" "signal-desktop" (resource =? "signal") manageWindow
  , NS "ircScratch" "hexchat" (resource =? "hexchat") manageWindow
  , NS "matrixScratch" "element-desktop" (resource =? "element") manageWindow
  , NS "discordScratch" "discord" (resource =? "discord") manageWindow
  , NS "teamsScratch"
       "teams"
       (resource =? "microsoft teams - preview")
       manageWindow
  , NS
    "todoScratch"
    "urxvt -name blogtodo -e bash -c 'vim ~/Programming/blob_of_code/doc/todo.md'"
    (resource =? "blogtodo")
    manageWindow
  ]
 where
  manageWindow = customFloating $ RationalRect l t w h
  h            = 0.95
  w            = 0.95
  t            = 0.99 - h
  l            = 0.985 - w


myGroupKeys =
  [ ("M-a " ++ key, viewGroup group)
  | (key, group) <- zip (map show [1 .. 9]) myGroups
  ]

overlayAppGroups screenCount appGroups baseConfig =
  baseConfig --addDescrKeys ((winKey, xK_g), xMessage) myKeys baseConfig
      { workspaces  = workspaces baseConfig
                        <+> groupSpaces' screenCount appGroups
      , startupHook = startupHook baseConfig <+> addAppGroups appGroups
      }
    `additionalKeysP` myGroupKeys
  --where myKeys baseConfig = [ subtitle "Workspace groups"] ++ (mkNamedKeymap baseConfig myGroupKeys)


viewGroup :: AppGroup -> X ()
viewGroup group = do
  homeDir     <- liftIO getHomeDirectory
  --catchIO . setCurrentDirectory . expandHome homeDir $ directory group
  screenCount <- liftIO countScreens
  mapM_ spawnIfDead $ apps group
  viewWSGroup $ Main.name group
 where
    -- Replace an initial @~@ character with the home directory.
  expandHome :: FilePath -> FilePath -> FilePath
  expandHome home dirPath = case stripPrefix "~" dirPath of
    Nothing           -> dirPath
    Just strippedPath -> home ++ strippedPath



spawnIfDead :: UniqApp -> X ()
spawnIfDead app = withWindowSet $ \winSet -> do
  matchingWindows <- filterM (runQuery (recognizer app)) (allWindows winSet)
  if null matchingWindows then spawn (spawnCmd app) else return ()

groupManageHook' :: [AppGroup] -> ManageHook
groupManageHook' groups = composeAll $ map groupManageHook groups

groupManageHook :: AppGroup -> ManageHook
groupManageHook group = do
  screenCount <- liftIO countScreens
  let groupWorkspaces = groupSpacesRaw screenCount (Main.name group)
  composeAll $ map (appManageHook groupWorkspaces) (apps group)

appManageHook :: [WorkspaceId] -> UniqApp -> ManageHook
appManageHook groupSpaces app = do
  let targetSpace = appSpace groupSpaces (placer app)
  (recognizer app) --> doShift targetSpace


-- TODO add layouts to screens
data AppGroup = AppGroup
  { name :: String -- | Name to show in log/xmobar
  , directory :: FilePath -- | Root folder of project
  , apps :: [UniqApp] -- which apps to run on in this group
  }

data UniqApp = UniqApp
  { spawnCmd :: String
  , recognizer :: Query Bool
  , placer :: ScreenCount -> PhysicalScreen
  }

groupSpacesRaw :: ScreenCount -> String -> [String]
groupSpacesRaw screenCount groupName =
  [ groupName ++ "-" ++ show id | id <- screenIdxs ]
  where screenIdxs = [0 .. screenCount - 1]

groupSpaces :: ScreenCount -> AppGroup -> [String]
groupSpaces screenCount group = groupSpacesRaw screenCount (Main.name group)

groupSpaces' :: ScreenCount -> [AppGroup] -> [String]
groupSpaces' screenCount groups = concatMap (groupSpaces screenCount) groups

appSpace :: [WorkspaceId] -> Placer -> WorkspaceId -- determine concrete workspaceId to place an app into
appSpace workspaces placer =
  workspaces !! (unPhysicalScreen . placer $ screenCount)
  where screenCount = length workspaces



addAppGroup :: AppGroup -> X ()
addAppGroup (AppGroup groupName _ apps) = do
  screenCount <- liftIO countScreens
  let groupWorkspaces = groupSpacesRaw screenCount groupName
  let physIds         = map P [0 .. screenCount - 1]
  screenIds <- mapM phys2ScreenId physIds -- need to map left-to-right physical screens to arbitrarily given screenId
  addRawWSGroup groupName $ zip screenIds groupWorkspaces

addAppGroups :: [AppGroup] -> X ()
addAppGroups groups = mapM_ addAppGroup groups


type Spawner = String
type Recognizer = Query Bool
type Placer = ScreenCount -> PhysicalScreen -- position the app on a screen, depending on what is there
type ScreenCount = Int -- n

-- TODO may change placer to [WorkspaceId] -> X WorkspaceId to select one, and be able to do queries
left :: ScreenCount -> PhysicalScreen
left _ = P 0 -- always on leftmost/first screen

right :: ScreenCount -> PhysicalScreen
right n = P (n - 1) -- always on rightmost/last screen

midRight :: ScreenCount -> PhysicalScreen
midRight n = P . ceiling $ fromIntegral (n - 1) / 2 -- always on middle screen with tendency to the right

phys2ScreenId :: PhysicalScreen -> X ScreenId
phys2ScreenId physId = fmap (fromMaybe (S 0)) . getScreen def $ physId

unScreenId :: ScreenId -> Int
unScreenId (S n) = n

unPhysicalScreen :: PhysicalScreen -> Int
unPhysicalScreen (P n) = n

blogGroup = AppGroup
  "Blog"
  "~/Programming/blob_of_code"
  [ (UniqApp (firefoxCmd' "devfox" "dev") (className =? "devfox") left)
  , (UniqApp "urxvt -name blogterminal -e bash"
             (resource =? "blogterminal")
             midRight
    )
  , (UniqApp "code ~/Programming/blob_of_code" (resource =? "code") midRight)
  , (UniqApp "chromium-browser" (resource =? "chromium-browser") right)
  ]

myGroups = [blogGroup]

