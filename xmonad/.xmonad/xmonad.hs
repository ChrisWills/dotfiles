{-# LANGUAGE DeriveDataTypeable #-}
-- ~/.xmonad/xmonad.hs
--import Control.Monad
--import Data.Functor
--import Data.List (find)
--import Data.Maybe
import Data.Monoid (Endo(..))
--import Data.Ratio ((%))
import Foreign.C.Types
import Graphics.X11.Xlib.Display
import qualified Data.Map as M
--import qualified Data.Map.Strict as Map
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Posix.Types
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows
--import XMonad.Layout.Grid
--import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
--import XMonad.Layout.SimplestFloat
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
--import XMonad.Util.Run
--import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)

import XMonad.Actions.ConditionalKeys -- manually hacked into contrib by cwills to place nice with ghc-mod 
import XMonad.Util.SpawnNamedPipe -- available in >= xmonad-contrib-0.12 

data StartupProgs = StartupProgs { getPids :: [ProcessID] }
    deriving (Show, Typeable)

instance ExtensionClass StartupProgs where
    initialValue = StartupProgs []

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "volume" (xtermCmd ++ " -title Volume -xrm \"XTerm*allowTitleOps:false\" -e alsamixer") (title =? "Volume")
         (customFloating $ W.RationalRect 0 (1/48) (1/8) (47/48))
    , NS "NetworkManager" (xtermCmd ++ " -title NetworkManager -xrm \"XTerm*allowTitleOps:false\" -e nmtui connect") (title =? "NetworkManager") 
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10))
    , NS "music" (xtermCmd ++ " -title Music -xrm \"XTerm*allowTitleOps:false\" -e ncmpc") (title =? "Music")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10))
    , NS "term" (xtermCmd ++ " -title Scratchpad -xrm \"XTerm*allowTitleOps:false\"") (title =? "Scratchpad")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10))
    , NS "dict" "artha" (title =? "Artha ~ The Open Thesaurus")
         (customFloating $ W.RationalRect (2/6) (1/6) (2/6) (4/6))] 

myNSManageHook :: NamedScratchpads -> ManageHook
myNSManageHook s = namedScratchpadManageHook s 
    <+> composeOne
        [ title =? "Music" -?> ask >>= \w -> liftX $ setOpacity w 0.7 >> idHook 
        , title =? "Scratchpad" -?> ask >>= \w -> liftX $ setOpacity w 0.7 >> idHook
        , title =? "Volume" -?> ask >>= \w -> liftX $ setOpacity w 0.8 >> idHook , title =? "NetworkManager" -?> ask >>= \w -> liftX $ setOpacity w 0.8 >> idHook
        ]
doWindowPropsMatchHelper :: (Query String, [String]) -> Query Bool -> Query Bool
doWindowPropsMatchHelper (prop, l) acc = do
    s <- prop ; a <- acc
    return (a || (s `elem` l))
    
-- | This is a nice code compactor for managehooks. It allows us to match
-- multiple window property types in a single list which allows for a
-- one-to-one mapping of window-lists -> ManageHooks. It's hard to name this
-- function susinctly; ignore the name and read the type sig.   
doWindowPropsMatch :: [(Query String, [String])] -> Query Bool
doWindowPropsMatch [] = return False
doWindowPropsMatch xs = foldr doWindowPropsMatchHelper (return False) xs 

-- | A trick for fullscreen but stil allow focusing of other workspaces. 
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

-- | A trick for moving an urgent window to the current workspace.
doInYoFace :: ManageHook
doInYoFace = do 
    cws <- currentWs
    doShift cws <+> doCenterFloat

manageHook' :: ManageHook
manageHook' = myNSManageHook scratchpads <+> composeOne 
    [ doWindowPropsMatch myIgnores      -?> doIgnore 
    , doWindowPropsMatch myCenterFloats -?> doCenterFloat  
    , doWindowPropsMatch myFloats       -?> doFloat  
    , isFullscreen                      -?> myDoFullFloat                           
    ] 
    where
        myIgnores       = [(resource,   ["desktop","desktop_window","notify-osd","stalonetray","trayer"])]
        myCenterFloats  = [(className,  ["VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor","qemu","artha"])
                          ,(title,      ["Google Chrome Options","Chromium Options"])]
        myFloats        = [(className,  ["Gimp","ij-ImageJ"])]

-- | This is the "ManageHook" that gets run on urgent windows. Any windows not
-- caught by this manage hook are caught by logHook' and their workspace is
-- highlighted in the Dzen topbar.
myUrgencyManageHook :: ManageHook
myUrgencyManageHook = composeOne
    [ doWindowPropsMatch myUrgents -?> doInYoFace ]
    where
        myUrgents = [(className, ["Pidgin"])
                    ,(title,     ["Testing"])]

-- | Basically stolen from the 'manage' function in XMonad.Operations. It is
-- yet to be seen whether we need to do any of the other stuff that the 'manage'
-- function does before modifying the "WindowSet"
applyManageHook :: ManageHook -> Window -> X ()
applyManageHook mh w = do
    g <- appEndo <$> userCodeDef (Endo id) (runQuery mh w) 
    windows g

-- | This is the thing that is passed into withUrgencyHook. We are utilizing
-- the "do whatever you want manually" urgencyHook. Although it could be agrued
-- that the above is a pretty standard thing that many people might like to do
-- so consider submitting a patch. 
myUrgencyHook :: (Window -> X ())
myUrgencyHook = applyManageHook myUrgencyManageHook 

chooseLayoutIcon :: String -> String -> String
chooseLayoutIcon xmonadDir layoutName =
    case layoutName of
    "ResizableTall"        -> "^i(" ++ xmonadDir ++ "/dzen/tall.xbm)"
    "Mirror ResizableTall" -> "^i(" ++ xmonadDir ++ "/dzen/mtall.xbm)"
    "Full"                 -> "^i(" ++ xmonadDir ++ "/dzen/full.xbm)"
    "SimplestFloat"        -> "~"
    "Grid"                 -> "^i(" ++ xmonadDir ++ "/dzen/grid.xbm)"
    "IM Grid"              -> "^i(" ++ xmonadDir ++ "/dzen/grid.xbm)"
    _                      -> layoutName 


logHook' :: X ()
logHook' = do 
  dzenHandle <- getNamedPipe "dzenPipe" 
  xmonadDir <- getXMonadDir 
  fadeInactiveLogHook 0xdddddddd
  setWMName "LG3D"
  dynamicLogWithPP
    $ def { ppCurrent         = dzenColor barCurrent barBg . pad
          , ppVisible         = dzenColor barVisible colorInvisible . pad
          , ppHidden          = dzenColor barVisible colorInvisible . pad
          , ppUrgent          = dzenColor barBg barCurrent 
          , ppHiddenNoWindows = dzenColor colorInvisible colorInvisible . pad
          , ppWsSep           = ""
          , ppSep             = " | "
          , ppLayout          = dzenColor barVisible barBg . (chooseLayoutIcon xmonadDir) 
          , ppSort            = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort def
          , ppTitle           = ("" ++) . dzenColor barFg colorInvisible . dzenEscape
          , ppOutput          = maybe (\s -> return ()) hPutStrLn dzenHandle
          }

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20
borderWidth'          = 4

-- Solarized dark
--base03  = "#002b36"
--base02  = "#073642"
--base00  = "#657b83"
--base0 = "#839496"
--base1   = "#93a1a1"
--base2   = "#eee8d5"
--yellow  = "#b58900"
--red     = "#dc322f"
--blue    = "#268bd2"
--cyan    = "#2aa198"
--
--colorInvisible        = ""
--normalBorderColor'    = base03
--focusedBorderColor'   = yellow
--barBg = base02
--barFg = base0 
--barCurrent = cyan
--barVisible = cyan
--tabActiveBg = base02
--tabActiveFg = base0
--tabInactiveBg = base03
--tabInactiveFg = base0
--tabActiveBorder = tabActiveBg 
--tabInactiveBorder = tabInactiveBg 
--xtermCmd = "xterm -class XTermSolarized"

-- gruvbox dark 
black0 = "#282828"
black0s = "#32302f"
black01 = "#3c3836"
black02 = "#504945"
black1 = "#928374"
red0 = "#cc241d"
red1 = "#fb4934"
green0 = "#98971a"
green1 = "#b8bb26"
yellow0 = "#d79921"
yellow1 = "#fabd2f"
blue0 = "#458588"
blue1 = "#83a598"
violet0 = "#b16286"
violet1 = "#d3869b"
cyan0 = "#689d6a"
cyan1 = "#8ec07c"
white0 = "#a89984"
white1 = "#ebdbb2"

colorInvisible = ""
normalBorderColor' = black0s
focusedBorderColor' = yellow1
barBg = black0
barFg = white0
barCurrent = yellow1
barVisible = yellow0
tabActiveBg = black02  
tabActiveFg = white1
tabInactiveBg = black01
tabInactiveFg = white1 
tabActiveBorder = tabActiveBg 
tabInactiveBorder = tabInactiveBg 
xtermCmd = "xterm -class XTermGruvboxDark"

myTabTheme = def
  { fontName              = barXFont
  , activeColor           = tabActiveBg
  , inactiveColor         = tabInactiveBg
  , activeBorderColor     = tabActiveBorder
  , inactiveBorderColor   = tabInactiveBorder
  , activeTextColor       = tabActiveFg
  , inactiveTextColor     = tabInactiveFg }

-- Run Dialog Theme
myPromptConfig = def
  { font                  = xftFont
  , bgColor               = barBg
  , fgColor               = barFg
  , bgHLight              = barCurrent 
  , fgHLight              = barBg
  , promptBorderWidth     = 0
  , height                = 20
  , historyFilter         = deleteConsecutive }

layoutHook' = tabs
              ||| flexTiled 
              ||| flexMirrorTiled 
              ||| flexFull
  where
    tabs = named "Tabs"
      $ avoidStruts
      $ smartBorders
      $ addTabs shrinkText myTabTheme
      $ Simplest

    flexTiled = avoidStruts
      $ smartBorders
      $ WN.windowNavigation
      $ boringAuto
      $ addTabs shrinkText myTabTheme
      $ subLayout [] (named "Tabs" $ Simplest) 
      $ ResizableTall 1 (2/100) (1/2) []

    flexMirrorTiled = avoidStruts
      $ smartBorders
      $ WN.windowNavigation
      $ boringAuto
      $ addTabs shrinkText myTabTheme
      $ subLayout [] (named "Tabs" $ Simplest) 
      $ (Mirror (ResizableTall 1 (2/100) (1/2) []))

    flexFull = avoidStruts
      $ WN.windowNavigation
      $ boringAuto
      $ addTabs shrinkText myTabTheme
      $ subLayout [] (named "Tabs" $ Simplest) 
      $ noBorders Full 

  --- 

barFont  = "DejaVu Sans Mono Book:size=10"
barXFont = "inconsolata:size=14"
xftFont  = "xft:Sauce Code Powerline:pixelsize=18"


isFloatDo f1 f2 w s = if (M.member w (W.floating s)) then f1 w else f2 w

-- Useful manual commands for testing 
pcmds :: [(String, X ())]
pcmds =
    [ ("expandTowardsR", sendMessage $ ExpandTowards R)
    , ("expandTowardsL", sendMessage $ ExpandTowards L)
    , ("expandTowardsD", sendMessage $ ExpandTowards D)
    , ("expandTowardsU", sendMessage $ ExpandTowards U)
    , ("shrinkFromR", sendMessage $ ShrinkFrom R)
    , ("shrinkFromL", sendMessage $ ShrinkFrom L)
    , ("shrinkFromD", sendMessage $ ShrinkFrom D)
    , ("shrinkFromU", sendMessage $ ShrinkFrom U)
    , ("rotate", sendMessage Rotate)
    , ("swap", sendMessage Swap)
    , ("focusParent", sendMessage FocusParent)
    , ("selectNode", sendMessage SelectNode)
    , ("moveNode", sendMessage MoveNode)
    , ("unmerge", withFocused (sendMessage . UnMerge))
    , ("merge", withFocused (sendMessage . MergeAll))
    , ("mergeUp", (sendMessage . pullGroup $ U ))
    ]

dirKeys        = ["j","k","h","l"]
arrowKeys      = ["<D>","<U>","<L>","<R>"]
dirs           = [ D,  U,  L,  R ]

myKeymap :: [(String, X ())] 
myKeymap = 
  [   ("M-<Return>", spawn $ XMonad.terminal myConfig)
  ,   ("M-p", spawn xtermCmd)
  ,   ("M-r", shellPrompt myPromptConfig)
  ,   ("M-S-r", xmonadPromptC pcmds myPromptConfig)
  ,   ("M-S-c", kill)
  ,   ("M-<Space>", sendMessage NextLayout)
  ,   ("M-S-<Space>", asks config >>= \c -> setLayout (XMonad.layoutHook c))
  --,   ("M-n", refresh)
  ,   ("M-<Tab>", windows W.focusDown)
  ,   ("M-j", bindOn LD [("Tabs", return () ), ("Tabbed Full", windows W.focusDown), ("", focusDown) ])
  ,   ("M-k", bindOn LD [("Tabs", return () ), ("Tabbed Full", windows W.focusUp), ("", focusUp) ])
  --    ,   ("M-j", windowGo D True)
  --    ,   ("M-k", windowGo U True)  
  --    ,   ("M-h", windowGo L True)  
  --    ,   ("M-l", windowGo R True)  
  ,   ("M-m", windows W.focusMaster)
  ,   ("M-S-<Return>", windows W.swapMaster)
  ,   ("M-S-j", windows W.swapDown)
  ,   ("M-S-k", windows W.swapUp)
  ,   ("M-h", withFocused $ withWindowSet . isFloatDo (keysResizeWindow (-10,0) (0,0)) (\_ -> sendMessage Shrink))
  ,   ("M-l", withFocused $ withWindowSet . isFloatDo (keysResizeWindow (10,0) (0,0)) (\_ -> sendMessage Expand))
  ,   ("M-S-h", withFocused $ withWindowSet . isFloatDo (keysResizeWindow (0,-10) (0,0)) (\_ -> sendMessage MirrorShrink))
  ,   ("M-S-l", withFocused $ withWindowSet . isFloatDo (keysResizeWindow (0,10) (0,0)) (\_ -> sendMessage MirrorExpand)) -- Doesn't work for some reason
  ,   ("M-t", withFocused $ windows . W.sink)
  ,   ("M-S-t", sinkAll)
  ,   ("M-<Backspace>", focusUrgent)
  ,   ("M-S-<Backspace>",clearUrgents)
  ,   ("M-,", sendMessage (IncMasterN 1))
  ,   ("M-.", sendMessage (IncMasterN (-1)))
  ,   ("M-'", bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
  ,   ("M-;", bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
    --,   ("M-v", toggleMasterPane)
  ,   ("M-b", sendMessage ToggleStruts)
  ,   ("M-S-q",cleanupHook >> spawn "xmonad --recompile; xmonad --restart" >> io exitSuccess)
  ,   ("M-q", cleanupHook >> spawn "xmonad --recompile; xmonad --restart")
  ,   ("<XF86AudioMute>", spawn "amixer set Master toggle")
  ,   ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  ,   ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  ,   ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  ,   ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  ,   ("M-S-x", spawn "xscreensaver-command -lock")
  ,   ("M-o", namedScratchpadAction scratchpads "term")
  ,   ("M-s", namedScratchpadAction scratchpads "volume")
  ,   ("M-n", namedScratchpadAction scratchpads "NetworkManager")
  ,   ("M-i", namedScratchpadAction scratchpads "music")
  ,   ("M-w", namedScratchpadAction scratchpads "dict")
    --,   ("M-S-<Space>", layoutScreens 3 (fixedLayout [(Rectangle 0 0 1360 768),(Rectangle 1360 0 1280 1024),(Rectangle 2640 0 1280 1024)]))
      --,   ("M-C-S-<Space>", rescreen)
  ]
  ++
  [ ("M-S-" ++ key, (windows . W.shift) name)
  | (name, key) <-
      zip (XMonad.workspaces myConfig) $ map show [1 .. 9] ++ ["0","<F1>","<F2>","<F3>","<F4>","<F5>","<F6>"]]
  ++
  [ ("M-" ++ key, (windows . W.greedyView) name)
  | (name, key) <-
      zip (XMonad.workspaces myConfig) $ map show [1 .. 9] ++ ["0","<F1>","<F2>","<F3>","<F4>","<F5>","<F6>"]]
  ++
  [ ("M-M1-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . W.view))
  | (key, scr)  <- zip "1234" [0,1,2,4] -- change to match your screen order and number of screens
  ]

getDefaultScreenWidth :: X CInt 
getDefaultScreenWidth = withDisplay $ \dpy ->
    return $ displayWidth dpy $ defaultScreen dpy

-- | Run this as a namedPipe so that we can update it with loghook


myStatusBar :: CInt -> String 
myStatusBar screenWidth =
  "dzen2 -x 0 -y '0' -h '16' -w "
  ++ show (screenWidth - 326)
  ++ " -ta 'l' -fg '" ++ barFg ++ "' -bg '" ++ barBg ++ "' -fn "
  ++ (show barFont) 

-- | These stay running during an xmonad session. We save the pids so we can kill these on shutdown/restart
startupApps :: CInt -> [String] 
startupApps screenWidth =
     ["while true; do date +'%a %b %d %l:%M%p'; sleep 30; done | dzen2 -x "
      ++ show (screenWidth - 136)
      ++" -y '0' -h '16' -w '136' -ta 'c' -fg '" ++ barFg ++ "' -bg '" ++ barBg ++ "' -fn "
      ++ (show barFont) 
     ,"stalonetray --geometry 12x1+"
       ++ show (screenWidth - 326)
       ++"+0 --max-geometry 12x1+"
       ++ show (screenWidth - 326)
       ++"+0 --background '" ++ barBg ++ "' --icon-size 16 --icon-gravity NE --kludges=force_icons_size" 
     ,"batt_stat.exe"
     ,"nm-applet"
     ,"xscreensaver"]

-- | These run once on startup and exit immediately so we don't care about their pids
startupCmds :: String -> [String]
startupCmds xmonadDir =
    ["xset r rate 200 60"
    ,"xrdb -load .Xdefaults"
    ,"xmodmap " ++ xmonadDir ++ "/.Xmodmap"
    ,"xbacklight -set 70"
    ,"nm-applet"
    ,"xsetroot -bg black"
    ,"xfce4-power-manager"
    ,"syncthing-gtk -m"]

startupHook' :: X ()
startupHook' = do
    screenWidth <- getDefaultScreenWidth
    xmonadDir <- getXMonadDir
    checkKeymap myConfig myKeymap
    spawnNamedPipe (myStatusBar screenWidth) "dzenPipe"
    mapM spawn $ startupCmds xmonadDir
    appPids <- mapM (io . spawnPID) $ startupApps screenWidth
    XS.put (StartupProgs appPids)
    setWMName "LG3D"
    spawnOn "1" "firefox"
    spawnOn "2" (xtermCmd ++ " -e '/home/cwills/.xmonad/tmux-dev.sh'")
    -- spawnOn "12" "pidgin"

cleanupHook :: X ()
cleanupHook = do
  pids <- XS.get
  io $ mapM_
    (\p -> catchIOError (signalProcess sigTERM p) (\_ -> return ()))
    (getPids pids)

myConfig =
  def
  { terminal            = xtermCmd ++ " -e 'tmux -2'"
  , workspaces          = map show [1 .. 15] ++ ["NSP"]
  , modMask             = mod4Mask -- alt and windows key are swapped with xmodmap in startup hook
  , keys                = \c -> mkKeymap c myKeymap
  , startupHook         = startupHook'
  , layoutHook          = layoutHook'
  , handleEventHook     = docksEventHook
  , manageHook          = (manageSpawn <+> manageHook') <+> manageDocks -- manageSpawn is needed for spawnOn to work
  , logHook             = logHook'
  , normalBorderColor   = normalBorderColor'
  , focusedBorderColor  = focusedBorderColor'
--  , borderWidth         = borderWidth'
  }


main :: IO ()
main = xmonad $ withUrgencyHook myUrgencyHook myConfig
