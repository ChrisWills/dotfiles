{-# LANGUAGE DeriveDataTypeable #-}
-- ~/.xmonad/xmonad.hs
-- Imports {{{
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.IM
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib.Display
import Foreign.C.Types
import Data.Maybe
import Data.Ratio ((%))
import Data.Monoid (Endo(..))
import Data.Functor
import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.Signals
import System.Posix.Types
import System.IO.Error
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Control.Monad
import XMonad.Actions.FloatKeys
-- import XMonad.Util.SpawnNamedPipe -- This is in the darcs version of contrib  
--}}}
-- SpawnNamedPipe {{{
-- | This has been accepted as a patch to xmonad-contrib tip but adding it here
-- so that I'm not tied to running tip
data NamedPipes = NamedPipes { pipeMap :: Map.Map String Handle }
    deriving (Show, Typeable)

instance ExtensionClass NamedPipes where
    initialValue = NamedPipes Map.empty 

-- | When 'spawnNamedPipe' is executed with a command "String" and a name
-- "String" respectively.  The command string is spawned with 'spawnPipe' (as
-- long as the name chosen hasn't been used already) and the "Handle" returned
-- is saved in Xmonad's state associated with the name "String". 
spawnNamedPipe :: String -> String -> X ()
spawnNamedPipe cmd name = do
  b <- XS.gets (Map.member name . pipeMap) 
  unless b $ do
    h <- spawnPipe cmd 
    XS.modify (NamedPipes . Map.insert name h . pipeMap)   

-- | Attempts to retrieve a "Handle" to a pipe previously stored in Xmonad's
-- state associated with the given string via a call to 'spawnNamedPipe'. If the
-- given string doesn't exist in the map stored in Xmonad's state Nothing is
-- returned.   
getNamedPipe :: String -> X (Maybe Handle)
getNamedPipe name = XS.gets (Map.lookup name . pipeMap)

-- }}}
-- ExtensibleState {{{
data StartupProgs = StartupProgs { getPids :: [ProcessID] }
    deriving (Show, Typeable)

instance ExtensionClass StartupProgs where
    initialValue = StartupProgs []

-- Proof of concept for xmonad mailing list
--data MasterPaneFlag = MasterPaneFlag { getFlag :: Bool }
--    deriving (Show, Typeable)
--
--instance ExtensionClass MasterPaneFlag where
--    initialValue = MasterPaneFlag False 
--
--pickIncrFun :: Bool -> X () 
--pickIncrFun flag = if flag then (sendMessage (IncMasterN (1))) else (sendMessage (IncMasterN 0))
--
--toggleMasterPane :: X ()
--toggleMasterPane  = do
--    flag <- XS.get 
--    XS.modify(MasterPaneFlag . not . getFlag)
--    asks config >>= \c -> setLayout (XMonad.layoutHook c) >> pickIncrFun (getFlag flag)
     
--}}} 
-- Scratchpads {{{
scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "volume" "xterm -title Volume -xrm \"XTerm*allowTitleOps:false\" -e alsamixer" (title =? "Volume") 
         (customFloating $ W.RationalRect 0 (1/48) (1/8) (47/48))
    , NS "music" "xterm -title Music -xrm \"XTerm*allowTitleOps:false\" -e ncmpc" (title =? "Music")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10))
    , NS "term" "xterm -title Scratchpad -xrm \"XTerm*allowTitleOps:false\"" (title =? "Scratchpad")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10))
    , NS "dict" "artha" (title =? "Artha ~ The Open Thesaurus")
         (customFloating $ W.RationalRect (2/6) (1/6) (2/6) (4/6))] 

myNSManageHook :: NamedScratchpads -> ManageHook
myNSManageHook s = namedScratchpadManageHook s 
    <+> composeOne
        [ title =? "Music" -?> ask >>= \w -> liftX $ setOpacity w 0.7 >> idHook 
        , title =? "Scratchpad" -?> ask >>= \w -> liftX $ setOpacity w 0.7 >> idHook
        , title =? "Volume" -?> ask >>= \w -> liftX $ setOpacity w 0.8 >> idHook
        --, title =? "Artha ~ The Open Thesaurus" -?> ask >>= \w -> liftX $ setOpacity w 0.8 >> idHook
        ]
---}}}
-- ManageHook {{{
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
    --, doWindowPropsMatch myWebs       -?> doShift  "3:web"
    --, doWindowPropsMatch myDevs       -?> doShift  "4:dev"
    --, doWindowPropsMatch myWines      -?> doF(W.shift "6:wine")
    ] 
    where
        myIgnores       = [(resource,   ["desktop","desktop_window","notify-osd","stalonetray","trayer"])]
        myCenterFloats  = [(className,  ["VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor","qemu","artha"])
                          ,(title,      ["Google Chrome Options","Chromium Options"])]
        myFloats        = [(className,  ["Gimp","ij-ImageJ"])]
--      myWebs          = [(className,  ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail"])]
--      myDevs          = [(className,  ["Eclipse","eclipse","Netbeans","Gvim"])]
--      myWines         = [(className,  ["Wine"])]
-- }}}
-- UrgencyHook {{{
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
-- }}}
-- LogHook {{{    
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
    dynamicLogWithPP $ defaultPP
          { ppCurrent           = dzenColor colorYellow colorDarkGrey . pad
          , ppVisible           = dzenColor colorWhite colorDarkGrey . pad
          , ppHidden            = dzenColor colorInvisible colorInvisible . pad
          , ppUrgent            = dzenColor colorMedGrey colorYellow
          , ppHiddenNoWindows   = dzenColor colorMedGrey colorDarkGrey . pad
          , ppWsSep             = ""
          , ppSep               = " | "
          , ppLayout            = dzenColor colorYellow colorDarkGrey . (chooseLayoutIcon xmonadDir) 
          , ppSort              = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
          , ppTitle             = ("" ++) . dzenColor colorWhite colorDarkGrey . dzenEscape
          , ppOutput            = maybe (\s -> return ()) hPutStrLn dzenHandle}
    fadeInactiveLogHook 0xdddddddd
    setWMName "LG3D"
-- }}}
-- Layout {{{
layoutHook' = avoidStruts $
    noBorders Full
    ||| smartBorders tiled
    ||| smartBorders (Mirror tiled)
    ||| smartBorders simplestFloat
    ||| withIM (1%7) (Role "buddy_list") (smartBorders Grid) -- It would be cool to be able to pass a ManageHook in directly here...patch? 
        where tiled   = ResizableTall 1 (2/100) (1/2) []

--}}}
-- Theme {{{
-- Color names are easier to remember:
colorInvisible        = ""
colorOrange           = "#ff7701"
colorDarkGrey         = "#161616"
colorMedGrey          = "#444444"
colorPink             = "#e3008d"
colorGreen            = "#00aa4a"
colorBlue             = "#008dd5"
colorYellow           = "#ebac54"
--colorWhite            = "#cfbfad"
colorWhite            = "#ffffff"
colorLightBlue        = "#afdfff"
colorDarkGrey2        = "#262626"
colorLightGrey        = "#a6a6a6"
normalBorderColor'    = "#262626"
focusedBorderColor'   = "#9F6C3B"

barFont  = "Sauce Code Powerline:size=9"
barXFont = "inconsolata:size=14"
xftFont  = "xft:Sauce Code Powerline:pixelsize=18"
--}}}
-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig
        { font                  = xftFont
        , bgColor               = colorDarkGrey
        , fgColor               = colorLightGrey
        , bgHLight              = colorLightGrey
        , fgHLight              = colorDarkGrey
        , promptBorderWidth     = 0
        , height                = 20
        , historyFilter         = deleteConsecutive }
-- }}}
-- Key Map {{{

isFloatDo f1 f2 w s = if (M.member w (W.floating s)) then f1 w else f2 w

myKeymap = 
    [   ("M-<Return>", spawn $ XMonad.terminal myConfig)
    ,   ("M-p", spawn "xterm")
    ,   ("M-r", shellPrompt mXPConfig)
    ,   ("M-S-r", xmonadPrompt mXPConfig)
    ,   ("M-S-c", kill)
    ,   ("M-<Space>", sendMessage NextLayout)
    ,   ("M-S-<Space>", asks config >>= \c -> setLayout (XMonad.layoutHook c))
    ,   ("M-n", refresh)
    ,   ("M-<Tab>", windows W.focusDown)
    ,   ("M-j", windows W.focusDown)
    ,   ("M-k", windows W.focusUp)
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
    --,   ("M-v", toggleMasterPane)
    ,   ("M-b", sendMessage ToggleStruts)
    ,   ("M-S-q",cleanupHook >> spawn "xmonad --recompile; xmonad --restart" >> io exitSuccess)
    ,   ("M-q", cleanupHook >> spawn "xmonad --recompile; xmonad --restart")
    ,   ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ,   ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ,   ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    ,   ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
    ,   ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
    ,   ("<XF86ScreenSaver>", spawn "xscreensaver-command -lock")
    ,   ("M-o", namedScratchpadAction scratchpads "term")
    ,   ("M-s", namedScratchpadAction scratchpads "volume")
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
--}}}
-- StartupHook {{{

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
    spawn program
    windows $ W.greedyView workspace

getDefaultScreenWidth :: X CInt 
getDefaultScreenWidth = withDisplay $ \dpy ->
    return $ displayWidth dpy $ defaultScreen dpy

-- | Run this as a namedPipe so that we can update it with loghook
myStatusBar screenWidth =
    "dzen2 -x 0 -y '0' -h '16' -w " ++ show (screenWidth - 326) ++ " -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn " ++ (show barFont) 

-- | These stay running during an xmonad session. We save the pids so we can kill these on shutdown/restart 
startupApps screenWidth =
     ["while true; do date +'%a %b %d %l:%M%p'; sleep 30; done | dzen2 -x "++ show (screenWidth - 136) ++" -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn " ++ (show barFont) 
     ,"/usr/bin/stalonetray --geometry 12x1+"++ show (screenWidth - 326) ++"+0 --max-geometry 12x1+"++ show (screenWidth - 326) ++"+0 --background '#161616' --icon-size 16 --icon-gravity NE --kludges=force_icons_size" 
     ,"batt_stat.exe"
     ,"nm-applet"
     ,"xscreensaver"]

-- | These run once on startup and exit immediately so we don't care about their pids
startupCmds xmonadDir =
    ["xset r rate 200 60"
    ,"xrdb -load .Xdefaults"
    ,"xmodmap " ++ xmonadDir ++ "/.Xmodmap"
    ,"feh --bg-fill " ++ xmonadDir ++ "/wallpapers/current"
    ,"xbacklight -set 70"
    ,"nm-applet"
    ,"xfce4-power-manager"]

startupHook' :: X ()
startupHook' = do
    screenWidth <- getDefaultScreenWidth
    xmonadDir <- getXMonadDir
    checkKeymap myConfig myKeymap
    spawnNamedPipe (myStatusBar screenWidth) "dzenPipe"
    mapM spawn (startupCmds xmonadDir)
    appPids <- mapM (\app -> io $ spawnPID app) (startupApps screenWidth)
    XS.put (StartupProgs appPids)
    setWMName "LG3D"
    spawnOn "1" "google-chrome"
    spawnOn "2" "xterm -e '/home/cwills/.xmonad/tmux-dev.sh'" 
    -- spawnOn "12" "pidgin"

cleanupHook :: X ()
cleanupHook = do
    pids <- XS.get
    io $ mapM_ (\p -> catchIOError (signalProcess sigTERM p) (\_ -> return ())) (getPids pids)
-- }}} 
-- Main {{{
myConfig =
    defaultConfig
        { terminal            = "xterm -e 'tmux -2'"
        , workspaces          = map show [1 .. 15] ++ ["NSP"]
        , modMask             = mod4Mask -- alt and windows key are swapped with xmodmap in startup hook
        , keys                = \c -> mkKeymap c myKeymap
        , startupHook         = startupHook'
        , layoutHook          = layoutHook'
        , handleEventHook     = docksEventHook
        , manageHook          = (manageSpawn <+> manageHook') <+> manageDocks -- manageSpawn is needed for spawnOn to work
        , logHook             = logHook'
        , normalBorderColor   = normalBorderColor'
        , focusedBorderColor  = focusedBorderColor' }

main = xmonad $ withUrgencyHook myUrgencyHook myConfig
--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
