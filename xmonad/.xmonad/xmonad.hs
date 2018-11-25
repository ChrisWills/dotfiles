{-# LANGUAGE DeriveDataTypeable #-}
-- ~/.xmonad/xmonad.hs
import System.Exit
import System.IO
import Control.Monad
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.Navigation2D
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
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Named
import XMonad.Layout.BoringWindows
import XMonad.Layout.NoFrillsDecoration
import qualified XMonad.Layout.WindowNavigation as WN
import XMonad.Layout.Simplest
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
import Data.List (find)
import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.Signals
import System.Posix.Types
import System.IO.Error
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Control.Monad
import XMonad.Actions.FloatKeys
-- import XMonad.Util.SpawnNamedPipe -- This is in the darcs version of contrib  
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
     
-- $usage
-- Place this file in ~/.xmonad/lib/XMonad/Actions/ConditionalKeys.hs
--
--          TODO choose API and module split and document correctly.
--          next paragraph for separate modules, rather than combined
--          even though should merge this and PerWorkspaceKeys.
--
-- See also 'XMonad.Actions.PerWorkspaceKeys' from which this module is shamelessly
-- derived.  -- Use 'XMonad.Layout.Named' to shorten and distinguish the layout
-- descriptions you want to bind conditionally.
--
-- Add something like the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >  import XMonad.Actions.ConditionalKeys
--
-- >  import XMonad.Layout.Named
-- >  import XMonad.Layout.ResizableTile
--
-- >     layoutHook = avoidStruts (named "MRT" (Mirror rt) ||| rt) ||| Full
-- >       where rt = ResizableTall 2 (1/118) (11/15) []
--
-- >     , ("C-M-h", bindOn LD [("MRT", sendMessage MirrorExpand), ("", sendMessage Shrink)]
-- >     , ("C-M-l", bindOn LD [("MRT", sendMessage MirrorShrink), ("", sendMessage Expand)]
-- >     , ("C-M-k", bindOn LD [("MRT", sendMessage Shrink), ("", sendMessage MirrorExpand)]
-- >     , ("C-M-j", bindOn LD [("MRT", sendMessage Expand), ("", sendMessage MirrorShrink)]


data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)


-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()

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
    $ def { ppCurrent         = dzenColor colorYellow colorDarkGrey . pad
          , ppVisible         = dzenColor colorWhite colorDarkGrey . pad
          , ppHidden          = dzenColor colorInvisible colorInvisible . pad
          , ppUrgent          = dzenColor colorMedGrey colorYellow
          , ppHiddenNoWindows = dzenColor colorMedGrey colorDarkGrey . pad
          , ppWsSep           = ""
          , ppSep             = " | "
          , ppLayout          = dzenColor colorYellow colorDarkGrey . (chooseLayoutIcon xmonadDir) 
          , ppSort            = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort def
          , ppTitle           = ("" ++) . dzenColor colorWhite colorDarkGrey . dzenEscape
          , ppOutput          = maybe (\s -> return ()) hPutStrLn dzenHandle
          }

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

topBarTheme = def
    { fontName              = barXFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
  { fontName              = barXFont
  , activeColor           = active
  , inactiveColor         = base02
  , activeBorderColor     = active
  , inactiveBorderColor   = base02
  , activeTextColor       = base03
  , inactiveTextColor     = base00
  }

layoutHook' = avoidStruts $
--    noBorders Full
  tabs
--    ||| smartBorders tiled
  ||| smartBorders flexTiled 
  ||| flexFull
--    ||| smartBorders (Mirror tiled)
--   ||| smartBorders simplestFloat
--    ||| withIM (1%7) (Role "buddy_list") (smartBorders Grid) -- It would be cool to be able to pass a ManageHook in directly here...patch? 
--        where tiled   = ResizableTall 1 (2/100) (1/2) []
  where
    -- addTopBar           = noFrillsDeco shrinkText topBarTheme
    
    tabs = named "Tabs"
      $ avoidStruts
      $ smartBorders
      $ addTabs shrinkText myTabTheme
      $ Simplest

    flexTiled = avoidStruts
      $ WN.windowNavigation
      $ boringAuto
      $ addTabs shrinkText myTabTheme
      $ subLayout [] (named "Tabs" $ Simplest) 
      $ ResizableTall 1 (2/100) (1/2) []

    flexFull = avoidStruts
      $ WN.windowNavigation
      $ boringAuto
      $ addTabs shrinkText myTabTheme
      $ subLayout [] (named "Tabs" $ Simplest) 
      $ noBorders Full 


  --- 
  -- color names are easier to remember:
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
mXPConfig :: XPConfig
mXPConfig =
    def
        { font                  = xftFont
        , bgColor               = colorDarkGrey
        , fgColor               = colorLightGrey
        , bgHLight              = colorLightGrey
        , fgHLight              = colorDarkGrey
        , promptBorderWidth     = 0
        , height                = 20
        , historyFilter         = deleteConsecutive }

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
    , ("megeUp", (sendMessage . pullGroup $ U ))
    ]

dirKeys        = ["j","k","h","l"]
arrowKeys      = ["<D>","<U>","<L>","<R>"]
dirs           = [ D,  U,  L,  R ]

myKeymap = 
  [   ("M-<Return>", spawn $ XMonad.terminal myConfig)
  ,   ("M-p", spawn "xterm")
  ,   ("M-r", shellPrompt mXPConfig)
  ,   ("M-S-r", xmonadPromptC pcmds mXPConfig)
  ,   ("M-S-c", kill)
  ,   ("M-<Space>", sendMessage NextLayout)
  ,   ("M-S-<Space>", asks config >>= \c -> setLayout (XMonad.layoutHook c))
  ,   ("M-n", refresh)
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
  
spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
  spawn program
  windows $ W.greedyView workspace

getDefaultScreenWidth :: X CInt 
getDefaultScreenWidth = withDisplay $ \dpy ->
    return $ displayWidth dpy $ defaultScreen dpy

-- | Run this as a namedPipe so that we can update it with loghook
myStatusBar screenWidth =
  "dzen2 -x 0 -y '0' -h '16' -w "
  ++ show (screenWidth - 326)
  ++ " -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn "
  ++ (show barFont) 

-- | These stay running during an xmonad session. We save the pids so we can kill these on shutdown/restart 
startupApps screenWidth =
     ["while true; do date +'%a %b %d %l:%M%p'; sleep 30; done | dzen2 -x "
      ++ show (screenWidth - 136)
      ++" -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn "
      ++ (show barFont) 
     ,"/usr/bin/stalonetray --geometry 12x1+"
       ++ show (screenWidth - 326)
       ++"+0 --max-geometry 12x1+"
       ++ show (screenWidth - 326)
       ++"+0 --background '#161616' --icon-size 16 --icon-gravity NE --kludges=force_icons_size" 
     ,"batt_stat.exe"
     ,"nm-applet"
     ,"xscreensaver"]

-- | These run once on startup and exit immediately so we don't care about their pids
startupCmds xmonadDir =
    ["xset r rate 200 60"
    ,"xrdb -load .Xdefaults"
    ,"xmodmap " ++ xmonadDir ++ "/.Xmodmap"
    ,"convert -size 1600x900 xc:black " ++ xmonadDir ++ "/.wallpaper.png"
    ,"xbacklight -set 70"
    ,"nm-applet"
    ,"feh --bg-fill " ++ xmonadDir ++ "/.wallpaper.png"
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
  io $ mapM_
    (\p -> catchIOError (signalProcess sigTERM p) (\_ -> return ()))
    (getPids pids)
myConfig =
  def
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
