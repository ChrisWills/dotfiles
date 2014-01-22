-- ~/.xmonad/xmonad.hs
-- Imports {{{
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Operations
import XMonad.Prompt 
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
--}}}
-- Config {{{
terminal'      = "xterm -e screen"
-- mod4Mask is Windows key, mod1Mask is alt
modMask' :: KeyMask
modMask' = mod4Mask
-- modMask' = mod1Mask 
workspaces'    = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","NSP"]
myStatusBar = "dzen2 -x 0 -y '0' -h '16' -w $((1600 - 326)) -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
--myStatusBar = "dzen2 -x 0 -y '0' -h '16' -w $((1600 - 600)) -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myClockBar = "while true; do date +'%a %b %d %l:%M%p'; sleep 30; done | dzen2 -x $((1600 - 136)) -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myBatteryBar = "while true; do acpi -b | sed -e 's/^Battery 0: //g' ; sleep 30; done | dzen2 -x $((1600 - 600)) -y '0' -h '16' -w '274' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myStartupScript = "/home/cwills/.xmonad/xmonad-startup.sh"
traystart = 1600 - 326
myTray = "/usr/bin/stalonetray --geometry 12x1+" ++ (show traystart) ++ "+0 --max-geometry 12x1+" ++ (show traystart) ++ "+0 --background '#161616' --icon-size 16 --icon-gravity NE --kludges=force_icons_size"
myBitmapsDir = "/home/cwills/.xmonad/dzen"
myUrgencyHook = withUrgencyHook NoUrgencyHook -- Highlight workspace where urgent windows lives 
-- myUrgencyHook = withUrgencyHook FocusHook -- Automatically focus urgent window 
myScreenHack = layoutScreens 3 (fixedLayout [(Rectangle 0 0 1360 768),(Rectangle 1360 0 1280 1024),(Rectangle 2640 0 1280 1024)])
--}}}
-- Scratchpads {{{
scratchpads :: [NamedScratchpad]
scratchpads = [
     NS "volume" "xterm -title Volume -xrm \"XTerm*allowTitleOps:false\" -e alsamixer" (title =? "Volume") 
         (customFloating $ W.RationalRect (0) (1/48) (1/8) (47/48)) ,

     NS "music" "xterm -title Music -xrm \"XTerm*allowTitleOps:false\" -e ncmpc" (title =? "Music")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10)) ,

     NS "term" "xterm -title Scratchpad -xrm \"XTerm*allowTitleOps:false\"" (title =? "Scratchpad")
         (customFloating $ W.RationalRect (1/10) (2/10) (8/10) (6/10)) ,
         
     NS "dict" "artha" (title =? "Artha ~ The Open Thesaurus")
         (customFloating $ W.RationalRect (2/6) (1/6) (2/6) (4/6)) 
 ] 

myNSManageHook :: NamedScratchpads -> ManageHook
myNSManageHook s =
    namedScratchpadManageHook s
    <+> composeOne
            [ title =? "Music" -?> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook)
            , title =? "Scratchpad" -?> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook)
            , title =? "Volume" -?> (ask >>= \w -> liftX (setOpacity w 0.8) >> idHook)
            --, title =? "Artha ~ The Open Thesaurus" -?> (ask >>= \w -> liftX (setOpacity w 0.8) >> idHook)
            ]
---}}}
-- Hooks {{{
manageHook' :: ManageHook
manageHook' = myNSManageHook scratchpads <+> composeAll 
    [ isIgnore      --> doIgnore 
    , isCenterFloat --> doCenterFloat  
    , isNormalFloat --> doFloat  
    , isFullscreen  --> myDoFullFloat                           
    --, isWeb         --> doShift  "3:web"
    --, isDev         --> doShift  "4:dev"
    --, isWine        --> doF(W.shift "6:wine")
    ] 
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        
        myFloatClassNames  = ["Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor","XVroot", "qemu","artha","Artha"]
        myNormalFloatClassNames = ["Gimp","ij-ImageJ"]
        myIgnoreResources = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
        myCenterFloatNames   = ["bashrun","Google Chrome Options","Chromium Options"]
        myWebClassNames    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail"]
        myDevClassNames    = ["Eclipse","eclipse","Netbeans","Gvim"]
        myWineClassNames   = ["Wine"]
       
        isElementOfPropList :: Query String -> [String] -> Query Bool
        isElementOfPropList prop l = prop >>= \s -> return (s `elem` l)

        --isElementOfPropListGen :: [(Query String, [String])] -> Query Bool
        



        isIgnore :: Query Bool
        isIgnore = isElementOfPropList resource myIgnoreResources
       
        isNormalFloat :: Query Bool
        isNormalFloat  = isElementOfPropList className myNormalFloatClassNames
       
        isCenterFloat :: Query Bool
        isCenterFloat = isElementOfPropList name myCenterFloatNames >>= \x ->
                        isElementOfPropList className myFloatClassNames >>= \y ->
                        return (x || y)
        isWeb :: Query Bool
        isWeb = isElementOfPropList className myWebClassNames
        isDev :: Query Bool 
        isDev = isElementOfPropList className myDevClassNames
        isWine :: Query Bool
        isWine = isElementOfPropList className myWineClassNames
        


-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

-- Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorYellow colorDarkGrey . pad
      , ppVisible           =   dzenColor "white" colorDarkGrey . pad
      , ppHidden            =   dzenColor "" "" . pad
      , ppUrgent            =   dzenColor colorMedGrey colorYellow
      , ppHiddenNoWindows   =   dzenColor colorMedGrey colorDarkGrey . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor colorYellow colorDarkGrey .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "SimplestFloat"             ->      "~"
                                    "Grid"                      ->      "^i(" ++ myBitmapsDir ++ "/grid.xbm)"
                                    _                           ->      x
                                )
      , ppSort              = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
      , ppTitle             =   (" " ++) . dzenColor "white" colorDarkGrey . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- Layout
layoutHook' = avoidStruts $ 
    smartBorders tiled
    ||| smartBorders (Mirror tiled)
    ||| noBorders Full
    ||| smartBorders simplestFloat
    ||| smartBorders Grid 
        where tiled   = ResizableTall 1 (2/100) (1/2) []
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGrey        = "#161616"
colorMedGrey         = "#444444"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#ebac54"
colorWhite           = "#cfbfad"
colorLightBlue       = "#afdfff"
colorDarkGrey2       = "#262626"
colorLightGrey       = "#a6a6a6"

colorNormalBorder    = "#262626"
colorFocusedBorder   = "#9F6C3B"

barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}
-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = xftFont
                    , bgColor               = colorDarkGrey
                    , fgColor               = colorLightGrey
                    , bgHLight              = colorLightGrey
                    , fgHLight              = colorDarkGrey
                    , promptBorderWidth     = 0
                    , height                = 20
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 20
                }
-- }}}
-- Key mapping {{{
keys' = \c -> mkKeymap c $
    [   ("M-<Return>", spawn $ XMonad.terminal c)
    ,   ("M-p", spawn "xterm")
    ,   ("M-r", shellPrompt mXPConfig)
    --,   ("M-w", spawn "google-chrome")
    ,   ("M-e", spawn "xterm -e screen -c ~/.screenrc-mail")
    ,   ("M-S-c", kill)
    ,   ("M-<Space>", sendMessage NextLayout)
    --,   ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
    ,   ("M-n", refresh)
    ,   ("M-<Tab>", windows W.focusDown)
    ,   ("M-j", windows W.focusDown)
    ,   ("M-k", windows W.focusUp)
    ,   ("M-m", windows W.focusMaster)
    ,   ("M-S-<Return>", windows W.swapMaster)
    ,   ("M-S-j", windows W.swapDown)
    ,   ("M-S-k", windows W.swapUp)
    ,   ("M-h", sendMessage Shrink)
    ,   ("M-l", sendMessage Expand)
    ,   ("M-S-h", sendMessage MirrorShrink)
    ,   ("M-S-l", sendMessage MirrorExpand)
    ,   ("M-t", withFocused $ windows . W.sink)
    ,   ("M-S-t", sinkAll)
    ,   ("M-<Backspace>", focusUrgent)
    ,   ("M-S-<Backspace>",clearUrgents)
    ,   ("M-,", sendMessage (IncMasterN 1))
    ,   ("M-.", sendMessage (IncMasterN (-1)))
    ,   ("M-b", sendMessage ToggleStruts)
    ,   ("M-S-q", io (exitWith ExitSuccess))
    ,   ("M-q", spawn "/home/cwills/.xmonad/cleanup.sh; xmonad --recompile; xmonad --restart")
    ,   ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ,   ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ,   ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    ,   ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
    ,   ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
--    ,   ("M-<F8>", spawn "xbacklight -dec 5")
--    ,   ("M-<F9>", spawn "xbacklight -inc 5")
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
             zip workspaces' ["1","2","3","4","5","6","7","8","9","0","<F1>","<F2>","<F3>","<F4>","<F5>","<F6>"]]
    ++
    [ ("M-" ++ key, (windows . W.greedyView) name)
         | (name, key) <-
             zip workspaces' ["1","2","3","4","5","6","7","8","9","0","<F1>","<F2>","<F3>","<F4>","<F5>","<F6>"]]
    ++
    [ ("M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . W.view))
    	| (key, scr)  <- zip "dfg" [0,1,2] -- change to match your screen order
    ]
--}}}
-- Main {{{


main = do
    dzenTopBar <- spawnPipe myStatusBar
    spawn myClockBar
    spawn myTray 
    -- spawn "artha" -- moved to scratchpad
    spawn "nm-applet"
    spawn "/home/cwills/bin/batt_stat.rb"
    spawn "xscreensaver"
    spawn "xmodmap /home/cwills/.Xmodmap"
    --spawn "xrdb -merge /home/cwills/.Xdefaults"
    spawn "xset r rate 200 60"
    spawn "feh --bg-fill /home/cwills/.wallpaper/current"
    spawn "xbacklight -set 70"
    xmonad $ myUrgencyHook $ defaultConfig
      { terminal            = terminal'
      , workspaces          = workspaces'
      , keys                = keys'
      , modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "LG3D" 
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenTopBar >> fadeInactiveLogHook 0xdddddddd  >> setWMName "LG3D"
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
}
--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
