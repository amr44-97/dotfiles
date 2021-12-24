
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.Run --(runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.DynamicScratchpads
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders

import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Util.Hacks as Hacks




main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB  (statusBarProp "xmobar ~/.config/xmobar/xmobar-17.hs" (pure def)) defToggleStrutsKey  
     . docks
     $ myConfig


myConfig = def
    { modMask            = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook         = smartBorders $ myLayout  -- Use custom layout	
    , manageHook         = myManageHook <+> namedScratchpadManageHook scratchpads -- Match on certain windows
    , terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    
    -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    ,startupHook        = myStartupHook
    }



myTerminal      = "alacritty --command fish"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 3
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#7651ad"


xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]


myWorkspaces :: [String]       
myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","NSP","8","9"]
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
  where                                                                       
         clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,                                        
                            let n = i ]

--myscratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
--
--               ]
--  where
--    spawnTerm = myTerminal ++ " -n scratchpad"
--    findTerm = resource =? "scratchpad"
--    manageTerm = customFloating $ W.RationalRect l t w h
--      where
--        h = 0.9
--        w = 0.9
--        t = 0.95 -h
--        l = 0.95 -h
--handleEventHook = handleEventHook def <+> Hacks.windowedFullscreenFixEventHook

scratchpads = [
    NS "terminal" spawnTerm findTerm manageTerm]
   
   where 
    spawnTerm = "alacritty --title scratchpad --command fish"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -h


myStartupHook = do 
        spawnOnce "xfce4-power-manager &"
        spawnOnce "nitrogen --restore  &"
        spawnOnce "emacs --daemon &"
        spawnOnce "clipmenud &"
        spawnOnce "setxkbmap -layout us,ara -option grp:lalt_lshift_toggle &"
        spawnOnce "sxhkd -c ~/.config/sxhkd/sxhkd-xmonad &"
        spawnOnce "picom --experimental-backends &"
        spawnOnce "xsetroot -cursor_name left_ptr "

myManageHook :: ManageHook 

myManageHook =  composeAll
    [ className =? "mpv"        --> doFloat
    ,title =? "scratchpad"        --> doFloat
    , className =? "Gimp"           --> doFloat
 -- , title =?    "~ : nnn — Konsole"    --> (customFloating $ W.RationalRect (0.2) (0.1) (0.6) (0.7)) --doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]



myLayout = avoidStruts  tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = spacing 3 $(Tall nmaster delta ratio)
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes



myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppLayout = wrap "(<fc=#e4b63c>" "</fc>)"
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""  
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- additionalKeysP :: XConfig l -> [(String, X ())] -> XConfig l 
-- `additionalKeysP`
--     [ ("M-S-z", spawn "xscreensaver-command -lock")
--     , ("M-S-=", unGrab *> spawn "scrot -s"        )
--     , ("M-]"  , spawn "firefox"                   )
--     ]
-- 

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    , ((modm,               xK_d     ), spawn "nautilus")

    
    , ((modm,               xK_a     ), spawn "konsole -e 'nnn -Re'")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- ranger file manager
    , ((modm .|. shiftMask, xK_a     ), spawn "konsole -e 'ranger'")

    , ((modm .|. controlMask, xK_t), namedScratchpadAction scratchpads "terminal")

    -- launch browser
    , ((modm .|. shiftMask, xK_w     ), spawn "firefox")

    -- Scratchpad
     , ((modm .|. shiftMask .|. controlMask , xK_s),  spawnDynamicSP "scratchpad")

    -- close focused window
    , ((modm,                  xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
     , ((modm,               xK_Tab   ), windows W.focusDown)



    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown )

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp   )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask,           xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask             ,xK_q     ), io (exitWith ExitSuccess))


    -- Emacs
    ,((modm                            ,xK_e     ), spawn "emacs")
    
    -- clipmenu
    ,((modm                            ,xK_o   ), spawn "clipmenu")

    -- Restart xmonad
    , ((modm .|. shiftMask             , xK_r    ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
--    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
   -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


 
