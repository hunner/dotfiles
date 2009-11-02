--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
{-# LANGUAGE NoMonomorphismRestriction #-}



import XMonad
import System.Exit
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Warp(warpToScreen)
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid
import Data.List
import Data.Maybe

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- myTerminal      = "urxvt;ps -U $USER |grep dzen2|awk '{print $1}'|xargs kill -USR1"
--
mterminal      = "urxvtc"
mborderWidth   = 1
mmodMask       = mod4Mask

-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
mworkspaces :: [WorkspaceId]
mworkspaces = map show [1 .. 9 :: Int]

-- Border colors for unfocused and focused windows, respectively.
--
mnormalBorderColor  = "#999999"
mfocusedBorderColor = "#dd0000"

-- Custom keys
--
mkeys = [ ("M-S-n", sendMessage MirrorShrink  ) -- Expand current window
        , ("M-S-t", sendMessage MirrorExpand  ) -- Shrink current window
        , ("M-b"  , withFocused toggleBorder  ) -- Toggle the border of the currently focused window
        , ("M-g"  , warpToCentre >> promptedWs) -- Gridselect to pick windows
        , ("M-S-b", spawn "ps -U hunner|grep dzen2|awk '{print $1}'|xargs kill -USR1") -- Bring dzen to the front

        -- Goes to window or bring up window
        --, ("M-S-g", gotoMenu)
        --, ("M-S-b", bringMenu)
        , ("M-S-g", warpToCentre >> goToSelected gsConfig )

        -- Multimedia
        , ("<XF86AudioPlay>"       , spawn "mpc toggle"       ) -- play/pause mpd
        , ("<XF86AudioStop>"       , spawn "mpc stop"         ) -- stop mpd
        , ("<XF86AudioPrev>"       , spawn "mpc prev"         ) -- prev song
        , ("<XF86AudioNext>"       , spawn "mpc next"         ) -- next song
        , ("<XF86AudioLowerVolume>", spawn "mpc volume -3"    ) -- volume down via custom script
        , ("<XF86AudioRaiseVolume>", spawn "mpc volume +3"    ) -- volume up via custom script
        , ("<XF86AudioMute>"       , spawn "amixer -q -- sset Headphone togglemute") -- toggle mute via custom script

        -- Dynamic workspace commands
        , ("M-S-<Backspace>"       , removeWorkspace)
        , ("M-S-w"                 , selectWorkspace myXPConfig)
        , ("M-S-r"                 , renameWorkspace myXPConfig)
        , ("M-m"                   , withWorkspace myXPConfig (windows . W.shift))
        , ("M-S-m"                 , withWorkspace myXPConfig (windows . copy))
        ]
        -- Don't auto-assign the key shortcuts
        -- ++
        -- zip (map (("M-" ++) . show) [1..9]) (map (withNthWorkspace W.greedyView) [0..])
        -- ++
        -- zip (map (("M-S-" ++) . show) [1..9]) (map (withNthWorkspace W.shift) [0..])
  where -- Make the mouse jump to the middle of the screen for gridselect
        warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5
        promptedWs = wsgrid >>= \x -> whenJust x $ \y -> windows $ W.greedyView y
        wsgrid = gridselect gsConfig =<< gets (map (\x -> (x,x)) . (map W.tag . W.workspaces . windowset))
        --wsgrid = gridselect gsConfig =<< gets (map (\x -> (x,x)) . (map W.tag . uncurry (++) . partition (isJust . W.stack) . W.workspaces . windowset)) -- (map W.tag . W.workspaces . windowset))


{-
[10:28]  dschoepe : gets (map W.tag . W.workspaces . windowset) should work
[10:31]    aavogt : somewhat useful variation on that is:
[10:32]    aavogt : gets $ map W.tag . uncurry (++) . partition (isJust . W.stack) . W.workspaces . windowset
[10:32]    aavogt : to put the populated ones towards the inside
[10:33]    aavogt : unfortunately, it is a bit more awkward to supply those ones with a different color
[10:34]    aavogt : needs imports of Data.List and Data.Maybe
-}

{-
 - Fancy gsConfig
gsConfig = defaultGSConfig { gs_navigate = neiu `M.union` gs_navigate (defaultGSConfig`asTypeOf`gsConfig) }
    where neiu = M.insert (0,xK_space) (const (0,0)) $ M.map (\(x,y) (a,b) -> (x+a,y+b)) $ M.fromList
            [((0,xK_n),(-1,0))
            ,((0,xK_e),(0,1))
            ,((0,xK_i),(1,0))
            ,((0,xK_u),(0,-1))]
-}

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { fgColor = "#dd0000", bgColor = "black", borderColor = "#dd0000" }

gsConfig = defaultGSConfig
   { gs_navigate = M.unions
           [reset
           ,nethackKeys
           ,gs_navigate                              -- get the default navigation bindings
               $ defaultGSConfig `asTypeOf` gsConfig -- needed to fix an ambiguous type variable
           ]
   }
  where addPair (a,b) (x,y) = (a+x,b+y)
        nethackKeys = M.map addPair $ M.fromList
                              [((0,xK_y),(-1,-1))
                              ,((0,xK_u),(1,-1))
                              ,((0,xK_b),(-1,1))
                              ,((0,xK_n),(1,1))
                              ]
        -- jump back to the center with the spacebar, regardless of the current position.
        reset = M.singleton (0,xK_space) (const (0,0))

------------------------------------------------------------------------
-- Layouts:

mlayout = smartBorders Full ||| tiled ||| Mirror tiled ||| simplestFloat
  where
     -- default tiling algorithm partitions the screen into two panes
     --tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     --ratio   = 1/2
     ratio   = toRational (2/(1+sqrt(5)::Double)) -- golden

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
-- > xprop | grep WM_CLASS
--
mmanageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , title =? "VLC media player"   --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Anki"           --> doFloat
    , className =? "Skype"          --> doFloat
    , className =? "googleearth"    --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "mangclient"     --> doFloat
    , className =? "CellWriter"     --> doFloat
    , className =? "Gvba"           --> doFloat
    , className =? "Cellwriter"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-- Whether focus follows the mouse pointer.
mfocusFollowsMouse :: Bool
mfocusFollowsMouse = True

{-
[14:25]  dschoepe : Hunner: http://hpaste.org/fastcgi/hpaste.fcgi/view?id=8798#a8798
[14:25]  dschoepe : that should work in darcs
[14:28]  dschoepe : Hunner: you probably want something like `flip 
                    (/="cellwriterclass") `fmap` className' as shouldFollow
[14:28]  dschoepe : err, without the flip

-- Special window following code
--shouldFollow :: Query Bool
--shouldFollow = className =? "CellWriter"
-}

pickyFocusEventHook e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode   e == notifyNormal
    = whenX (runQuery shouldFollow w) (focus w) >> return (All False)
    | otherwise = return $ All True
    where shouldFollow = (/="Cellwriter") `fmap` className
pickyFocusEventHook _ = return $ All True

-- Run xmonad!
--
main = do
  xmonad $ defaultConfig
    { terminal           = mterminal
    , focusFollowsMouse  = mfocusFollowsMouse
    , borderWidth        = mborderWidth
    , modMask            = mmodMask
    , workspaces         = mworkspaces
    , normalBorderColor  = mnormalBorderColor
    , focusedBorderColor = mfocusedBorderColor
    , layoutHook         = mlayout
    , manageHook         = mmanageHook
    , handleEventHook    = pickyFocusEventHook
    } `additionalKeysP` mkeys
