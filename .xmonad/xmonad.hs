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
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Warp(warpToScreen)
import XMonad.Actions.WindowBringer
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal      = "urxvt;ps -U $USER |grep dzen2|awk '{print $1}'|xargs kill -USR1"
myTerminal      = "urxvtc"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- | The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#999999"
myFocusedBorderColor = "#dd0000"

-- Custom keys
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
myKeys =
    -- Window management
    [ ((modMask .|. shiftMask, xK_n     ), sendMessage MirrorShrink) -- %! Expand current window
    , ((modMask .|. shiftMask, xK_t     ), sendMessage MirrorExpand) -- %! Shrink current window

    -- Bring dzen to the front
    , ("M-S-b", spawn "ps -U hunner|grep dzen2|awk '{print $1}'|xargs kill -USR1")

    -- Toggle the border of the currently focused window
    , ((modMask              , xK_b     ), withFocused toggleBorder)

    -- Gridselect to pick windows
    --, ((modMask              , xK_g     ), goToSelected defaultGSConfig)
    --, ((modMask              , xK_g     ), goToSelected gsconfig3)
    , ((modMask               , xK_a      ), warpToCentre >> wsgrid)

    -- Goes to window or bring up window
    , ((modMask .|. shiftMask, xK_g     ), gotoMenu)
    , ((modMask .|. shiftMask, xK_b     ), bringMenu)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

{-
[10:28]  dschoepe : gets (map W.tag . W.workspaces . windowset) should work
[10:28]    aavogt : dschoepe: yeah
[10:31]    aavogt : somewhat useful variation on that is:
[10:32]    aavogt : gets $ map W.tag . uncurry (++) . partition (isJust . W.stack) . W.workspaces . windowset
[10:32]    aavogt : to put the populated ones towards the inside
[10:33]    aavogt : unfortunately, it is a bit more awkward to supply those ones with a different color
[10:34]    aavogt : needs imports of Data.List and Data.Maybe
-}

warpToCentre = gets (W.screen . map W.tag . W.workspaces . windowset) >>= \x -> warpToScreen x  0.5 0.5

wsgrid = gridselect gsConfig =<< asks (map (\x -> (x,x)) . workspaces . config)
--promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)
--promptedShift = wsgrid >>= \x -> whenJust x $ \y -> windows (W.greedyView y . W.shift y)

{-
-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWorkspace :: GSConfig W.Workspace -> X (Maybe W.Workspace)
gridselectWorkspace gsconf = workspaceMap >>= gridselect gsconf

workspaceMap :: X [(String,W.Workspace)]
workspaceMap = do
    ws <- gets workspaceset
    wins <- mapM keyValuePair (W.workspaces ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: W.workspace -> X String
decorateName' w = do
  fmap show $ getName w
-}

{-
gsConfig = defaultGSConfig { gs_navigate = neiu `M.union` gs_navigate (defaultGSConfig`asTypeOf`gsConfig) }
    where neiu = M.insert (0,xK_space) (const (0,0)) $ M.map (\(x,y) (a,b) -> (x+a,y+b)) $ M.fromList
            [((0,xK_n),(-1,0))
            ,((0,xK_e),(0,1))
            ,((0,xK_i),(1,0))
            ,((0,xK_u),(0,-1))]
-}

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
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = smartBorders Full ||| tiled ||| Mirror tiled ||| simplestFloat
  where
     -- default tiling algorithm partitions the screen into two panes
     --tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     --ratio   = toRational (2/(1+sqrt(5)::Double)) -- golden

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
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
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

{-
[14:25]  dschoepe : Hunner: http://hpaste.org/fastcgi/hpaste.fcgi/view?id=8798#a8798
[14:25]  dschoepe : that should work in darcs
[14:27]    Hunner : awesome. I'll check it
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
main = xmonad defaults

defaults = defaultConfig
    { -- simple stuff
    , terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    , -- key bindings
    , mouseBindings      = myMouseBindings

    , -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = pickyFocusEventHook
    } `additionalKeysP` myKeys
