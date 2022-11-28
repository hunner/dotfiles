--
-- xmonad config file.
--
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


import XMonad hiding (Tall)
import System.Exit
import XMonad.Layout.Circle
import XMonad.Layout.HintedTile
import XMonad.Layout.MagicFocus
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy, copyToAll, killAllOtherCopies)
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp(warpToScreen)
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Paste as P
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import Control.Monad (when)
import Data.Monoid
import Data.List
import Data.Maybe

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- mTerminal      = "urxvt;ps -U $USER |grep dzen2|awk '{print $1}'|xargs kill -USR1"
mTerminal      = "alacritty"
mBorderWidth   = 2
mModMask       = mod4Mask

-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
mWorkspaces :: [WorkspaceId]
mWorkspaces = map show [0 .. 9 :: Int]

-- Border colors for unfocused and focused windows, respectively.
--
mNormalBorderColor  = "#222222"
--mFocusedBorderColor = "#005577"
mFocusedBorderColor = "#88c0d0"

-- Custom keys
--
mKeys = [ ("M-S-n"   , sendMessage MirrorShrink  ) -- Expand current window
        , ("M-S-t"   , sendMessage MirrorExpand  ) -- Shrink current window
        , ("M-S-M1-<Tab>", P.sendKey (controlMask .|. shiftMask .|. mod1Mask .|. mod2Mask ) xK_Tab )
        , ("M-r"     , warpToCorner              ) -- Kill the rodent
        , ("M-b"     , withFocused toggleBorder  ) -- Toggle the border of the currently focused window
        , ("M-g"     , warpToCentre >> promptedWs) -- Gridselect to pick windows
        --, ("M-s"     , shellPromptHere sp mXPConfig ) -- Shell prompt
        , ("M-M1-C-8", spawn "xcalib -i -a"      ) -- Invert screen color
        , ("M-S-b"   , spawn "restart_battery.sh") -- Bring dzen to the front
        , ("M-p"     , spawn "rofi -show run"    ) -- Run rofi
        , ("M-C-<Space>"     , spawn "/home/hunner/local/bin/emoji-menu")
        , ("M-C-c"   , spawn "CM_LAUNCHER=rofi CM_DIR=~/.config/clipmenu clipmenu")
        , ("M-S-c"   , spawn "CM_LAUNCHER=rofi CM_DIR=~/.config/clipmenu clipmenu") -- Rebind to avoid closing windows
        , ("M-C-S-c" , withFocused killWindow    ) -- make it harder to do accidentally
        , ("<Scroll_lock>", spawn "xlock -mode fzort -echokeys -usefirst" ) -- SCReen LocK

        -- Sticky/unsticky windows (does not work on workspaces created after the fact)
        , ("M-a"  , windows copyToAll)  -- Copy focused window to all workspaces
        , ("M-S-a", killAllOtherCopies) -- Uncopy focused window from all workspaces

        -- Goes to window or bring up window
        --, ("M-S-g", gotoMenu)
        --, ("M-S-b", bringMenu)
        --, ("M-S-g", warpToCentre >> goToSelected gsConfig )
        , ("M-S-g", spawn "rofi -show window")

        -- Multimedia
        , ("<XF86MonBrightnessUp>"   , spawn "xbacklight -inc 10"            ) -- Brightness up
        , ("<XF86MonBrightnessDown>" , spawn "xbacklight -dec 10"            ) -- Brightness down
        , ("<XF86AudioPlay>"         , spawn "mpc toggle"                    ) -- play/pause mpd
        , ("<XF86AudioStop>"         , spawn "mpc stop"                      ) -- stop mpd
        , ("<XF86AudioPrev>"         , spawn "mpc prev"                      ) -- prev song
        , ("<XF86AudioNext>"         , spawn "mpc next"                      ) -- next song
        , ("<XF86AudioLowerVolume>"  , spawn "pamixer --decrease 4"      ) -- volume down
        , ("<XF86AudioRaiseVolume>"  , spawn "pamixer --increase 4"      ) -- volume up
        , ("<XF86AudioMute>"         , spawn "pamixer --toggle-mute") -- toggle mute
        , ("M-<XF86AudioMute>"       , spawn "amixer -q set Speaker toggle"  )

        -- Dynamic workspace commands
        --, ("M-S-<Backspace>"       , removeWorkspace)
        , ("M-S-w"                 , selectWorkspace mXPConfig)
        , ("M-S-r"                 , renameWorkspace mXPConfig)
        , ("M-m"                   , withWorkspace mXPConfig (windows . W.shift))
        , ("M-S-m"                 , withWorkspace mXPConfig (windows . copy))
        -- todo: make a better command to "move" (rename) a whole workspace to a different name
        --       if src is [1..9] then create a blank one after renaming
        --       if there is a workspace existing target name that is empty, delete it before moving
        --       else leave everything as it is (no move/rename)
        ]
        ++ -- mod-[1..9] %! Switch to workspace N if it exists
        zip (map (("M-" ++) . show)   [0..9]) (map (windows . W.greedyView) (workspaces mConfig))
        ++ -- mod-shift-[1..9] %! Move window to workspace N if it exists
        zip (map (("M-S-" ++) . show) [0..9]) (map (windows . W.shift)      (workspaces mConfig))
        -- Don't auto-assign the key shortcuts to dynamic workspaces. I want them staying on [1..9] only
        -- ++ -- mod-[1..9] %! Switch to Nth workspace that exists in alphabetical order
        -- zip (map (("M-" ++) . show)   [0..9]) (map (withNthWorkspace W.greedyView) [0..])
        -- ++ -- mod-[1..9] %! Move window to Nth workspace that exists in alphabetical order
        -- zip (map (("M-S-" ++) . show) [0..9]) (map (withNthWorkspace W.shift) [0..])
        ++ -- mod-{o,e,u} %! Switch to physical/Xinerama screens 0, 1, or 2
        zip (map ("M-" ++)   ["o","e","u"]) (map (\x -> screenWorkspace x >>= flip whenJust (windows . W.view))  [0..])
        ++ -- mod-shift-{o,e,u} %! Move client to screen 0, 1, or 2
        zip (map ("M-S-" ++) ["o","e","u"]) (map (\x -> screenWorkspace x >>= flip whenJust (windows . W.shift)) [0..])
  where -- Make the mouse jump to the middle of the screen for gridselect
        warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5
        warpToCorner = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  1.0 1.0
        promptedWs = wsgrid >>= \x -> whenJust x $ \y -> windows $ W.greedyView y
        wsgrid = gridselect gsConfig =<< gets (map (\x -> (x,x)) . (map W.tag . W.workspaces . windowset))
        --wsgrid = gridselect gsConfig =<< gets (map (\x -> (x,x)) . (map W.tag . uncurry (++) . partition (isJust . W.stack) . W.workspaces . windowset)) -- (map W.tag . W.workspaces . windowset))

mKeysExt = [((m .|. mModMask, k), f i) -- changing workspaces with bébo
             | (i, k) <- zip ([0..]) [0x2a,0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f]
             , (f, m) <- [(withNthWorkspace W.greedyView, 0), (withNthWorkspace W.shift, shiftMask)]]
{-
[10:28]  dschoepe : gets (map W.tag . W.workspaces . windowset) should work
[10:31]    aavogt : somewhat useful variation on that is:
[10:32]    aavogt : gets $ map W.tag . uncurry (++) . partition (isJust . W.stack) . W.workspaces . windowset
[10:32]    aavogt : to put the populated ones towards the inside
[10:33]    aavogt : unfortunately, it is a bit more awkward to supply those ones with a different color
[10:34]    aavogt : needs imports of Data.List and Data.Maybe
-}

mXPConfig :: XPConfig
mXPConfig = def { fgColor = "#005577", bgColor = "#222222", borderColor = "#005577" }

{-
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
-}

gsConfig = def

------------------------------------------------------------------------
-- Layouts:

--mLayout = smartBorders Full ||| tiled ||| hintedTile Wide ||| simplestFloat ||| Circle ||| magnifier Circle
mLayout = Mirror tiled ||| tiled ||| smartBorders Full ||| Circle
  where
     -- default tiling algorithm partitions the screen into two panes
     --tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall nmaster delta ratio []
     --hintedTile = HintedTile nmaster delta ratio TopLeft
     --tiled      = hintedTile Tall

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
mManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , title =? "VLC media player"   --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Anki"           --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "XClock"         --> doFloat
    , className =? "Skype"          --> doFloat
    , className =? "googleearth"    --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "mangclient"     --> doFloat
    , className =? "CellWriter"     --> doFloat
    , className =? "Gvba"           --> doFloat
    , className =? "Thunar"         --> doFloat
    , className =? "feh"            --> doFloat
    , className =? "emoji-keyboard" --> doFloat
    , title     =? "Talon Draft"    --> doFloat
    , title     =? "Plover"         --> doFloat
    , className =? "Cellwriter"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen                  --> doFullFloat ]

------------------------------------------------------------------------
-- Whether focus follows the mouse pointer.
--
mFocusFollowsMouse :: Bool
mFocusFollowsMouse = True

-- Mouse bindings: default actions bound to mouse events
--
mMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm              , button1), (\w -> focus w >> mouseMoveWindow w
                                                     >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm              , button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-shift-button1, Set the window to floating mode and resize by dragging
    , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w
                                                     >> windows W.shiftMaster))

    -- button4: 2-swipe up
    -- button5: 2-swipe down
    -- button6: 2-swipe left
    -- button7: 2-swipe right
    -- button8: 3-swipe up
    -- button9: 3-swipe down
    -- button10: 3-swipe left
    -- button11: 3-swipe right
    -- button12: scale in
    -- button13: scale out
    -- button14: rotate left
    -- button15: rotate right
    ]
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

-- Define my configuration
--
mConfig = ewmh def
  { terminal           = mTerminal
  , focusFollowsMouse  = mFocusFollowsMouse
  , borderWidth        = mBorderWidth
  , modMask            = mModMask
  , workspaces         = mWorkspaces
  , mouseBindings      = mMouseBindings
  , normalBorderColor  = mNormalBorderColor
  , focusedBorderColor = mFocusedBorderColor
  , layoutHook         = mLayout
  --, manageHook         = manageSpawn sp <+> mManageHook
  , manageHook         = mManageHook
  , handleEventHook    = handleEventHook def <+> pickyFocusEventHook
  , startupHook        = do
      setWMName "LG3D"
      return () >> checkKeymap mConfig mKeys
  } `additionalKeysP` mKeys `additionalKeys` mKeysExt

-- Run xmonad!
--
main = xmonad $ mConfig
