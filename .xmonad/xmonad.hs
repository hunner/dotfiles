--
-- xmonad config file.
--
{-# LANGUAGE NoMonomorphismRestriction #-}


import XMonad hiding (Tall)
import System.Exit
import XMonad.Layout.Circle
import XMonad.Layout.HintedTile
import XMonad.Layout.MagicFocus
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp(warpToScreen)
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import Monad
import Data.Monoid
import Data.List
import Data.Maybe

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- mTerminal      = "urxvt;ps -U $USER |grep dzen2|awk '{print $1}'|xargs kill -USR1"
mTerminal      = "urxvtc"
mBorderWidth   = 1
mModMask       = mod4Mask

-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
mWorkspaces :: [WorkspaceId]
mWorkspaces = map show [0 .. 9 :: Int]

-- Border colors for unfocused and focused windows, respectively.
--
mNormalBorderColor  = "#999999"
mFocusedBorderColor = "#dd0000"

-- Custom keys
--
mKeys = [ ("M-S-n", sendMessage MirrorShrink  ) -- Expand current window
        , ("M-S-t", sendMessage MirrorExpand  ) -- Shrink current window
        , ("M-r"  , warpToCorner              ) -- Kill the rodent
        , ("M-b"  , withFocused toggleBorder  ) -- Toggle the border of the currently focused window
        , ("M-g"  , warpToCentre >> promptedWs) -- Gridselect to pick windows
        --, ("M-s"  , shellPromptHere sp mXPConfig ) -- Shell prompt
        , ("M-S-b", spawn "ps -U hunner|grep dzen2|awk '{print $1}'|xargs kill -USR1") -- Bring dzen to the front
        , ("<Scroll_lock>", spawn "xlock -mode fzort" ) -- SCReen LocK

        -- Sticky/unsticky windows (does not work on workspaces created after the fact)
        , ("M-a"  , windows copyToAll)  -- Copy focused window to all workspaces
        , ("M-S-a", killAllOtherCopies) -- Uncopy focused window from all workspaces

        -- Goes to window or bring up window
        --, ("M-S-g", gotoMenu)
        --, ("M-S-b", bringMenu)
        , ("M-S-g", warpToCentre >> goToSelected gsConfig )

        -- Multimedia
        , ("<XF86AudioPlay>"       , spawn "cmus-remote --pause" ) -- play/pause mpd
        , ("<XF86AudioStop>"       , spawn "cmus-remote --stop"  ) -- stop mpd
        , ("<XF86AudioPrev>"       , spawn "cmus-remote --prev"  ) -- prev song
        , ("<XF86AudioNext>"       , spawn "cmus-remote --next"  ) -- next song
        , ("<XF86AudioLowerVolume>", spawn "amixer -q set PCM 1-") -- volume down
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q set PCM 1+") -- volume up
        , ("<XF86AudioMute>"       , spawn "amixer -q set Headphone toggle") -- toggle mute
        , ("M-<XF86AudioMute>"     , spawn "amixer -q set Speaker toggle")

        -- Dynamic workspace commands
        , ("M-S-<Backspace>"       , removeWorkspace)
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
mXPConfig = defaultXPConfig { fgColor = "#dd0000", bgColor = "black", borderColor = "#dd0000" }

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
-- Fullscreen hack from http://code.google.com/p/xmonad/issues/detail?id=339

-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win
  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2
      -- The ATOM property type for changeProperty
      ptype = 4 
      action = head dat
  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win
  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False
evHook _ = return $ All True

------------------------------------------------------------------------
-- Layouts:

--mLayout = smartBorders Full ||| tiled ||| hintedTile Wide ||| simplestFloat ||| Circle ||| magnifier Circle
mLayout = smartBorders Full ||| tiled ||| Mirror tiled ||| simplestFloat ||| Circle ||| magnifier Circle
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
    , className =? "Cellwriter"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen                  --> doFullFloat ]

-- Whether focus follows the mouse pointer.
mFocusFollowsMouse :: Bool
mFocusFollowsMouse = True

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
mConfig = defaultConfig
  { terminal           = mTerminal
  , focusFollowsMouse  = mFocusFollowsMouse
  , borderWidth        = mBorderWidth
  , modMask            = mModMask
  , workspaces         = mWorkspaces
  , normalBorderColor  = mNormalBorderColor
  , focusedBorderColor = mFocusedBorderColor
  , layoutHook         = mLayout
  --, manageHook         = manageSpawn sp <+> mManageHook
  , manageHook         = mManageHook
  , handleEventHook    = pickyFocusEventHook >> evHook
  , startupHook        = do
      ewmhDesktopsStartup >> setWMName "LG3D"
      return () >> checkKeymap mConfig mKeys
  } `additionalKeysP` mKeys `additionalKeys` mKeysExt

-- Run xmonad!
--
main = do
    sp <- mkSpawner
    xmonad mConfig
