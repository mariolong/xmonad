import XMonad

import XMonad.Actions.CopyWindow (copyToAll) -- useful for mplayer
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition -- Focus windows
import XMonad.Hooks.ManageDocks -- This module provides tools to automatically manage dock type programs
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.UrgencyHook
-- import XMonad.Layout.Decoration --(defaultTheme)
import XMonad.Layout.Maximize
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing()

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.WorkspaceCompare

import System.Taffybar.Hooks.PagerHints (pagerHints)

main::IO()
main = xmonad
       $ ewmh
       $ pagerHints
       $ desktopConfig
             { workspaces = myWorkspaces
             , modMask    = mod4Mask
             , terminal   = "st"
             , focusFollowsMouse = False
             , borderWidth = 2
             , normalBorderColor = "#3d3d3d"
             , focusedBorderColor = "#f99157"
             , manageHook = myManageHook
                            <+> manageDocks
                            <+> manageSpawn
--                            <+> manageHook desktopConfig
                            <+> composeOne
                                    [ fmap not isDialog -?> doF avoidMaster
                                    , return True       -?> doF W.swapDown
                                    ]
             , layoutHook = myLayout
             , handleEventHook = docksEventHook <+> fullscreenEventHook -- for google chrome fullscreen
             , startupHook = spawnOnce "taffybar"
             } `additionalKeysP` myKeys

myWorkspaces::[String]
myWorkspaces = map show [1 .. 9 :: Int]

myTabTheme::Theme
myTabTheme = def { inactiveColor = "#222222"
                 , inactiveTextColor = "#777777"
                 , inactiveBorderColor = "#222222"
                 , activeColor = "#555555"
                 , activeTextColor = "#ffdb99"
                 , activeBorderColor = "#ffdb99"
                 , decoHeight = 28
                 , fontName = "xft:Source Han Sans:size=12"}

myTabbedBottom = noBorders (tabbedBottom shrinkText myTabTheme)

myLayout = avoidStruts $
           onWorkspace "1" myLayoutEmacs $
           onWorkspace "2" myLayoutTalk $
           onWorkspace "3" myLayoutEmacs $
           onWorkspace "6" myLayoutFM $
           onWorkspace "9" myLayoutTerm
           myLayoutDefault

myLayoutDefault = myTileDefault |||
                  Mirror myTileDefault |||
                  myTabbedBottom

myLayoutEmacs =   myTabbedBottom |||
                  myTileDefault

myLayoutFM = myTileDefault |||
             myTabbedBottom

myLayoutTalk = smartBorders tiled
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 2/7
      delta = 2/100

myLayoutTerm = Mirror (smartBorders tiled) |||
               smartBorders tiled |||
               noBorders simpleTabbedBottom
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 5/7
      delta = 5/100

myTileDefault = smartBorders (Tall nmaster delta ratio)
    where
      nmaster = 1
      ratio = 1/2
      delta = 2/100

myKeys = [ ("M-S-q",     spawnHere "oblogout")
         -- applications
         , ("M-x M-b",   spawnHere "google-chrome-stable")
         , ("M-x M-e",   notifySpawnHere "emacsclient -c -a ''" "Start Emacs")
         , ("M-x M-d",   spawnHere "pcmanfm-qt")
         , ("M-x M-t",   spawnHere "qbittorrent")
         , ("M-x M-v",   spawnHere "gwenview")
         , ("M-x M-f",   spawnHere "st -n mc -e mc")
         , ("M-c c",     spawnHere "emacsclient -ne '(make-capture-frame)'")
         , ("M-x M-m",   spawnHere "st -n ncmpcpp -e ncmpcpp")
         , ("C-<Esc>",   spawnHere "st -n htop -e htop")
         -- music player control
         , ("M-m M-m",   spawn "mpc toggle")
         , ("M-m M-p",   spawn "mpc prev")
         , ("M-m M-n",   spawn "mpc next")
         , ("M-m M-d",   notifySpawn "mpc volume -3" "Volume down")
         , ("M-m M-i",   notifySpawn "mpc volume +3" "Volume up")
         , ("M-m M-h",   spawn "mpc volume 100")
         , ("M-m M-j",   spawn "mpc volume 80")
         , ("M-m M-k",   spawn "mpc volume 60")
         , ("M-m M-l",   spawn "mpc volume 40")
         -- volume control
         , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
         -- calculator
         , ("<XF86Calculator>", spawn "speedcrunch")
         -- recompile & restart xmonad
         , ("M-q",       notifySpawn "xmonad --recompile && xmonad --restart" "Restart XMonad")
         , ("<Print>",   spawn "import -window root ~/Pictures/$(date +%Y%m%d-%H%M%S).png")
         , ("M-<Print>",   spawn "import ~/Pictures/$(date +%Y%m%d-%H%M%S).png")
         -- workspace navigation
         , ("M-f",       moveTo Next EmptyWS)
         , ("M-<R>",     moveTo Next HiddenNonEmptyWS)
         , ("M-<L>",     moveTo Prev HiddenNonEmptyWS)
         -- windown navigation
         , ("M-<U>",     windows W.focusUp)
         , ("M-<D>",     windows W.focusDown)
         -- move focus window to other workspace
         , ("S-M-f",     followTo Next EmptyWS)
         , ("S-M-<D>",   followTo Next HiddenNonEmptyWS)
         , ("S-M-<U>",   followTo Prev HiddenNonEmptyWS)
         -- sticky window
         , ("M-S-s",     windows copyToAll)
         -- left/right switch
         , ("S-M-x",     sendMessage $ Toggle REFLECTX)
         -- up/down switch
         , ("S-M-y",     sendMessage $ Toggle REFLECTY)
         -- switch between current WS and previous WS
         , ("M-z",       toggleWS)
         -- switch fullscreen
         , ("M-\\",      withFocused (sendMessage . maximizeRestore))
         ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloatToAll
    , className =? "mpv" --> doFloatToAll
    , className =? "Oblogout" --> doFullToAll
    , className =? "Speedcrunch" --> doCenterFloat
    , insertPosition Below Newer -- http://lpaste.net/93070
    -- , className =? "google-chrome" --> doShiftAndGo "1"
    , role =? "browser" --> doShiftAndGo "1"
    , appName =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> doShiftAndGo "2"  -- hangout (PM)
    , appName =? "emacs" --> doShiftAndGo "3"
    , appName =? "htop" --> doShiftAndGo "5"
    , appName =? "mc" --> doShiftAndGo "6"
    , appName =? "pcmanfm-qt" --> doShiftAndGo "6"
    , appName =? "Kodi" --> doShiftAndGo "7"
    , appName =? "ncmpcpp" --> doShiftAndGo "7"
    , appName =? "gwenview" --> doShiftAndGo "7"
    , appName =? "qbittorrent" --> doShiftAndGo "8"
    , appName =? "xterm" --> doShiftAndGo "9"
    , appName =? "st-256color" --> doShiftAndGo "9"
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    ]
    where
      doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws -- open on workspace ws then switch to ws
      doFloatToAll = doFloat <+> doF copyToAll -- floating window and copy it to all workspace
      doFullToAll = doFullFloat <+> doF copyToAll
      -- name = stringProperty "WM_NAME"
      role = stringProperty "WM_WINDOW_ROLE"

-- Written by Marshall Lochbaum on xmonad@haskell.org mailing list
followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> (windows (W.shift w)) >> (windows (W.greedyView w)))

-- Section 3 of http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions
-- and, in particular, including the tweak to keep focus with the master window
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) ->  W.Stack r [] (t:rs)
    otherwise -> c

notifySpawnHere::String->String-> X ()
notifySpawnHere c n = do
  safeSpawn "notify-send" [" ", n]
  spawnHere c

notifySpawn::String->String-> X ()
notifySpawn c n = do
  safeSpawn "notify-send" [" ", n]
  spawn c
