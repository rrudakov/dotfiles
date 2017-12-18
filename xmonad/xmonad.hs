{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}
import           Data.List
import           Data.Monoid
-- import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.PerWindowKbdLayout
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Ssh
import           XMonad.Util.Run                 (spawnPipe)
import           XMonad.Util.SpawnOnce

import qualified Data.Map                        as M
import qualified XMonad.StackSet                 as W

-- | Set default terminal emulator
myTerminal ∷ String
myTerminal = "gnome-terminal"

-- | Set focus follow mouse
myFocusFollowsMouse ∷ Bool
myFocusFollowsMouse = True

-- | Set click just focuses
myClickJustFocuses ∷ Bool
myClickJustFocuses = False

-- | Set border width
myBorderWidth ∷ Dimension
myBorderWidth = 1

-- | Set super key as default modificator
myModMask ∷ KeyMask
myModMask = mod4Mask

-- | My work spaces
myWorkspaces ∷ [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

-- | Set unfocused windows border color
myNormalBorderColor :: String
myNormalBorderColor  = "#282828"

-- | Set focused window border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#928374"

-- | Settings for ssh prompt
myPromptConfig :: XPConfig
myPromptConfig = XPC
  { font                = "xft:Source Code Variable:size=10:bold:antialias=true"
  , bgColor             = "#282828"
  , fgColor             = "#ebdbb2"
  , fgHLight            = "black"
  , bgHLight            = "gray"
  , borderColor         = "#C5C8C6"
  , promptBorderWidth   = 0
  , promptKeymap        = defaultXPKeymap
  , completionKey       = (0, xK_Tab)
  , changeModeKey       = xK_grave
  , position            = Bottom
  , height              = 30
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , autoComplete        = Nothing
  , showCompletionOnTab = False
  , searchPredicate     = isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing
  }

-- | My keybindings
myKeys ∷ XConfig Layout → M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi
    --, ((modm,               xK_p     ), spawn "rofi -show run")
    , ((modm,               xK_p     ), shellPrompt myPromptConfig)

    -- launch ssh prompt
    , ((modm,               xK_s     ), sshPrompt myPromptConfig)

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm .|. shiftMask, xK_x     ), spawn "xlock")

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

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
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "if type /usr/local/bin/xmonad; then /usr/local/bin/xmonad --recompile && /usr/local/bin/xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    --, ((modm              , xK_q     ), spawn "stack exec xmonad -- --recompile && stack exec xmonad -- --restart")

    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm              , xK_BackSpace), focusUrgent)

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    -- , ((0                 , xF86XK_AudioLowerVolume), spawn "amixer -c 0 -q set Master 2dB-")
    -- , ((0                 , xF86XK_AudioRaiseVolume), spawn "amixer -c 0 -q set Master 2dB+")
    -- , ((0                 , xF86XK_AudioMute       ), spawn "amixer -c 0 -q set Master toggle")

    -- , ((0                 , xF86XK_AudioPlay       ), spawn "ncmpcpp toggle")
    -- , ((0                 , xF86XK_AudioNext       ), spawn "ncmpcpp next")
    -- , ((0                 , xF86XK_AudioPrev       ), spawn "ncmpcpp prev")
    -- , ((0                 , xF86XK_AudioStop       ), spawn "ncmpcpp stop")

    -- , ((0                 , xK_Print               ), spawn "scrot '%F_%H%M%S_$wx$h.png' -e 'mv $f ~/screenshots/'")
    , ((0                 , xK_Print               ), spawn "xfce4-screenshooter")
    ]

    `mappend`

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) ← zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) ← [(W.greedyView, 0), (W.shift, shiftMask)]]

    `mappend`

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) ← zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) ← [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- | Mouse bindings: default actions bound to mouse events
myMouseBindings ∷ XConfig Layout → M.Map (KeyMask, Button) (Window → X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \ w → focus w >> mouseMoveWindow w
                              >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \ w → focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \ w → focus w >> mouseResizeWindow w
                              >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- | Set layouts
myLayout ∷ ModifiedLayout
  AvoidStruts
  (Choose
    (ModifiedLayout (ConfigurableBorder Ambiguity) Tall)
    (Choose
      (ModifiedLayout WithBorder Full)
      (ModifiedLayout (ConfigurableBorder Ambiguity) Grid)))
  Window
myLayout = avoidStruts $ lessBorders Screen tiled ||| noBorders Full ||| lessBorders Screen Grid
    where
      tiled = Tall 1 (3/100) (1/2)

-- | Set hooks for applications
myManageHook ∷ Query (Endo WindowSet)
myManageHook = composeAll [ className =? "Firefox"        --> doShift "5"
                          , className =? "Thunderbird"    --> doShift "3"
                          , className =? "Skype"          --> doShift "7"
                          , className =? "Google-chrome"  --> doShift "2"
                          , className =? "MPlayer"        --> doFloat
                          , className =? "Gimp"           --> doFloat
                          , resource  =? "desktop_window" --> doIgnore
                          , resource  =? "kdesktop"       --> doIgnore
                          , className =? "xfce4-notifyd"  --> doIgnore
                          , className =? "rdesktop"       --> doFullFloat
                          , className =? "Nm-openconnect-auth-dialog" --> doCenterFloat
                          ]

-- | Fullscreen flash
flashHook ∷ ManageHook
flashHook = composeOne [
                isFullscreen -?> doFullFloat
               ]

myEventHook ∷ Event → X Data.Monoid.All
myEventHook = docksEventHook

-- | Make java GUI working
myStartupHook ∷ X ()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "compton --backend glx --vsync opengl-swc -b"
  spawnOnce "stalonetray"
  spawnOnce "nm-applet"
  spawnOnce "xfce4-power-manager"
  spawnOnce "nitrogen --restore"
  spawnOnce "xgamma -rgamma 1 -ggamma 1 -bgamma 0.8"

------------------------------------------------------------------------
-- Event Masks:

-- | The client events that xmonad is interested in
myClientMask ∷ EventMask
myClientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

-- | The root events that xmonad is interested in
myRootMask ∷ EventMask
myRootMask =
  substructureRedirectMask .|.
  substructureNotifyMask .|.
  enterWindowMask .|.
  leaveWindowMask .|.
  structureNotifyMask .|.
  buttonPressMask

------------------------------------------------------------------------

-- | Run xmonad with xmobar
main ∷ IO ()
main = do
  xmproc ← spawnPipe "$HOME/.local/bin/xmobar"
  launch $ ewmh $ withUrgencyHook NoUrgencyHook
  -- launch $ withUrgencyHook NoUrgencyHook
         XConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      clickJustFocuses   = myClickJustFocuses,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      rootMask           = myRootMask,
      clientMask         = myClientMask,

      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      manageHook         = myManageHook <+> flashHook <+> manageDocks,
      layoutHook         = myLayout,
      handleEventHook    = myEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook <+> perWindowKbdLayout,
      logHook            = dynamicLogWithPP xmobarPP { ppOutput          = hPutStrLn xmproc
                                                     , ppCurrent         = xmobarColor "#b16286" "#3c3836" . wrap " " " "
                                                     , ppTitle           = xmobarColor "#d79921" "" . shorten 60
                                                     , ppHidden          = xmobarColor "#ebdbb2" ""
                                                     , ppHiddenNoWindows = xmobarColor "#504945" ""
                                                     , ppUrgent          = xmobarColor "#fabd2f" "#fb4934" . wrap " " " "
                                                     , ppSep             = " "
                                                     , ppLayout          = xmobarColor "#ebdbb2" "" },
      startupHook        = myStartupHook,
      handleExtraArgs    = \ xs theConf → case xs of
                                            [] → return theConf
                                            _ → fail ("unrecognized flags:" `mappend` show xs)
       }
