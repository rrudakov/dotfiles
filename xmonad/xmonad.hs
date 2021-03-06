{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
import           Data.List
import qualified Data.Map                        as M
import           Data.Monoid
-- import           Control.Monad (liftM2)
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.PerWindowKbdLayout
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import           XMonad.Prompt.Ssh
import qualified XMonad.StackSet                 as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Actions.UpdatePointer

-- | Set default terminal emulator
myTerminal :: String
myTerminal = "alacritty"

-- | Set focus follow mouse
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- | Set click just focuses
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- | Set border width
myBorderWidth :: Dimension
myBorderWidth = 1

-- | Set super key as default modificator
myModMask :: KeyMask
myModMask = mod4Mask

-- | My work spaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

-- | Set unfocused windows border color
myNormalBorderColor :: String
myNormalBorderColor  = "#bebebe"

-- | Set focused window border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#f5f5f5"

-- | Spotify play toggle command
spotifyPlayToggle :: String
spotifyPlayToggle =
  mconcat
    [ "dbus-send "
    , "--print-reply "
    , "--dest=org.mpris.MediaPlayer2.spotify "
    , "/org/mpris/MediaPlayer2 "
    , "org.mpris.MediaPlayer2.Player.PlayPause"
    ]

-- | Spotify next song command
spotifyNext :: String
spotifyNext =
  mconcat
    [ "dbus-send "
    , "--print-reply "
    , "--dest=org.mpris.MediaPlayer2.spotify "
    , "/org/mpris/MediaPlayer2 "
    , "org.mpris.MediaPlayer2.Player.Next"
    ]

-- | Spotify previous song command
spotifyPrevious :: String
spotifyPrevious =
  mconcat
    [ "dbus-send "
    , "--print-reply "
    , "--dest=org.mpris.MediaPlayer2.spotify "
    , "/org/mpris/MediaPlayer2 "
    , "org.mpris.MediaPlayer2.Player.Previous"
    ]

-- | Spotify stop command
spotifyStop :: String
spotifyStop =
  mconcat
    [ "dbus-send "
    , "--print-reply "
    , "--dest=org.mpris.MediaPlayer2.spotify "
    , "/org/mpris/MediaPlayer2 "
    , "org.mpris.MediaPlayer2.Player.Stop"
    ]

-- | Settings for ssh prompt
myPromptConfig :: XPConfig
myPromptConfig =
  XPC
    { font = "xft:Source Code Variable:size=8:semibold:antialias=true"
    , bgColor = "#000000"
    , fgColor = "#ffffff"
    , fgHLight = "black"
    , bgHLight = "gray"
    , borderColor = "#ffffff"
    , promptBorderWidth = 0
    , promptKeymap = defaultXPKeymap
    , completionKey = (0, xK_Tab)
    , changeModeKey = xK_grave
    , position = Bottom
    , height = 30
    , historySize = 256
    , historyFilter = id
    , defaultText = []
    , autoComplete = Nothing
    , showCompletionOnTab = False
    -- , complCaseSensitivity = ComplCaseSensitive False
    , searchPredicate = isPrefixOf
    , alwaysHighlight = True
    , maxComplRows = Nothing
    , defaultPrompter = id
    , sorter = const id
    }

-- | My keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch command prompt
  -- , ((modm, xK_p), shellPrompt myPromptConfig)
  , ((modm, xK_p), spawn "rofi -show run")
    -- launch emacs client with new frame
  , ((modm .|. shiftMask, xK_u), spawn "emacsclient -c")
    -- restart emacs daemon
  , ((modm .|. shiftMask, xK_g), spawn "systemctl --user restart emacs.service && notify-send \"Emacs has been restarted\"")
    -- launch emacs anywhere command
  , ((modm, xK_f), spawn "~/.emacs_anywhere/bin/run")
    -- launch ssh prompt
  , ((modm, xK_s), sshPrompt myPromptConfig)
    -- close focused window
  , ((modm .|. shiftMask, xK_c), kill)
     -- Rotate through the available layout algorithms
  , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- , ((modm .|. shiftMask, xK_x), spawn "light-locker-command -l")
    -- Resize viewed windows to the correct size
  , ((modm, xK_n), refresh)
    -- Move focus to the next window
  , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Scratchpads
  , ((modm .|. shiftMask, xK_t), namedScratchpadAction scratchpads "telegram")
  , ((modm .|. shiftMask, xK_m), namedScratchpadAction scratchpads "spotify")
  , ((modm .|. shiftMask, xK_s), namedScratchpadAction scratchpads "slack")
  , ((modm, xK_u), namedScratchpadAction scratchpads "wire")
  -- , ((modm .|. shiftMask, xK_b), namedScratchpadAction scratchpads "skype")
    -- Toggle copy to all workspaces
  , ((modm, xK_a), toggleCopyToAll)
    -- Org capture
  , ((modm, xK_c), spawn "emacsclient -ne \"(make-capture-frame)\"")
    -- Quit xmonad
  , ((modm .|. shiftMask, xK_q), io exitSuccess)
    -- Restart xmonad
  , ( (modm, xK_q)
    , spawn $
      mconcat
        [ "if type xmonad; then "
        , "xmonad --recompile && xmonad --restart; "
        , "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
        ])
  , ((modm, xK_b), sendMessage ToggleStruts)
  , ((modm, xK_BackSpace), focusUrgent)
  , ( (0, xF86XK_AudioLowerVolume)
    , spawn
        "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ( (0, xF86XK_AudioRaiseVolume)
    , spawn
        "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ +5%")
  , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ((0, xF86XK_AudioPlay), spawn spotifyPlayToggle)
  , ((0, xF86XK_AudioNext), spawn spotifyNext)
  , ((0, xF86XK_AudioPrev), spawn spotifyPrevious)
  , ((0, xF86XK_AudioStop), spawn spotifyStop)
    -- Run screenshoter
  , ((0, xK_Print), spawn "flameshot gui")
  ] <>
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] <>
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
  [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  where
    toggleCopyToAll =
      wsContainingCopies >>= \case
        [] -> windows copyToAll
        _ -> killAllOtherCopies


------------------------------------------------------------------------
-- | Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- | Set layouts
myLayout ::
  ModifiedLayout
    Rename
    (ModifiedLayout
       Spacing
       (ModifiedLayout
          AvoidStruts
          (Choose
             (ModifiedLayout
                (ConfigurableBorder Ambiguity) (ModifiedLayout Rename Tall))
             (Choose
                (ModifiedLayout WithBorder (ModifiedLayout Rename Full))
                (ModifiedLayout
                   (ConfigurableBorder Ambiguity) (ModifiedLayout Rename Grid))))))
    Window
myLayout =
  renamed [CutWordsLeft 1] $
  spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
  avoidStruts $
  lessBorders Screen tiled ||| noBorders full ||| lessBorders Screen grid
  where
    tiled = renamed [Replace "[T]"] $ Tall 1 (2 / 100) (1 / 2)
    full  = renamed [Replace "[F]"] Full
    grid  = renamed [Replace "[G]"] Grid

-- |Scratchpads definitions
-- RationalRect arguments
-- From left, From top, width, heigh
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "telegram"
      "telegram-desktop"
      (className =? "TelegramDesktop")
      (customFloating $ W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  , NS
      "spotify"
      "spotify"
      (className =? "Spotify")
      (customFloating $ W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  , NS
      "wire"
      "wire-desktop"
      (appName =? "wire")
      (customFloating $ W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  , NS
      "slack"
      "slack"
      (appName =? "slack")
      (customFloating $ W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  -- , NS
  --     "skype"
  --     "skypeforlinux"
  --     (className =? "Skype")
  --     (customFloating $ W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  ]

-- | Set hooks for applications
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
  [ className =? "firefox" --> doShift "2"
  , className =? "Google-chrome" --> doShift "5"
  , className =? "MPlayer" --> doFloat
  , resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  , className =? "xfce4-notifyd" --> doIgnore
  , className =? "mpv" --> doFloat
  , className =? "rdesktop" --> doFullFloat
  , title =? "Media viewer" --> doFullFloat
  -- , (className =? "Microsoft Teams - Preview" <&&> stringProperty "WM_NAME" =? "Microsoft Teams Notification") --> doFloat
  -- , resource =? "microsoft teams - preview" --> doFloat
  , className =? "Nm-openconnect-auth-dialog" --> doCenterFloat
  , isFullscreen --> doFullFloat
  , isDialog --> doFloat
  , title =? "Helm" -->
    customFloating (W.RationalRect (1 / 5) (1 / 5) (3 / 5) (3 / 5))
  , title =? "capture" -->
    customFloating (W.RationalRect (1 / 5) (1 / 5) (3 / 5) (3 / 5))
  , className =? "vlc" -->
    customFloating (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
    -- scratchpads
  , namedScratchpadManageHook scratchpads
  ]
  -- where viewShift = doF . liftM2 (.) W.greedyView W.shift

-- | Set hooks for windows with dynamic properties
myDynHook :: ManageHook
myDynHook =
  composeAll
  [ className =? "Spotify" --> customFloating (W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  , title =? "*Emacs Anywhere* @ Emacs" --> customFloating (W.RationalRect (1 / 10) (1 / 8) (4 / 5) (3 / 4))
  , title =? "Unlock Keyring" --> doCenterFloat
  ]

------------------------------------------------------------------------
-- Event Masks:

-- | The client events that xmonad is interested in
myClientMask :: EventMask
myClientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

-- | The root events that xmonad is interested in
myRootMask :: EventMask
myRootMask =
  substructureRedirectMask .|.
  substructureNotifyMask .|.
  enterWindowMask .|.
  leaveWindowMask .|.
  structureNotifyMask .|.
  buttonPressMask

------------------------------------------------------------------------

-- | Run xmonad
main :: IO ()
main = do
  -- dirs <- getDirs
  -- launch myConfig dirs
  launch myConfig

myPP :: PP
myPP =
  namedScratchpadFilterOutWorkspacePP $
  -- filterOutWsPP [scratchpadWorkspaceTag] $
  xmobarPP { ppCurrent = xmobarColor "#f8dec0" "#382f27" . wrap " " " "
           , ppTitle = xmobarColor "#a8a8a8" "" . shorten 60
           , ppHidden = xmobarColor "#f8dec0" ""
           , ppHiddenNoWindows = xmobarColor "#392a48" ""
           , ppUrgent = xmobarColor "#000000" "#ff8059" . wrap " " " "
           , ppSep = "  "
           , ppLayout = xmobarColor "#ffffff" ""
           }

toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask'} = (modMask', xK_b)

myConfig ::
  XConfig
    (ModifiedLayout
       Rename
       (ModifiedLayout
          Spacing
          (ModifiedLayout
             AvoidStruts
             (Choose
                (ModifiedLayout
                   (ConfigurableBorder Ambiguity) (ModifiedLayout Rename Tall))
                (Choose
                   (ModifiedLayout WithBorder (ModifiedLayout Rename Full))
                   (ModifiedLayout
                      (ConfigurableBorder Ambiguity) (ModifiedLayout Rename Grid)))))))
myConfig =
  ewmh $
  docks $
  withUrgencyHook
    NoUrgencyHook
    def
      { terminal = myTerminal
      , focusFollowsMouse = myFocusFollowsMouse
      , clickJustFocuses = myClickJustFocuses
      , borderWidth = myBorderWidth
      , modMask = myModMask
      , workspaces = myWorkspaces
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , rootMask = myRootMask
      , clientMask = myClientMask
        -- key bindings
      , keys = myKeys
      , mouseBindings = myMouseBindings
        -- hooks, layouts
      , manageHook = myManageHook <+> manageDocks
      , layoutHook = myLayout
      , handleEventHook =
          ewmhDesktopsEventHook <+>
          fullscreenEventHook <+>
          perWindowKbdLayout <+>
          docksEventHook <+>
          dynamicPropertyChange "WM_NAME" myDynHook
      -- , startupHook = myStartupHook
      , logHook = dynamicLogString myPP >>= xmonadPropLog >> updatePointer (0.5, 0.5) (0, 0)
      }
