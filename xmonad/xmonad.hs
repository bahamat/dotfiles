--   Copyright 2025 Brianna Bennett
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
-- import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig
import System.IO

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "#859900" "" -- . shorten 50
      }
    , modMask = mod4Mask -- Use Super instead of Alt
    , terminal = "urxvt"
    , focusedBorderColor = "#d33682"
    , normalBorderColor  = "#6c71c4"
    } `additionalKeysP`
    -- M  == Super
    -- S  == Shift
    -- C  == Control
    -- M1 == Alt
    [ ("M1-<Space>", spawn "exe=$(dmenu_path | dmenu) && eval \"exec $exe\"")
    , ("<XF86PowerOff>", spawn "xscreensaver-command -lock")
    , ("<XF86Paste>", spawn "xsel | xvkbd -xsendevent -file -")
    -- , ("M-S-z", spawn "xscreensaver-command -lock")
    -- The style below is used by `additionalKeys`. The above style is used by `additionalKeysP`.
    -- , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    -- , ((0,                0x1008FF2A), spawn "xscreensaver-command -lock")
    -- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    -- , ((0, xK_Print), spawn "scrot")
    ]
