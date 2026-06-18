-- Hyprland 0.55 Lua config.
-- https://wiki.hypr.land/Configuring/Start/

local terminal = "alacritty"
local fileManager = "dolphin"
local menu = "rofi -show run"
local mainMod = "SUPER"

hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })
hl.monitor({ output = "eDP-1", mode = "2560x1600@60", position = "auto", scale = 1.25 })
hl.monitor({ output = "DP-2", mode = "2560x1440", position = "2560x0", scale = "auto" })
hl.monitor({ output = "DP-3", mode = "2560x1440", position = "0x0", scale = "auto" })
hl.monitor({ output = "DP-4", mode = "2560x1440", position = "0x0", scale = 1 })
hl.monitor({ output = "DP-9", mode = "2560x1440", position = "2560x0", scale = "auto" })
hl.monitor({ output = "DP-10", mode = "2560x1440", position = "0x0", scale = "auto" })
--hl.monitor({ output = "DP-9", mode = "2560x1440", position = "1440x0", scale = "auto" })
--hl.monitor({ output = "DP-10", mode = "2560x1440", position = "0x0", scale = "auto", transform = 3 })
--hl.monitor({ output = "DP-11", mode = "2560x1440", position = "2560x0", scale = "auto" })
--hl.monitor({ output = "DP-12", mode = "2560x1440", position = "0x0", scale = "auto" })
hl.monitor({ output = "DP-11", mode = "2560x1440", position = "1440x0", scale = "auto" })
hl.monitor({ output = "DP-12", mode = "2560x1440", position = "0x0", scale = "auto", transform = 3 })

hl.on("hyprland.start", function()
  hl.exec_cmd("quickshell")
  -- hl.exec_cmd("wl-paste --type text --watch cliphist store")
  -- hl.exec_cmd("wl-paste --type image --watch cliphist store")
  -- Clipse watches `--type text` and `--type image/png`, but we need to add
  -- `--primary` and mirror selected clipse entries to the primary selection.
  -- Also `-listen` kills other wl-paste processes so we need to replace it
  -- with explicit `--watch` commands.
  --hl.exec_cmd("clipse -listen")
  hl.exec_cmd("wl-paste --type text --watch clipse -wl-store")
  hl.exec_cmd("wl-paste --type image --watch clipse -wl-store")
  hl.exec_cmd("wl-paste --primary --type text --watch clipse -wl-store")
  hl.exec_cmd("wl-paste --watch wl-copy --primary")
  hl.exec_cmd('tmux setenv -g HYPRLAND_INSTANCE_SIGNATURE "$HYPRLAND_INSTANCE_SIGNATURE"')
  hl.exec_cmd("hypridle")
  hl.exec_cmd("1password --silent")
end)

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "40")
hl.env("HYPRCURSOR_THEME", "Nordzy-hyprcursors-catppuccin-macchiato-dark")
hl.env("QT_QPA_PLATFORM", "wayland")

hl.config({
  general = {
    gaps_in = 3,
    gaps_out = 5,
    border_size = 1,
    col = {
      active_border = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
      inactive_border = "rgba(595959aa)",
    },
    resize_on_border = false,
    allow_tearing = false,
    layout = "master",
  },
  decoration = {
    rounding = 3,
    active_opacity = 1.0,
    inactive_opacity = 1.0,
    shadow = {
      enabled = true,
      range = 4,
      render_power = 3,
      color = "rgba(1a1a1aee)",
    },
    blur = {
      enabled = true,
      size = 3,
      passes = 1,
      vibrancy = 0.1696,
    },
  },
  animations = {
    enabled = false,
  },
  dwindle = {
    preserve_split = true,
  },
  master = {
    new_on_active = "after",
    new_status = "master",
  },
  misc = {
    force_default_wallpaper = 0,
    disable_hyprland_logo = true,
    on_focus_under_fullscreen = 1,
  },
  input = {
    kb_layout = "us",
    kb_variant = "",
    kb_model = "",
    kb_options = "",
    kb_rules = "",
    repeat_delay = 200,
    repeat_rate = 30,
    follow_mouse = 1,
    sensitivity = 0,
    touchpad = {
      natural_scroll = false,
    },
  },
})

hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1 }, { 0.32, 1 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1.0 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })

hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn", enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })

hl.device({
  name = "epic-mouse-v1",
  sensitivity = -0.5,
})

local function mod(keys)
  if keys == "" then
    return mainMod
  end
  return mainMod .. " + " .. keys
end

hl.bind(mod("SHIFT + RETURN"), hl.dsp.exec_cmd(terminal))
hl.bind(mod("SHIFT + CTRL + C"), hl.dsp.window.close())
hl.bind(mod("SHIFT + Q"), hl.dsp.exit())
hl.bind(mod("V"), hl.dsp.window.float({ action = "toggle" }))
hl.bind(mod("P"), hl.dsp.exec_cmd(menu))
hl.bind(mod("CTRL + SPACE"), hl.dsp.exec_cmd("EMOJI_MENU_COMMAND=wl-copy /home/hunner/local/bin/emoji-menu"))
hl.bind(mod("SPACE"), hl.dsp.window.fullscreen({ mode = "maximized" }))

hl.bind(mod("RETURN"), hl.dsp.layout("swapwithmaster"))
hl.bind(mod("J"), hl.dsp.layout("cyclenext"))
hl.bind(mod("K"), hl.dsp.layout("cycleprev"))
hl.bind(mod("SHIFT + J"), hl.dsp.layout("swapnext"))
hl.bind(mod("SHIFT + K"), hl.dsp.layout("swapprev"))
hl.bind(mod("comma"), hl.dsp.layout("addmaster"))
hl.bind(mod("period"), hl.dsp.layout("removemaster"))
hl.bind(mod("H"), hl.dsp.layout("mfact -0.05"))
hl.bind(mod("L"), hl.dsp.layout("mfact +0.05"))

hl.bind(mod("mouse_down"), hl.dsp.exec_cmd([[hyprctl keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 + 0.5}')]]))
hl.bind(mod("mouse_up"), hl.dsp.exec_cmd([[hyprctl keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 - 0.5}')]]))
hl.bind(mod("SHIFT + mouse_up"), hl.dsp.exec_cmd("hyprctl keyword cursor:zoom_factor 1"))

hl.bind(mod("left"), hl.dsp.focus({ direction = "left" }))
hl.bind(mod("right"), hl.dsp.focus({ direction = "right" }))
hl.bind(mod("up"), hl.dsp.focus({ direction = "up" }))
hl.bind(mod("down"), hl.dsp.focus({ direction = "down" }))

hl.bind(mod("o"), hl.dsp.focus({ monitor = 2 }))
hl.bind(mod("e"), hl.dsp.focus({ monitor = 1 }))
hl.bind(mod("u"), hl.dsp.focus({ monitor = 0 }))

hl.bind(mod("SHIFT + o"), hl.dsp.window.move({ monitor = 2 }))
hl.bind(mod("SHIFT + e"), hl.dsp.window.move({ monitor = 1 }))
hl.bind(mod("SHIFT + u"), hl.dsp.window.move({ monitor = 0 }))

for i = 1, 10 do
  local key = tostring(i % 10)
  hl.bind(mod(key), hl.dsp.focus({ workspace = i, on_current_monitor = true }))
  hl.bind(mod("SHIFT + " .. key), hl.dsp.window.move({ workspace = i, follow = false }))
end

hl.bind(mod("S"), hl.dsp.workspace.toggle_special("magic"))
hl.bind(mod("SHIFT + S"), hl.dsp.window.move({ workspace = "special:magic" }))

hl.bind(mod("mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mod("mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

hl.bind(mod("mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(mod("mouse:273"), hl.dsp.window.resize(), { mouse = true })

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true, repeating = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brillo -A 10"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brillo -U 10"), { locked = true, repeating = true })

hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })

-- hl.bind(mod("CTRL + C"), hl.dsp.exec_cmd("cliphist list | wofi --dmenu | cliphist decode | wl-copy"))
hl.bind(mod("CTRL + C"), hl.dsp.exec_cmd("alacritty --class clipse -e 'clipse'"))
hl.bind(mod("SHIFT + W"), hl.dsp.exec_cmd("sh -c 'quickshell kill --any-display; quickshell --daemonize'"))

local function window_rule(name, match, effects)
  local rule = { name = name, match = match }
  for key, value in pairs(effects) do
    rule[key] = value
  end
  hl.window_rule(rule)
end

window_rule("clipse", { class = "clipse" }, {
  float = true,
  center = true,
  no_shadow = true,
})
window_rule("suppress-maximize-events", { class = ".*" }, { suppress_event = "maximize" })
window_rule("fix-xwayland-drags", {
  class = "^$",
  title = "^$",
  xwayland = true,
  float = true,
  fullscreen = false,
  pin = false,
}, {
  no_focus = true,
})

window_rule("flameshot-overlay", { class = "(flameshot)", title = "(flameshot)" }, {
  move = { 0, 0 },
  pin = true,
  fullscreen_state = "2 2",
  float = true,
})

window_rule("jrpn15", { class = "^(jrpn15)$" }, {
  float = true,
  center = true,
})

window_rule("zoom-sharing-toolbar", { class = "^(Zoom( Workplace)?)$", title = "^(as_toolbar)$" }, {
  float = true,
  border_size = 0,
  no_shadow = true,
})

window_rule("zoom-more-toolbar-menu", { class = "^(Zoom( Workplace)?)$", title = "^(Toolbar Menu)$" }, {
  float = true,
  border_size = 0,
  no_shadow = true,
})

window_rule("zoom-breakout-rooms", { class = "^(Zoom( Workplace)?)$", title = "^(Breakout rooms - .+)$" }, {
  float = true,
  no_shadow = true,
})

window_rule("zoom-annotation-toolbar", { class = "^(Zoom( Workplace)?)$", title = "^(annotate_toolbar)$" }, {
  float = true,
  no_shadow = true,
})

-- This is the annotation overlay. It should be overlayed on top of the shared screen, but dunno how to do that
window_rule("zoom-annotations", { class = "^(Zoom( Workplace)?)$", title = "^(Annotation - Zoom)$" }, {
  float = true,
  no_shadow = true,
})

window_rule("zoom-green-border-selection", { title = "^(cpt_frame_xcb_window)$" }, {
  float = true,
  border_size = 0,
})

window_rule("zoom-landing-window", { class = "^(Zoom( Workplace)?)$", title = "^(Zoom( Workplace)? - Licensed account)$" }, {
  float = true,
  center = true,
})

window_rule("zoom-settings", { class = "^(Zoom( Workplace)?)$", title = "^(Settings)$" }, {
  float = true,
})

window_rule("zoom-menu-window", { class = "^(Zoom( Workplace)?)$", title = "^(menu window)$" }, {
  float = true,
  stay_focused = true,
})

window_rule("zoom-sub-menu-window", { class = "^(Zoom( Workplace)?)$", title = "^(sub menu window)$" }, {
  float = true,
  stay_focused = true,
})

window_rule("zoom-topbar-popup", { class = "^(Zoom( Workplace)?)$", title = "^(meeting topbar popup)$" }, {
  float = true,
  border_size = 0,
  no_shadow = true,
  stay_focused = true,
})

window_rule("zoom-bottombar-popup", { class = "^(Zoom( Workplace)?)$", title = "^(meeting bottombar popup)$" }, {
  float = true,
  border_size = 0,
  no_shadow = true,
  stay_focused = true,
})

window_rule("zoom-misc-popup", { class = "^(Zoom( Workplace)?)$", title = "^(zoom)$" }, {
  float = true,
  border_size = 0,
  no_shadow = true,
})

window_rule("zoom-participants", { class = "^(Zoom( Workplace)?)$", title = "^(Participants)(.*)$" }, {
  float = true,
})

window_rule("zoom-chat", { class = "^(Zoom( Workplace)?)$", title = "^(Meeting chat)$" }, {
  float = true,
})

window_rule("zoom-end-meeting-dialog", { class = "^(Zoom( Workplace)?)$", title = "^(Zoom( Workplace)?|Leave meeting panel)$" }, {
  float = true,
  stay_focused = false,
})
