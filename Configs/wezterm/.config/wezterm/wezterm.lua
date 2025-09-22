local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Macchiato"

config.font = wezterm.font("JetBrainsMono NF", { weight = "DemiBold" })
config.font_size = 14

config.enable_tab_bar = false

config.window_background_opacity = 0.75

config.enable_wayland = true

return config
