local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Macchiato"

config.font = wezterm.font("JetBrainsMono NF")
config.font_size = 14

config.enable_tab_bar = false

return config
