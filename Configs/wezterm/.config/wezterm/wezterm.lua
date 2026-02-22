local wezterm = require("wezterm")
local cur_os = wezterm.target_triple
local config = wezterm.config_builder()

if cur_os:find("windows") then
	config.default_prog = { "pwsh.exe", "-NoLogo" }
end

config.color_scheme = "Catppuccin Macchiato"

config.font = wezterm.font("JetBrainsMono NF", { weight = "DemiBold" })
config.font = wezterm.font_with_fallback({
	"D2CodingLigature Nerd Font",
})
config.font_size = 14

config.enable_tab_bar = true

config.window_background_opacity = 0.75

config.enable_wayland = false

config.mux_enable_ssh_agent = false

return config
