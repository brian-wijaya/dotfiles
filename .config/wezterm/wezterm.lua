-- WezTerm config - matching alacritty

local wezterm = require "wezterm"
local config = wezterm.config_builder and wezterm.config_builder() or {}

-- Font
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 10

-- Window
config.window_padding = { left = 14, right = 14, top = 14, bottom = 14 }
config.window_decorations = "NONE"
config.window_background_opacity = 0.97
config.window_close_confirmation = "NeverPrompt"

-- Tab bar
config.enable_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true

-- Tokyo Night colors (matching alacritty)
config.colors = {
    foreground = "#a9b1d6",
    background = "#1a1b26",

    cursor_fg = "#1a1b26",
    cursor_bg = "#c0caf5",

    selection_fg = "#c0caf5",
    selection_bg = "#7aa2f7",

    ansi = {
        "#15161e", -- black
        "#f7768e", -- red
        "#9ece6a", -- green
        "#e0af68", -- yellow
        "#7aa2f7", -- blue
        "#bb9af7", -- magenta
        "#7dcfff", -- cyan
        "#a9b1d6", -- white
    },
    brights = {
        "#414868", -- bright black
        "#f7768e", -- bright red
        "#9ece6a", -- bright green
        "#e0af68", -- bright yellow
        "#7aa2f7", -- bright blue
        "#bb9af7", -- bright magenta
        "#7dcfff", -- bright cyan
        "#c0caf5", -- bright white
    },
}

-- Keybindings
config.keys = {
    -- Shift+Enter sends escape sequence (matching alacritty)
    { key = "Enter", mods = "SHIFT", action = wezterm.action.SendString("\x1b\r") },
}

return config
