local wezterm = require "wezterm"

return {
  -- Default to WSL archlinux
  default_domain = "WSL:archlinux",

  -- Font settings (Nerd Fonts for icons)
  -- WezTerm font names - try multiple variations to ensure compatibility
  font = wezterm.font_with_fallback({
    { family = "CascadiaCode NF", weight = "Regular" },
    { family = "CascadiaCode Nerd Font", weight = "Regular" },
    { family = "Cascadia Code NF", weight = "Regular" },
    { family = "Cascadia Code", weight = "Regular" },
    { family = "Hack Nerd Font", weight = "Regular" },
    { family = "FiraCode Nerd Font", weight = "Regular" },
    { family = "Fira Code", weight = "Regular" },
    { family = "JetBrainsMono Nerd Font", weight = "Regular" },
    "Consolas",
    "monospace",
  }),
  font_size = 11.5,

  -- Terminal appearance
  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = false,
  use_fancy_tab_bar = true,
  window_decorations = "RESIZE",
  color_scheme = "Catppuccin Mocha",

  -- Keybindings
  keys = {
    -- Ctrl+Shift+T → new independent bash tab (no tmux)
    {
      key = "t",
      mods = "CTRL|SHIFT",
      action = wezterm.action.SpawnCommandInNewTab {
        domain = { DomainName = "WSL:archlinux" },
        cwd = "/home/bw/signal-assembly-platform",
        args = { "bash", "-c", "export WEZTERM_NOTMUX=1; cd ~/signal-assembly-platform; exec bash -l" },
      },
    },

    -- Ctrl+Shift+N → nvim in new tab
    {
      key = "n",
      mods = "CTRL|SHIFT",
      action = wezterm.action.SpawnCommandInNewTab {
        domain = { DomainName = "WSL:archlinux" },
        cwd = "/home/bw/signal-assembly-platform",
        args = { "bash", "-c", "export WEZTERM_NOTMUX=1; cd ~/signal-assembly-platform; exec nvim" },
      },
    },
  },
}

