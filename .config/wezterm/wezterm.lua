local wezterm = require "wezterm"

return {
  -- Default to WSL archlinux
  default_domain = "WSL:archlinux",

  -- Font settings (Nerd Fonts for icons)
  -- Primary font: Cascadia Code NF (installed by install.sh)
  font = wezterm.font("Cascadia Code NF"),
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

