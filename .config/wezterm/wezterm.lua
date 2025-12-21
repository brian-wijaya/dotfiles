local wezterm = require "wezterm"
local mux = wezterm.mux
local act = wezterm.action

-- Smart paste: if clipboard has image, save it and paste path; otherwise normal paste
wezterm.on("smart-paste", function(window, pane)
  local success, stdout, stderr = wezterm.run_child_process({
    "powershell.exe", "-NoProfile", "-Command", [[
      $img = Get-Clipboard -Format Image
      if ($img) {
        $timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
        $path = "C:\Users\wijay\Dropbox\Downloads\clipboard_$timestamp.png"
        $img.Save($path)
        Write-Output $path
      } else {
        Write-Output "TEXT"
      }
    ]]
  })

  local output = stdout:gsub("[\r\n]+$", "")

  if output == "TEXT" then
    -- Normal text paste
    window:perform_action(act.PasteFrom("Clipboard"), pane)
  else
    -- Image was saved, convert Windows path to WSL path and type it
    local wsl_path = output:gsub("C:\\Users\\wijay\\", "/mnt/c/Users/wijay/"):gsub("\\", "/")
    window:perform_action(act.SendString(wsl_path), pane)
  end
end)

-- Window position and size in pixels
local window_x = 1146
local window_y = 31  -- Account for title bar height
local window_width = 2301
local window_height = 1416  -- Reduced to fit with title bar offset

local config = wezterm.config_builder and wezterm.config_builder() or {}

-- Default to WSL archlinux
config.default_domain = "WSL:archlinux"

-- Font settings
config.font_dirs = { wezterm.home_dir .. "\\AppData\\Local\\Microsoft\\Windows\\Fonts" }
config.font = wezterm.font_with_fallback({
  "JetBrainsMono Nerd Font",  -- Nerd Font first (has icons + all regular chars)
  "JetBrains Mono",
  "Consolas",
})
config.font_size = 10

-- Terminal appearance
config.enable_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_decorations = "TITLE | RESIZE"
config.color_scheme = "Vs Code Dark+ (Gogh)"
config.window_close_confirmation = "NeverPrompt"

-- Keybindings
config.keys = {
  -- Ctrl+Shift+T → new independent zsh tab (no tmux)
  {
    key = "t",
    mods = "CTRL|SHIFT",
    action = wezterm.action.SpawnCommandInNewTab {
      domain = { DomainName = "WSL:archlinux" },
      cwd = "/home/bw",
      args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
    },
  },

  -- Ctrl+Shift+N → nvim in new tab
  {
    key = "n",
    mods = "CTRL|SHIFT",
    action = wezterm.action.SpawnCommandInNewTab {
      domain = { DomainName = "WSL:archlinux" },
      cwd = "/home/bw",
      args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec nvim" },
    },
  },

  -- Alt+Shift+V → side by side (vertical divider |) - opens in current directory
  {
    key = "V",
    mods = "ALT|SHIFT",
    action = wezterm.action_callback(function(window, pane)
      local cwd = pane:get_current_working_dir()
      local cwd_path = cwd and cwd.file_path or "/home/bw"
      window:perform_action(wezterm.action.SplitHorizontal {
        domain = { DomainName = "WSL:archlinux" },
        cwd = cwd_path,
        args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
      }, pane)
    end),
  },

  -- Alt+Shift+H → stacked (horizontal divider ─) - opens in current directory
  {
    key = "H",
    mods = "ALT|SHIFT",
    action = wezterm.action_callback(function(window, pane)
      local cwd = pane:get_current_working_dir()
      local cwd_path = cwd and cwd.file_path or "/home/bw"
      window:perform_action(wezterm.action.SplitVertical {
        domain = { DomainName = "WSL:archlinux" },
        cwd = cwd_path,
        args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
      }, pane)
    end),
  },

  -- Smart paste: image → save & paste path, text → normal paste
  -- Ctrl+V passes through to vim/tmux, only Ctrl+Shift+V is smart paste
  { key = "V", mods = "CTRL|SHIFT", action = act.EmitEvent("smart-paste") },

  -- Alt+hjkl → WezTerm panes | Ctrl+hjkl → tmux panes (passes through)
  { key = "h", mods = "ALT", action = act.ActivatePaneDirection "Left" },
  { key = "j", mods = "ALT", action = act.ActivatePaneDirection "Down" },
  { key = "k", mods = "ALT", action = act.ActivatePaneDirection "Up" },
  { key = "l", mods = "ALT", action = act.ActivatePaneDirection "Right" },

  -- Copy mode (vim-like scrollback navigation)
  -- Ctrl+Shift+X → enter copy mode, then use hjkl, v, V, y, /, ?, n, N
  { key = "X", mods = "CTRL|SHIFT", action = act.ActivateCopyMode },

  -- Quick scroll without entering copy mode
  { key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-1) },
  { key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(1) },
  { key = "UpArrow", mods = "SHIFT", action = act.ScrollByLine(-1) },
  { key = "DownArrow", mods = "SHIFT", action = act.ScrollByLine(1) },

  -- Command palette (shows all available actions)
  { key = "P", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
}

-- Set initial window position and size on startup (in pixels)
wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  local gui = window:gui_window()
  gui:set_inner_size(window_width, window_height)
  gui:set_position(window_x, window_y)
end)

return config
