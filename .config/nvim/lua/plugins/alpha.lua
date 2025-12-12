return {
  "goolord/alpha-nvim",
  event = "VimEnter",
  enabled = true,
  init = false,
  opts = function()
    local dashboard = require("alpha.themes.dashboard")

    -- Header
    dashboard.section.header.val = {
      "                      ",
      "  ▄▄         ▄ ▄▄▄▄▄▄▄",
      "▄▀███▄     ▄██ █████▀ ",
      "██▄▀███▄   ███        ",
      "███  ▀███▄ ███        ",
      "███    ▀██ ███        ",
      "███      ▀ ███        ",
      "▀██ █████▄▀█▀▄██████▄ ",
      "  ▀ ▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀",
      "                      ",
      "  ⚡ Powered By ⚡ eovim ",
      "                      ",
    }

    -- Buttons
    dashboard.section.buttons.val = {
      dashboard.button("f", "󰈞  Find file", "<cmd>Telescope find_files<cr>"),
      dashboard.button("n", "󰈔  New file", "<cmd>ene <BAR> startinsert<cr>"),
      dashboard.button("r", "󰄉  Recent files", "<cmd>Telescope oldfiles<cr>"),
      dashboard.button("g", "󰱽  Find text", "<cmd>Telescope live_grep<cr>"),
      dashboard.button("c", "󰒓  Config", "<cmd>e ~/.config/nvim/init.lua<cr>"),
      dashboard.button("q", "󰩈  Quit", "<cmd>qa<cr>"),
    }

    -- Footer
    dashboard.section.footer.val = ""

    -- Layout
    dashboard.opts.layout = {
      { type = "padding", val = 2 },
      dashboard.section.header,
      { type = "padding", val = 2 },
      dashboard.section.buttons,
      { type = "padding", val = 1 },
      dashboard.section.footer,
    }

    return dashboard
  end,
  config = function(_, dashboard)
    require("alpha").setup(dashboard.opts)

    vim.api.nvim_create_autocmd("User", {
      pattern = "LazyVimStarted",
      callback = function()
        local stats = require("lazy").stats()
        local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
        dashboard.section.footer.val = "⚡ Neovim loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms .. "ms"
        pcall(vim.cmd.AlphaRedraw)
      end,
    })
  end,
}



