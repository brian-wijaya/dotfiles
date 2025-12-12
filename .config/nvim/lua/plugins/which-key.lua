-- which-key.nvim: Full panel mode for rehearsal and overview
-- Configured for large, non-scroll panel showing all mappings at once
-- IMPORTANT: Disabled in vscode-neovim because it tries to sync layout with vscode API
-- which causes "vscode.internal" errors. Use Cursor's built-in command palette instead.
return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  -- Aggressively disable in vscode-neovim to avoid layout sync errors
  -- Use lazy.nvim's cond to prevent loading, and also check in config
  cond = function() 
    -- Check early - if vscode is set to 1, don't load at all
    return not (vim.g.vscode == 1)
  end,
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,
  opts = {
    -- Large panel configuration - maximize width and height
    window = {
      border = "rounded",
      position = "bottom",
      margin = { 1, 0, 1, 0 }, -- top, right, bottom, left
      padding = { 2, 2, 2, 2 }, -- top, right, bottom, left
      winblend = 0,
      zindex = 1000,
    },
    -- Show all mappings, don't hide anything
    show_help = true,
    show_keys = true,
    -- Don't hide mappings by default
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " },
    -- Disable paging - show everything
    ignore_missing = false,
    -- Show all key groups
    plugins = {
      marks = true,
      registers = true,
      spelling = {
        enabled = true,
        suggestions = 20,
      },
    },
    -- Truncate long descriptions to fit in large panel
    triggers = "auto",
    triggers_blacklist = {
      i = { "j", "k" },
      v = { "j", "k" },
    },
  },
  config = function(_, opts)
    -- Double-check: if somehow loaded in vscode, don't set it up
    if vim.g.vscode == 1 then
      return
    end
    local wk = require("which-key")
    wk.setup(opts)
    
    -- Register our custom mappings
    wk.register({
      ["<leader>ac"] = { "<cmd>Claude<cr>", "Launch Claude" },
      ["<leader>gw"] = { "<cmd>edit /home/bw/dotfiles/WORKFLOW-GUIDE.md<cr>", "Open workflow guide" },
    })
  end,
}

