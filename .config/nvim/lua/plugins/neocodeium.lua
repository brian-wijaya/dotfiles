-- neocodeium: Free AI completion plugin powered by Windsurf (formerly Codeium)
-- https://github.com/monkoose/neocodeium
-- Disable in vscode-neovim (Cursor has built-in AI completion)
return {
  "monkoose/neocodeium",
  event = "InsertEnter",
  cond = function() return not (vim.g.vscode == 1) end,
  config = function()
    require("neocodeium").setup({
      -- Enable by default
      enabled = true,
      -- File types to enable (empty = all)
      filetypes = {},
      -- Filter function to enable/disable per buffer
      filter = function(bufnr)
        -- Enable for all buffers by default
        return true
      end,
      -- Logging level (trace, debug, info, warn, error)
      log_level = "warn",
    })

    -- Keymaps (using Alt key to avoid conflicts with Tab)
    local map = vim.keymap.set
    map("i", "<A-f>", function()
      require("neocodeium").accept()
    end, { desc = "Accept completion" })
    
    map("i", "<A-w>", function()
      require("neocodeium").accept_word()
    end, { desc = "Accept word" })
    
    map("i", "<A-a>", function()
      require("neocodeium").accept_line()
    end, { desc = "Accept line" })
    
    map("i", "<A-e>", function()
      require("neocodeium").cycle_or_complete()
    end, { desc = "Cycle or complete" })
    
    map("i", "<A-r>", function()
      require("neocodeium").cycle_or_complete(-1)
    end, { desc = "Cycle previous" })
    
    map("i", "<A-c>", function()
      require("neocodeium").clear()
    end, { desc = "Clear completion" })
  end,
}



