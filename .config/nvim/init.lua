vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"
vim.g.mapleader = " "

-- Detect vscode-neovim early (before plugin loading)
-- This allows conditional plugin loading to work correctly
-- vscode-neovim sets vim.g.vscode = 1, but we need to handle the case where it's not set yet
if vim.g.vscode == nil then
  -- In vscode-neovim, this will be set to 1 by the extension
  -- In standalone Neovim, it will remain nil (which is falsy)
  vim.g.vscode = 0
end

-- Disable which-key layout syncing in vscode-neovim to prevent vscode.internal errors
-- This must be set before any plugins load
if vim.g.vscode == 1 then
  vim.g.which_key_disable = true
end

-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
  },

  { import = "plugins" },
}, lazy_config)

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "options"
require "autocmds"

vim.schedule(function()
  require "mappings"
end)
