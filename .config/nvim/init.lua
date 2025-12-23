vim.g.mapleader = " "

-- Detect vscode-neovim early (before plugin loading)
if vim.g.vscode == nil then
  vim.g.vscode = 0
end

-- Disable which-key layout syncing in vscode-neovim
if vim.g.vscode == 1 then
  vim.g.which_key_disable = true
end

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

-- Load plugins
require("lazy").setup({
  { import = "plugins" },
}, require "configs.lazy")

-- Load core config
require "options"
require "autocmds"
require("config.smartjump").setup()

vim.schedule(function()
  require "mappings"
end)
