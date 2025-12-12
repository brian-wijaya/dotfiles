require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("i", "jk", "<ESC>")

-- Override NvChad's window navigation to use vim-tmux-navigator
-- This allows seamless navigation between Neovim splits and tmux panes
map("n", "<C-h>", "<cmd>TmuxNavigateLeft<cr>", { desc = "Navigate left (vim/tmux)" })
map("n", "<C-j>", "<cmd>TmuxNavigateDown<cr>", { desc = "Navigate down (vim/tmux)" })
map("n", "<C-k>", "<cmd>TmuxNavigateUp<cr>", { desc = "Navigate up (vim/tmux)" })
map("n", "<C-l>", "<cmd>TmuxNavigateRight<cr>", { desc = "Navigate right (vim/tmux)" })

-- Claude launcher inside Neovim
vim.api.nvim_create_user_command("Claude", function()
  vim.cmd("terminal cld")
end, { desc = "Launch Claude in terminal buffer" })

-- Workflow guide quick access (absolute path, no cwd dependence)
map("n", "<leader>gw", "<cmd>edit /home/bw/dotfiles/WORKFLOW-GUIDE.md<cr>", { desc = "Open workflow guide" })

-- Claude launcher mapping
map("n", "<leader>ac", "<cmd>Claude<cr>", { desc = "Launch Claude" })

-- Make percentage jumps repeatable with .
-- This overrides % to be repeatable when used with count (e.g., 10%)
local repeatable = require("utils.repeatable")
repeatable.setup_percent_repeat()

-- ============================================
-- COMPREHENSIVE NAVIGATION WORKFLOW
-- ============================================

local nav = require("utils.navigation")

-- LSP Navigation: Enhanced with next/prev support
-- Override default gd/gr/gi to store results for navigation
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function()
    local bufnr = vim.api.nvim_get_current_buf()
    
    -- Enhanced definition with navigation
    map("n", "gd", nav.goto_definition, { buffer = bufnr, desc = "Go to definition (with navigation)" })
    
    -- Enhanced references with navigation
    map("n", "gr", nav.goto_references, { buffer = bufnr, desc = "Find references (with navigation)" })
    
    -- Enhanced implementation with navigation
    map("n", "gi", nav.goto_implementation, { buffer = bufnr, desc = "Go to implementation (with navigation)" })
    
    -- Navigate next/prev in LSP results
    map("n", "]d", function() nav.navigate_lsp_results("next") end, { buffer = bufnr, desc = "Next definition/reference/implementation" })
    map("n", "[d", function() nav.navigate_lsp_results("prev") end, { buffer = bufnr, desc = "Previous definition/reference/implementation" })
    
    -- Navigate to next/prev word with definition
    map("n", "]w", nav.next_word_with_definition, { buffer = bufnr, desc = "Next word with definition" })
    map("n", "[w", nav.prev_word_with_definition, { buffer = bufnr, desc = "Previous word with definition" })
  end,
})

-- File path yanking
map("n", "<leader>yf", function()
  local path = vim.api.nvim_buf_get_name(0)
  if path and path ~= "" then
    vim.fn.setreg("+", path)
    vim.fn.setreg('"', path)
    vim.notify("Yanked: " .. path, vim.log.levels.INFO)
  else
    vim.notify("No file path available", vim.log.levels.WARN)
  end
end, { desc = "Yank file absolute path" })

-- Yank relative path
map("n", "<leader>yr", function()
  local path = vim.api.nvim_buf_get_name(0)
  if path and path ~= "" then
    local rel_path = vim.fn.fnamemodify(path, ":~:.")
    vim.fn.setreg("+", rel_path)
    vim.fn.setreg('"', rel_path)
    vim.notify("Yanked: " .. rel_path, vim.log.levels.INFO)
  else
    vim.notify("No file path available", vim.log.levels.WARN)
  end
end, { desc = "Yank file relative path" })

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
