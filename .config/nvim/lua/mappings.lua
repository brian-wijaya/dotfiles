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

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
