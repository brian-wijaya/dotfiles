-- which-key.nvim: Full panel mode for rehearsal and overview
-- Configured for large, non-scroll panel showing all mappings at once
-- IMPORTANT: Disabled in vscode-neovim because it tries to sync layout with vscode API
-- which causes "vscode.internal" errors. Use Cursor's built-in command palette instead.

return {
  "folke/which-key.nvim",
  lazy = false,  -- Load immediately, not lazy

  -- Aggressively disable in vscode-neovim to avoid layout sync errors
  cond = function()
    return not (vim.g.vscode == 1)
  end,

  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,

  opts = {
    -- Window configuration (v3 format)
    win = {
      border = "rounded",
      padding = { 1, 2 },  -- { top/bottom, left/right }
      wo = {
        winblend = 0,
      },
    },

    -- Don't use custom triggers - let which-key auto-detect
    -- triggers = "auto" is the default in v3

    -- Only show mappings that actually have descriptions
    filter = function(mapping)
      return mapping.desc and mapping.desc ~= ""
    end,

    plugins = {
      marks = true,
      registers = true,
      spelling = {
        enabled = true,
        suggestions = 20,
      },
    },
  },

  config = function(_, opts)
    if vim.g.vscode == 1 then
      return
    end

    local wk = require("which-key")
    wk.setup(opts)

    -- ============================================
    -- EXHAUSTIVE KEY REFERENCE
    -- All prefix groups registered for discoverability
    -- ============================================

    wk.add({
      -- Leader groups
      { "<leader>a", group = "AI" },
      { "<leader>b", group = "Buffer" },
      { "<leader>c", group = "Code" },
      { "<leader>d", group = "Debug" },
      { "<leader>e", group = "Diagnostics" },
      { "<leader>f", group = "Find" },
      { "<leader>g", group = "Git" },
      { "<leader>h", group = "Harpoon" },
      { "<leader>m", group = "Marks" },
      { "<leader>p", group = "Project" },
      { "<leader>r", group = "Refactor" },
      { "<leader>s", group = "Search" },
      { "<leader>t", group = "Toggle" },
      { "<leader>T", group = "Treesitter" },
      { "<leader>w", group = "Window" },
      { "<leader>x", group = "Trouble" },
      { "<leader>y", group = "Yank" },

      -- Navigation prefixes
      { "[", group = "Previous" },
      { "]", group = "Next" },

      -- g prefix (go/LSP)
      { "g", group = "Go/LSP" },
      { "gr", group = "LSP references/rename" },

      -- z prefix (folds/view)
      { "z", group = "Folds/View" },

      -- s prefix (flash)
      { "s", desc = "Flash (jump to char)" },
      { "S", desc = "Flash Treesitter (jump to node)" },
    })

    -- Document [ prefix mappings
    wk.add({
      { "[d", desc = "Previous diagnostic" },
      { "[D", desc = "First diagnostic in buffer" },
      { "[b", desc = "Previous buffer" },
      { "[B", desc = "First buffer" },
      { "[q", desc = "Previous quickfix" },
      { "[Q", desc = "First quickfix" },
      { "[l", desc = "Previous loclist" },
      { "[L", desc = "First loclist" },
      { "[t", desc = "Previous todo comment" },
      { "[a", desc = "Previous file (arglist)" },
      { "[ ", desc = "Add blank line above" },
    })

    -- Document ] prefix mappings
    wk.add({
      { "]d", desc = "Next diagnostic" },
      { "]D", desc = "Last diagnostic in buffer" },
      { "]b", desc = "Next buffer" },
      { "]B", desc = "Last buffer" },
      { "]q", desc = "Next quickfix" },
      { "]Q", desc = "Last quickfix" },
      { "]l", desc = "Next loclist" },
      { "]L", desc = "Last loclist" },
      { "]t", desc = "Next todo comment" },
      { "]a", desc = "Next file (arglist)" },
      { "] ", desc = "Add blank line below" },
    })

    -- Document g prefix mappings
    wk.add({
      { "gd", desc = "Go to definition" },
      { "gD", desc = "Go to declaration" },
      { "gi", desc = "Go to implementation" },
      { "gr", desc = "Find references" },
      { "grn", desc = "Rename symbol" },
      { "gra", desc = "Code action" },
      { "grr", desc = "References" },
      { "gri", desc = "Implementation" },
      { "grt", desc = "Type definition" },
      { "gO", desc = "Document symbols" },
      { "gc", desc = "Comment (motion)" },
      { "gcc", desc = "Comment line" },
      { "gx", desc = "Open URI/filepath" },
    })

    -- Document z prefix mappings
    wk.add({
      { "zR", desc = "Open all folds" },
      { "zM", desc = "Close all folds" },
      { "za", desc = "Toggle fold under cursor" },
      { "zo", desc = "Open fold under cursor" },
      { "zc", desc = "Close fold under cursor" },
    })

    -- Document Ctrl mappings
    wk.add({
      { "<C-h>", desc = "Navigate left (vim/tmux)" },
      { "<C-j>", desc = "Navigate down (vim/tmux)" },
      { "<C-k>", desc = "Navigate up (vim/tmux)" },
      { "<C-l>", desc = "Navigate right (vim/tmux)" },
      { "<C-d>", desc = "Scroll down (centered)" },
      { "<C-u>", desc = "Scroll up (centered)" },
      { "<C-s>", desc = "Save file" },
      { "<C-a>", desc = "Increment number" },
      { "<C-x>", desc = "Decrement number" },
    })

    -- Document other useful mappings
    wk.add({
      { "K", desc = "Hover documentation" },
      { "n", desc = "Next search (centered)" },
      { "N", desc = "Prev search (centered)" },
      { "<Tab>", desc = "Next buffer" },
      { "<S-Tab>", desc = "Previous buffer" },
      { "<Esc>", desc = "Clear search highlights" },
      { "jk", desc = "Exit insert mode", mode = "i" },
    })

    -- Custom mappings
    wk.add({
      { "<leader>ac", "<cmd>Claude<cr>", desc = "Launch Claude", mode = "n" },
      {
        "<leader>gw",
        "<cmd>edit /home/bw/dotfiles/docs/CURSOR-WORKFLOW-GUIDE.md<cr>",
        desc = "Open workflow guide",
        mode = "n",
      },
    })
  end,
}
