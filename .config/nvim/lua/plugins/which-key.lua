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
    -- EVERYTHING FLAT ON LEADER MENU
    -- Press spacebar = see EVERYTHING
    -- ============================================

    wk.add({
      -- === NAVIGATION ([ and ]) ===
      { "<leader>1", function() wk.show({ keys = "[", mode = "n" }) end, desc = "[ PREV nav menu" },
      { "<leader>2", function() wk.show({ keys = "]", mode = "n" }) end, desc = "] NEXT nav menu" },

      -- === GO/LSP (g prefix) ===
      { "<leader>3", function() wk.show({ keys = "g", mode = "n" }) end, desc = "g GO/LSP menu" },

      -- === FOLDS (z prefix) ===
      { "<leader>4", function() wk.show({ keys = "z", mode = "n" }) end, desc = "z FOLDS menu" },

      -- === AI ===
      { "<leader>ac", "<cmd>Claude<cr>", desc = "Claude" },

      -- === BUFFER ===
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffer list" },
      { "<leader>bd", "<cmd>bdelete<cr>", desc = "Delete buffer" },
      { "<leader>bn", "<cmd>bnext<cr>", desc = "Next buffer" },
      { "<leader>bp", "<cmd>bprev<cr>", desc = "Prev buffer" },

      -- === CODE ===
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Code action" },
      { "<leader>cf", function() require("conform").format() end, desc = "Format" },
      { "<leader>cr", vim.lsp.buf.rename, desc = "Rename symbol" },

      -- === DIAGNOSTICS ===
      { "<leader>dd", vim.diagnostic.open_float, desc = "Line diagnostics" },
      { "<leader>dn", vim.diagnostic.goto_next, desc = "Next diagnostic" },
      { "<leader>dp", vim.diagnostic.goto_prev, desc = "Prev diagnostic" },
      { "<leader>dl", "<cmd>Telescope diagnostics<cr>", desc = "List diagnostics" },

      -- === FIND (Telescope) ===
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find files" },
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Grep text" },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help" },
      { "<leader>fo", "<cmd>Telescope oldfiles<cr>", desc = "Recent files" },
      { "<leader>fc", "<cmd>Telescope git_commits<cr>", desc = "Git commits" },
      { "<leader>fs", "<cmd>Telescope git_status<cr>", desc = "Git status" },
      { "<leader>fk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
      { "<leader>ft", "<cmd>TodoTelescope<cr>", desc = "TODOs" },

      -- === GIT ===
      { "<leader>gb", "<cmd>Git blame<cr>", desc = "Blame" },
      { "<leader>gl", "<cmd>LazyGit<cr>", desc = "LazyGit" },
      { "<leader>gd", "<cmd>Gitsigns diffthis<cr>", desc = "Diff" },
      { "<leader>gw", "<cmd>edit /home/bw/dotfiles/docs/CURSOR-WORKFLOW-GUIDE.md<cr>", desc = "Workflow guide" },

      -- === HARPOON ===
      { "<leader>ha", function() require("harpoon"):list():add() end, desc = "Add mark" },
      { "<leader>hh", function() require("harpoon").ui:toggle_quick_menu(require("harpoon"):list()) end, desc = "Menu" },
      { "<leader>h1", function() require("harpoon"):list():select(1) end, desc = "File 1" },
      { "<leader>h2", function() require("harpoon"):list():select(2) end, desc = "File 2" },
      { "<leader>h3", function() require("harpoon"):list():select(3) end, desc = "File 3" },
      { "<leader>h4", function() require("harpoon"):list():select(4) end, desc = "File 4" },

      -- === LSP ===
      { "<leader>li", "<cmd>LspInfo<cr>", desc = "LSP info" },
      { "<leader>lr", "<cmd>LspRestart<cr>", desc = "LSP restart" },

      -- === TOGGLE ===
      { "<leader>tn", "<cmd>set number!<cr>", desc = "Line numbers" },
      { "<leader>tr", "<cmd>set relativenumber!<cr>", desc = "Relative numbers" },
      { "<leader>tw", "<cmd>set wrap!<cr>", desc = "Word wrap" },
      { "<leader>ts", "<cmd>set spell!<cr>", desc = "Spell check" },
      { "<leader>th", function() require("close_buffers").delete({ type = "hidden" }) end, desc = "Close hidden bufs" },

      -- === TROUBLE ===
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics" },
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer diagnostics" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location list" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix" },
      { "<leader>xt", "<cmd>Trouble todo toggle<cr>", desc = "TODOs" },

      -- === YANK ===
      { "<leader>yf", function()
          local path = vim.api.nvim_buf_get_name(0)
          vim.fn.setreg("+", path)
          vim.notify("Yanked: " .. path)
        end, desc = "Yank file path" },
      { "<leader>yr", function()
          local path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":~:.")
          vim.fn.setreg("+", path)
          vim.notify("Yanked: " .. path)
        end, desc = "Yank relative path" },

      -- === WINDOW ===
      { "<leader>wv", "<cmd>vsplit<cr>", desc = "Split vertical" },
      { "<leader>ws", "<cmd>split<cr>", desc = "Split horizontal" },
      { "<leader>wc", "<cmd>close<cr>", desc = "Close window" },
      { "<leader>wo", "<cmd>only<cr>", desc = "Close others" },
      { "<leader>w=", "<C-w>=", desc = "Equal size" },

      -- === QUIT/WRITE ===
      { "<leader>qq", "<cmd>qa<cr>", desc = "Quit all" },
      { "<leader>qw", "<cmd>wqa<cr>", desc = "Write & quit all" },
      { "<leader>ww", "<cmd>w<cr>", desc = "Save" },

      -- === MISC ===
      { "<leader>/", function() require("Comment.api").toggle.linewise.current() end, desc = "Comment line" },
      { "<leader>e", "<cmd>Oil<cr>", desc = "File explorer (Oil)" },
      { "<leader>u", "<cmd>UndotreeToggle<cr>", desc = "Undo tree" },
    })

    -- Also register the non-leader prefixes for their own menus
    wk.add({
      { "[", group = "Previous" },
      { "]", group = "Next" },
      { "g", group = "Go/LSP" },
      { "z", group = "Folds" },
      { "[d", desc = "Prev diagnostic" },
      { "[b", desc = "Prev buffer" },
      { "[q", desc = "Prev quickfix" },
      { "[t", desc = "Prev todo" },
      { "]d", desc = "Next diagnostic" },
      { "]b", desc = "Next buffer" },
      { "]q", desc = "Next quickfix" },
      { "]t", desc = "Next todo" },
      { "gd", desc = "Definition" },
      { "gr", desc = "References" },
      { "gi", desc = "Implementation" },
      { "gc", desc = "Comment" },
      { "gcc", desc = "Comment line" },
      { "zR", desc = "Open all folds" },
      { "zM", desc = "Close all folds" },
      { "za", desc = "Toggle fold" },
      { "s", desc = "Flash (jump)" },
      { "S", desc = "Flash Treesitter" },
      { "K", desc = "Hover docs" },
    })
  end,
}
