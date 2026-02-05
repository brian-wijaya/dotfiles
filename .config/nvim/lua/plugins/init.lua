return {
  -- ============================================
  -- CORE PLUGINS (previously provided by NvChad)
  -- ============================================

  -- Plenary: Lua functions library (dependency for many plugins)
  { "nvim-lua/plenary.nvim", lazy = true },

  -- Web devicons
  { "nvim-tree/nvim-web-devicons", lazy = true },

  -- Comment.nvim: Toggle comments
  {
    "numToStr/Comment.nvim",
    keys = {
      { "gcc", mode = "n", desc = "Comment toggle" },
      { "gc", mode = { "n", "v" }, desc = "Comment toggle" },
    },
    opts = {},
  },

  -- Gitsigns: Git integration
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      signs = {
        add = { text = "│" },
        change = { text = "│" },
        delete = { text = "󰍵" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
        untracked = { text = "│" },
      },
    },
  },

  -- Indent blankline: Show indent guides
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main = "ibl",
    opts = {
      indent = { char = "│" },
      scope = { enabled = true },
    },
  },

  -- Telescope: Fuzzy finder
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Telescope",
    keys = {
      {
        "<leader>ff",
        (function()
          -- Persistent hidden state across invocations
          local hidden_state = true
          
          return function()
            local git_root = vim.fn.systemlist("git rev-parse --show-toplevel 2>/dev/null")[1]
            local cwd = git_root and vim.fn.isdirectory(git_root) == 1 and git_root or vim.fn.getcwd()
            local builtin = require("telescope.builtin")
            local actions = require("telescope.actions")
            
            -- Helper to create find_files with toggle
            local function find_files_with_toggle(default_text)
              local find_cmd = { "fd", "--type", "f", "--strip-cwd-prefix" }
              if hidden_state then
                table.insert(find_cmd, "--hidden")
              end
              -- Exclude .git directory
              table.insert(find_cmd, "--exclude")
              table.insert(find_cmd, ".git")
              
              builtin.find_files({
                cwd = cwd,
                find_command = find_cmd,
                default_text = default_text or "",
                attach_mappings = function(prompt_bufnr, map)
                  -- Toggle hidden files with <C-h> (preserves query)
                  map("i", "<C-h>", function()
                    local state = require("telescope.actions.state")
                    local picker = state.get_current_picker(prompt_bufnr)
                    -- Get the current prompt text - try multiple methods
                    local current_query = ""
                    if picker._get_prompt then
                      current_query = picker:_get_prompt() or ""
                    elseif picker.get_prompt then
                      current_query = picker:get_prompt() or ""
                    else
                      -- Fallback: get from prompt buffer
                      local prompt_lines = vim.api.nvim_buf_get_lines(prompt_bufnr, 0, 1, false)
                      if prompt_lines and prompt_lines[1] then
                        current_query = prompt_lines[1]:gsub("^   ", "") -- Remove prompt prefix
                      end
                    end
                    hidden_state = not hidden_state
                    actions.close(prompt_bufnr)
                    -- Small delay to ensure close completes
                    vim.defer_fn(function()
                      find_files_with_toggle(current_query)
                    end, 50)
                  end)
                  return true
                end,
              })
            end
            
            find_files_with_toggle() -- Start with current hidden state
          end
        end)(),
        desc = "Find files (toggle hidden with <C-h>)",
      },
      { "<leader>fw", "<cmd>Telescope live_grep<CR>", desc = "Live grep" },
      { "<leader>fb", "<cmd>Telescope buffers<CR>", desc = "Buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<CR>", desc = "Help tags" },
      { "<leader>fo", "<cmd>Telescope oldfiles<CR>", desc = "Recent files" },
    },
    opts = {
      defaults = {
        prompt_prefix = "   ",
        selection_caret = " ",
        sorting_strategy = "ascending",
        file_ignore_patterns = { "%.git/" },
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = {
            prompt_position = "top",
            preview_width = 0.5,  -- Smaller preview to make room for full paths
            preview_cutoff = 80,
            width = 0.95,  -- Wider to accommodate full paths
            height = 0.9,
          },
          vertical = {
            preview_height = 0.5,
            preview_cutoff = 40,
          },
        },
        -- Enable word wrap for results list (so long paths wrap)
        wrap_results = true,
        -- Preview config (wrap handled by autocmd)
        preview = {
          treesitter = true,
        },
        -- Show full absolute paths (no truncation)
        path_display = { "absolute" },
      },
      pickers = {
        find_files = {
          find_command = { "fd", "--type", "f", "--hidden", "--strip-cwd-prefix", "--exclude", ".git" },
          path_display = { "absolute" },
        },
        live_grep = {
          path_display = { "absolute" },
          only_sort_text = true,
        },
      },
    },
  },

  -- LuaSnip: Snippet engine
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    event = "InsertEnter",
    config = function()
      require("luasnip").config.set_config({
        history = true,
        updateevents = "TextChanged,TextChangedI",
      })
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },

  -- nvim-cmp: Completion engine
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "L3MON4D3/LuaSnip",
      "roobert/tailwindcss-colorizer-cmp.nvim",
      "hrsh7th/cmp-cmdline",
    },
    config = function()
      local cmp = require("cmp")
      -- Insert mode completion
      cmp.setup(require("configs.cmp"))

      -- Cmdline mappings: Tab confirms, C-n/C-p navigate
      local cmdline_mapping = cmp.mapping.preset.cmdline({
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.confirm({ select = true })
          else
            fallback()
          end
        end, { "c" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end, { "c" }),
      })

      -- Command line `:` completion
      cmp.setup.cmdline(":", {
        mapping = cmdline_mapping,
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })

      -- Search `/` and `?` completion
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmdline_mapping,
        sources = {
          { name = "buffer" },
        },
      })
    end,
  },

  -- Mason: Package manager for LSP servers, formatters, linters
  {
    "mason-org/mason.nvim",
    cmd = { "Mason", "MasonInstall", "MasonUpdate" },
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
      ensure_installed = {
        "prettierd",
        "stylua",
      },
    },
    config = function(_, opts)
      require("mason").setup(opts)
      -- Install ensure_installed packages
      local mr = require("mason-registry")
      mr.refresh(function()
        for _, tool in ipairs(opts.ensure_installed or {}) do
          local p = mr.get_package(tool)
          if not p:is_installed() then
            p:install()
          end
        end
      end)
    end,
  },

  -- mason-lspconfig: Bridge Mason with lspconfig
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = { "mason-org/mason.nvim" },
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      ensure_installed = {
        "html",
        "cssls",
        "tailwindcss",
        "ts_ls",
        "eslint",
        "rust_analyzer",
        "clangd",
        "gopls",
        "kotlin_language_server",
        "jdtls",
        "taplo",
        "lua_ls",
      },
    },
  },

  -- nvim-lspconfig: LSP configuration
  {
    "neovim/nvim-lspconfig",
    dependencies = { "mason-org/mason-lspconfig.nvim" },
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("configs.lspconfig")
    end,
  },

  -- Treesitter: Syntax highlighting and more
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPost", "BufNewFile" },
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
    build = ":TSUpdate",
    opts = {
      ensure_installed = {
        "lua", "luadoc", "vim", "vimdoc",
        "html", "css", "javascript", "typescript", "tsx", "json",
        "rust", "cpp", "c", "go", "kotlin", "java",
        "markdown", "markdown_inline", "yaml", "toml", "sql",
        "bash", "regex",
      },
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true },
    },
    main = "nvim-treesitter.config",
  },

  -- conform.nvim: Formatting
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    opts = require("configs.conform"),
  },

  -- ============================================
  -- USER PLUGINS
  -- ============================================

  -- Alpha-nvim dashboard
  require("plugins.alpha"),

  -- which-key.nvim
  require("plugins.which-key"),

  -- vim-repeat
  { "tpope/vim-repeat", event = "VeryLazy" },

  -- neocodeium: AI completion
  require("plugins.neocodeium"),

  -- vim-tmux-navigator
  require("plugins.vim-tmux-navigator"),

  -- lazygit.nvim
  require("plugins.lazygit"),

  -- trouble.nvim
  require("plugins.trouble"),

  -- todo-comments.nvim
  require("plugins.todo-comments"),

  -- refactoring.nvim
  require("plugins.refactoring"),

  -- ufo.nvim: Better folding
  require("plugins.ufo"),

  -- oil.nvim: File explorer
  require("plugins.oil"),

  -- flash.nvim: Enhanced navigation
  require("plugins.flash"),

  -- grug-far.nvim: Find and replace
  require("plugins.grug-far"),

  -- gitlinker.nvim
  require("plugins.gitlinker"),

  -- indent-o-matic
  require("plugins.indent-o-matic"),

  -- harpoon2
  require("plugins.harpoon"),

  -- treesitter-textobjects
  require("plugins.treesitter-textobjects"),

  -- mini.hipatterns: Highlight colors
  {
    "echasnovski/mini.hipatterns",
    event = { "BufReadPost", "BufNewFile" },
    opts = function()
      local hi = require("mini.hipatterns")
      return {
        highlighters = {
          hex_color = hi.gen_highlighter.hex_color({ priority = 2000 }),
        },
      }
    end,
  },

  -- tailwindcss-colorizer-cmp
  {
    "roobert/tailwindcss-colorizer-cmp.nvim",
    config = true,
  },

  -- crates.nvim: Rust Cargo.toml
  {
    "Saecki/crates.nvim",
    event = "BufRead Cargo.toml",
    config = true,
  },

  -- inc-rename.nvim
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },

  -- dial.nvim: Better increment/decrement
  {
    "monaqa/dial.nvim",
    keys = {
      { "<C-a>", function() return require("dial.map").inc_normal() end, expr = true, desc = "Increment" },
      { "<C-x>", function() return require("dial.map").dec_normal() end, expr = true, desc = "Decrement" },
    },
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
          augend.constant.new({ elements = { "let", "const" } }),
        },
      })
    end,
  },

  -- mini.bracketed
  {
    "echasnovski/mini.bracketed",
    event = "BufReadPost",
    config = function()
      require("mini.bracketed").setup({
        file = { suffix = "" },
        window = { suffix = "" },
        quickfix = { suffix = "" },
        yank = { suffix = "" },
        treesitter = { suffix = "n" },
      })
    end,
  },

  -- git.nvim
  {
    "dinhhuy258/git.nvim",
    event = "BufReadPre",
    opts = {
      keymaps = {
        blame = "<Leader>gb",
        browse = "<Leader>go",
      },
    },
  },

  -- close-buffers.nvim
  {
    "kazhala/close-buffers.nvim",
    keys = {
      { "<leader>th", function() require("close_buffers").delete({ type = "hidden" }) end, desc = "Close Hidden Buffers" },
      { "<leader>tu", function() require("close_buffers").delete({ type = "nameless" }) end, desc = "Close Nameless Buffers" },
    },
  },

  -- Telescope file browser
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    keys = {
      {
        "<leader>fe",
        function()
          local git_root = vim.fn.systemlist("git rev-parse --show-toplevel 2>/dev/null")[1]
          local path = git_root and vim.fn.isdirectory(git_root) == 1 and git_root or vim.fn.getcwd()
          require("telescope").extensions.file_browser.file_browser({ path = path })
        end,
        desc = "File browser",
      },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },

  -- Treesitter playground
  {
    "nvim-treesitter/playground",
    cmd = "TSPlaygroundToggle",
  },
}
