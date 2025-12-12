return {
  {
    "stevearc/conform.nvim",
    -- event = 'BufWritePre', -- uncomment for format on save
    opts = require "configs.conform",
  },

  -- Alpha-nvim dashboard
  require "plugins.alpha",

  -- which-key.nvim: Full panel mode for rehearsal and overview
  require "plugins.which-key",

  -- vim-repeat: Makes more commands repeatable with .
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },

  -- neocodeium: Free AI completion (Windsurf/Codeium)
  require "plugins.neocodeium",

  -- vim-tmux-navigator for seamless navigation
  require "plugins.vim-tmux-navigator",

  -- lazygit.nvim: Floating terminal wrapper for lazygit
  require "plugins.lazygit",

  -- trouble.nvim: Better diagnostics/quickfix management
  require "plugins.trouble",

  -- todo-comments.nvim: Highlight and navigate TODO comments
  require "plugins.todo-comments",

  -- refactoring.nvim: Code refactoring tools
  require "plugins.refactoring",

  -- ufo.nvim: Better folding with Treesitter support
  require "plugins.ufo",

  -- oil.nvim: Modern file explorer (replaces netrw)
  require "plugins.oil",

  -- flash.nvim: Enhanced navigation
  require "plugins.flash",

  -- grug-far.nvim: Find and replace with preview
  require "plugins.grug-far",

  -- gitlinker.nvim: Generate GitHub/GitLab links
  require "plugins.gitlinker",

  -- indent-o-matic: Auto-detect indentation
  require "plugins.indent-o-matic",

  -- harpoon2: Fast file navigation (ThePrimeagen)
  require "plugins.harpoon",

  -- treesitter-textobjects: Smart text objects based on AST
  require "plugins.treesitter-textobjects",

  -- mini.hipatterns: Highlight colors and Tailwind classes
  {
    "echasnovski/mini.hipatterns",
    event = { "BufReadPost", "BufNewFile" },
    opts = function()
      local hi = require("mini.hipatterns")
      return {
        tailwind = {
          enabled = true,
          ft = {
            "astro",
            "css",
            "html",
            "javascript",
            "javascriptreact",
            "typescript",
            "typescriptreact",
            "vue",
          },
          style = "full", -- highlights whole class, or "compact" for just color
        },
        highlighters = {
          hex_color = hi.gen_highlighter.hex_color({ priority = 2000 }),
        },
      }
    end,
    config = function(_, opts)
      require("mini.hipatterns").setup(opts)
    end,
  },

  -- tailwindcss-colorizer-cmp: Show Tailwind colors in completion menu
  {
    "roobert/tailwindcss-colorizer-cmp.nvim",
    dependencies = { "hrsh7th/nvim-cmp" },
    config = function()
      -- Integration with nvim-cmp will be done via NvChad's cmp config override
    end,
  },

  -- crates.nvim: Rust Cargo.toml completions
  {
    "Saecki/crates.nvim",
    event = "BufRead Cargo.toml",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup()
    end,
  },

  -- inc-rename.nvim: Incremental LSP renaming with live preview
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = function()
      require("inc_rename").setup()
    end,
  },

  -- dial.nvim: Better increment/decrement (dates, hex, semver, booleans, custom constants)
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

  -- mini.bracketed: Navigate with brackets (files, windows, quickfix, yanks, treesitter)
  {
    "nvim-mini/mini.bracketed",
    event = "BufReadPost",
    config = function()
      local bracketed = require("mini.bracketed")
      bracketed.setup({
        file = { suffix = "" },
        window = { suffix = "" },
        quickfix = { suffix = "" },
        yank = { suffix = "" },
        treesitter = { suffix = "n" },
      })
    end,
  },

  -- git.nvim: Git blame and browse
  -- Note: lazygit.nvim uses <leader>gg, so git.nvim uses different bindings
  {
    "dinhhuy258/git.nvim",
    event = "BufReadPre",
    opts = {
      keymaps = {
        -- Open blame window
        blame = "<Leader>gb",
        -- Open file/folder in git repository
        browse = "<Leader>go",
      },
    },
  },

  -- close-buffers.nvim: Close hidden/nameless buffers
  {
    "kazhala/close-buffers.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>th",
        function()
          require("close_buffers").delete({ type = "hidden" })
        end,
        desc = "Close Hidden Buffers",
      },
      {
        "<leader>tu",
        function()
          require("close_buffers").delete({ type = "nameless" })
        end,
        desc = "Close Nameless Buffers",
      },
    },
  },

  -- Ensure Mason loads early (NvChad loads it lazily, but we need it for auto-installation)
  {
    "mason-org/mason.nvim",
    -- Override NvChad's lazy loading - load on FilePost so it's available when we need it
    event = "User FilePost",
    opts = function()
      local mason_opts = require "nvchad.configs.mason"
      -- Add formatters for conform.nvim (these will be auto-installed)
      mason_opts.ensure_installed = mason_opts.ensure_installed or {}
      vim.list_extend(mason_opts.ensure_installed, {
        "prettierd",  -- Prettier daemon (faster than prettier) for JS/TS/CSS/HTML/JSON/Markdown
        "stylua",     -- For Lua
      })
      return mason_opts
    end,
  },

  -- mason-lspconfig: bridges Mason with lspconfig for auto-installation
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      "mason-org/mason.nvim",  -- Ensure Mason is loaded first
    },
    -- Load after Mason is set up - use same event as lspconfig
    event = "User FilePost",
    config = function()
      -- LSP servers to automatically install
      local servers = {
        -- Web development
        "html",           -- HTML
        "cssls",          -- CSS
        "tailwindcss",    -- Tailwind CSS
        "ts_ls",          -- TypeScript & JavaScript (handles React, Next.js, etc.)
        "eslint",         -- ESLint for JS/TS
        
        -- Systems & compiled languages
        "rust_analyzer",  -- Rust
        "clangd",         -- C/C++
        "gopls",          -- Go
        "taplo",          -- TOML (Cargo.toml, etc.)
      }
      
      -- Wait for Mason to be ready, then set up mason-lspconfig
      local function setup()
        local mason_ok, mason = pcall(require, "mason")
        if not mason_ok then
          vim.defer_fn(setup, 100)
          return
        end
        
        -- Ensure Go is in PATH for gopls installation (Mason uses 'go install' for gopls)
        -- Prioritize WSL Go installation over Windows Go for better compatibility with Mason
        local current_path = vim.env.PATH or ""
        local path_sep = vim.fn.has("win32") == 1 and ";" or ":"
        
        -- Check for WSL Go installations first (preferred for Mason)
        local wsl_go_paths = {
          "/usr/bin",              -- WSL system Go (pacman install) - highest priority
          "/usr/local/go/bin",     -- WSL user-installed Go
          os.getenv("HOME") .. "/.local/go/bin",  -- User-installed Go in WSL
        }
        
        local wsl_go_found = false
        for _, go_path in ipairs(wsl_go_paths) do
          if vim.fn.executable(go_path .. "/go") == 1 then
            -- Prepend WSL Go to PATH so it's found first
            if not string.find(current_path, go_path, 1, true) then
              vim.env.PATH = go_path .. path_sep .. current_path
            end
            wsl_go_found = true
            break
          end
        end
        
        -- Fallback to Windows Go if WSL Go not found
        if not wsl_go_found then
          local win_go_path = "C:\\Program Files\\Go\\bin"
          if vim.fn.isdirectory(win_go_path) == 1 then
            if not string.find(current_path, "Go\\bin", 1, true) then
              vim.env.PATH = win_go_path .. path_sep .. current_path
            end
          end
        end
        
        -- Final check and warn if Go still not found
        if vim.fn.executable("go") == 0 then
          vim.notify(
            "[mason-lspconfig] Go not found. gopls installation will fail. Run install.sh to install Go automatically.",
            vim.log.levels.WARN
          )
        end
        
        -- Ensure Mason is set up
        if not mason.has_setup then
          mason.setup({})
        end
        
          -- Set up mason-lspconfig (it will refresh registry and install packages)
          local ok, mason_lspconfig = pcall(require, "mason-lspconfig")
          if ok then
            mason_lspconfig.setup({
              ensure_installed = servers,
              -- automatic_enable is true by default
            })
            
            -- Set up error handling and retry logic for failed installations
            -- Note: Most Mason packages are pre-built binaries, but gopls requires Go to be installed
            -- (Mason uses 'go install' for gopls). Go is installed on Windows and should be in PATH.
            local mr = require("mason-registry")
            local mappings = require("mason-lspconfig.mappings")
            
            -- Track which packages failed to install
            local failed_packages = {}
            
            mr:on("package:install:failure", function(pkg)
              failed_packages[pkg.name] = true
              -- Retry installation after a delay for network issues
              vim.defer_fn(function()
                if not pkg:is_installed() and not pkg:is_installing() then
                  vim.notify(
                    string.format("[mason-lspconfig] Retrying installation of %s...", pkg.name),
                    vim.log.levels.INFO
                  )
                  pkg:install()
                end
              end, 3000) -- Retry after 3 seconds
            end)
            
            -- Also check and retry any failed installations after a delay
            vim.defer_fn(function()
              local server_mapping = mappings.get_mason_map()
              for _, server in ipairs(servers) do
                local pkg_name = server_mapping.lspconfig_to_package[server]
                if pkg_name then
                  local pkg = mr.get_package(pkg_name)
                  if pkg and not pkg:is_installed() and not pkg:is_installing() then
                    vim.defer_fn(function()
                      vim.notify(
                        string.format("[mason-lspconfig] Retrying installation of %s (%s)...", server, pkg_name),
                        vim.log.levels.INFO
                      )
                      pkg:install()
                    end, 5000) -- Stagger retries
                  end
                end
              end
            end, 8000) -- Wait 8 seconds after initial setup to allow first attempt to complete
        else
          vim.defer_fn(setup, 100)
        end
      end
      
      -- Start setup
      vim.schedule(setup)
    end,
  },

  -- LSP configuration with Mason auto-installation
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "mason-org/mason-lspconfig.nvim",
    },
    -- Ensure this loads after Mason (which NvChad provides)
    event = "User FilePost",
    config = function()
      require "configs.lspconfig"
    end,
  },

  -- Override nvim-cmp to integrate tailwindcss-colorizer-cmp
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "roobert/tailwindcss-colorizer-cmp.nvim",
    },
    opts = function()
      return require "configs.cmp"
    end,
  },

  -- Telescope file browser: Better file navigation than netrw
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },

  -- Treesitter playground: Visualize syntax tree (debugging)
  {
    "nvim-treesitter/playground",
    cmd = "TSPlaygroundToggle",
  },

  -- Treesitter for syntax highlighting and more
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPost", "BufNewFile" },
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
    build = ":TSUpdate",
    opts = function()
      local nvchad_opts = require "nvchad.configs.treesitter"
      -- Extend NvChad's defaults with our additional parsers
      return vim.tbl_deep_extend("force", nvchad_opts, {
        ensure_installed = {
          -- NvChad defaults
          "lua", "luadoc", "printf", "vim", "vimdoc",
          -- Web development
          "html", "css", "javascript", "typescript", "tsx", "jsx", "json",
          -- Systems & compiled languages
          "rust", "cpp", "c", "go",
          -- Markup & config
          "markdown", "yaml", "toml", "sql",
        },
        -- Auto-install missing parsers
        auto_install = true,
        -- Query linter: Catch errors in Treesitter queries
        query_linter = {
          enable = true,
          use_virtual_text = true,
          lint_events = { "BufWrite", "CursorHold" },
        },
        -- Playground: Visualize syntax tree
        playground = {
          enable = true,
          disable = {},
          updatetime = 25, -- Debounced time for highlighting nodes
          persist_queries = true, -- Query persists across sessions
          keybindings = {
            toggle_query_editor = "o",
            toggle_hl_groups = "i",
            toggle_injected_languages = "t",
            toggle_anonymous_nodes = "a",
            toggle_language_display = "I",
            focus_language = "f",
            unfocus_language = "F",
            update = "R",
            goto_node = "<cr>",
            show_help = "?",
          },
        },
      })
    end,
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
}
