-- nvim-treesitter-textobjects: Smart text objects based on AST
-- Enables selecting/navigating code structures (functions, classes, blocks, etc.)
return {
  "nvim-treesitter/nvim-treesitter-textobjects",
  dependencies = { "nvim-treesitter/nvim-treesitter" },
  event = "VeryLazy",
  config = function()
    -- Safely require treesitter configs (may not be available if plugin not installed)
    local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
    if not ok then
      vim.notify("nvim-treesitter-textobjects: treesitter configs not available", vim.log.levels.WARN)
      return
    end
    ts_configs.setup({
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            -- Function/class selection
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            
            -- Parameter selection
            ["aa"] = "@parameter.outer",
            ["ia"] = "@parameter.inner",
            
            -- Block selection
            ["ab"] = "@block.outer",
            ["ib"] = "@block.inner",
            
            -- Call selection
            ["aC"] = "@call.outer",
            ["iC"] = "@call.inner",
            
            -- Statement selection
            ["as"] = "@statement.outer",
            
            -- Comment selection
            ["a/"] = "@comment.outer",
          },
        },
        move = {
          enable = true,
          set_jumps = true, -- Whether to set jumps in the jumplist
          goto_next_start = {
            ["]f"] = "@function.outer",
            ["]c"] = "@class.outer",
            ["]a"] = "@parameter.outer",
            ["]b"] = "@block.outer",
            ["]s"] = "@statement.outer",
          },
          goto_next_end = {
            ["]F"] = "@function.outer",
            ["]C"] = "@class.outer",
            ["]A"] = "@parameter.outer",
            ["]B"] = "@block.outer",
            ["]S"] = "@statement.outer",
          },
          goto_previous_start = {
            ["[f"] = "@function.outer",
            ["[c"] = "@class.outer",
            ["[a"] = "@parameter.outer",
            ["[b"] = "@block.outer",
            ["[s"] = "@statement.outer",
          },
          goto_previous_end = {
            ["[F"] = "@function.outer",
            ["[C"] = "@class.outer",
            ["[A"] = "@parameter.outer",
            ["[B"] = "@block.outer",
            ["[S"] = "@statement.outer",
          },
        },
        swap = {
          enable = true,
          swap_next = {
            ["<leader>a"] = "@parameter.inner",
          },
          swap_previous = {
            ["<leader>A"] = "@parameter.inner",
          },
        },
      },
    })
  end,
}

