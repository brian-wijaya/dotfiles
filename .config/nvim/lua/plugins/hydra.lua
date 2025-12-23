-- Hydra.nvim: TEST SETUP
-- Try <leader>G to see how Hydra "sticky mode" feels
-- This is just for testing - delete or expand later

return {
  "nvimtools/hydra.nvim",
  lazy = false,
  config = function()
    local Hydra = require("hydra")

    -- ============================================
    -- TEST HYDRA: Git operations
    -- Press <leader>G to enter, <Esc> to exit
    -- While in mode, single keys trigger actions
    -- ============================================
    Hydra({
      name = "TEST Git",
      hint = [[
  ┌─────────────────────────────────┐
  │  TEST HYDRA - Git Operations    │
  ├─────────────────────────────────┤
  │  _b_ blame     _d_ diff         │
  │  _s_ status    _c_ commits      │
  │  _l_ lazygit                    │
  │                                 │
  │  _<Esc>_ exit                   │
  └─────────────────────────────────┘
      ]],
      config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          float_opts = {
            border = "rounded",
          },
        },
      },
      mode = "n",
      body = "<leader>G",  -- Capital G to avoid conflict
      heads = {
        { "b", "<cmd>Git blame<cr>", { desc = "Blame", exit = true } },
        { "d", "<cmd>Gitsigns diffthis<cr>", { desc = "Diff" } },
        { "s", "<cmd>Telescope git_status<cr>", { desc = "Status", exit = true } },
        { "c", "<cmd>Telescope git_commits<cr>", { desc = "Commits", exit = true } },
        { "l", "<cmd>LazyGit<cr>", { desc = "LazyGit", exit = true } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })
  end,
}
