-- Hydra.nvim: Sticky submodes for repetitive workflows
-- Press <leader>g/d/w/b/f to enter a mode, use single keys, <Esc> to exit

return {
  "nvimtools/hydra.nvim",
  lazy = false,  -- Load immediately - handles core keybindings
  config = function()
    local Hydra = require("hydra")

    -- ============================================
    -- GIT HYDRA
    -- ============================================
    Hydra({
      name = "Git",
      hint = [[
 _b_: blame    _d_: diff     _l_: lazygit
 _s_: status   _c_: commits  _w_: workflow
 _p_: push     _P_: pull
 ^
 _<Esc>_: exit
      ]],
      config = {
        color = "red",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          border = "rounded",
        },
        on_enter = function()
          vim.notify("Git mode", vim.log.levels.INFO)
        end,
      },
      mode = "n",
      body = "<leader>g",
      heads = {
        { "b", "<cmd>Git blame<cr>", { desc = "Blame", exit = true } },
        { "d", "<cmd>Gitsigns diffthis<cr>", { desc = "Diff" } },
        { "l", "<cmd>LazyGit<cr>", { desc = "LazyGit", exit = true } },
        { "s", "<cmd>Telescope git_status<cr>", { desc = "Status", exit = true } },
        { "c", "<cmd>Telescope git_commits<cr>", { desc = "Commits", exit = true } },
        { "w", "<cmd>edit /home/bw/dotfiles/docs/CURSOR-WORKFLOW-GUIDE.md<cr>", { desc = "Workflow", exit = true } },
        { "p", "<cmd>!git push<cr>", { desc = "Push", exit = true } },
        { "P", "<cmd>!git pull<cr>", { desc = "Pull", exit = true } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })

    -- ============================================
    -- DIAGNOSTICS HYDRA
    -- ============================================
    Hydra({
      name = "Diagnostics",
      hint = [[
 _n_/_]_: next     _p_/_[_: prev
 _f_: float        _l_: list (telescope)
 _x_: trouble      _q_: quickfix
 ^
 _<Esc>_: exit
      ]],
      config = {
        color = "red",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          border = "rounded",
        },
      },
      mode = "n",
      body = "<leader>d",
      heads = {
        { "n", vim.diagnostic.goto_next, { desc = "Next" } },
        { "]", vim.diagnostic.goto_next, { desc = "Next" } },
        { "p", vim.diagnostic.goto_prev, { desc = "Prev" } },
        { "[", vim.diagnostic.goto_prev, { desc = "Prev" } },
        { "f", vim.diagnostic.open_float, { desc = "Float" } },
        { "l", "<cmd>Telescope diagnostics<cr>", { desc = "List", exit = true } },
        { "x", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Trouble", exit = true } },
        { "q", "<cmd>Trouble qflist toggle<cr>", { desc = "Quickfix", exit = true } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })

    -- ============================================
    -- WINDOW HYDRA
    -- ============================================
    Hydra({
      name = "Window",
      hint = [[
 _v_: vsplit   _s_: hsplit   _c_: close    _o_: only
 _=_: equal    _w_: save
 ^
 Navigate: _h_ _j_ _k_ _l_
 Resize:   _H_ _J_ _K_ _L_
 ^
 _<Esc>_: exit
      ]],
      config = {
        color = "red",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          border = "rounded",
        },
      },
      mode = "n",
      body = "<leader>w",
      heads = {
        { "v", "<cmd>vsplit<cr>", { desc = "Vsplit" } },
        { "s", "<cmd>split<cr>", { desc = "Hsplit" } },
        { "c", "<cmd>close<cr>", { desc = "Close", exit = true } },
        { "o", "<cmd>only<cr>", { desc = "Only", exit = true } },
        { "=", "<C-w>=", { desc = "Equal" } },
        { "w", "<cmd>w<cr>", { desc = "Save" } },
        -- Navigate
        { "h", "<C-w>h", { desc = "Left" } },
        { "j", "<C-w>j", { desc = "Down" } },
        { "k", "<C-w>k", { desc = "Up" } },
        { "l", "<C-w>l", { desc = "Right" } },
        -- Resize
        { "H", "<cmd>vertical resize -5<cr>", { desc = "Shrink width" } },
        { "L", "<cmd>vertical resize +5<cr>", { desc = "Grow width" } },
        { "J", "<cmd>resize -3<cr>", { desc = "Shrink height" } },
        { "K", "<cmd>resize +3<cr>", { desc = "Grow height" } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })

    -- ============================================
    -- BUFFER HYDRA
    -- ============================================
    Hydra({
      name = "Buffer",
      hint = [[
 _n_/_l_: next     _p_/_h_: prev
 _d_: delete       _b_: list
 ^
 _<Esc>_: exit
      ]],
      config = {
        color = "red",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          border = "rounded",
        },
      },
      mode = "n",
      body = "<leader>b",
      heads = {
        { "n", "<cmd>bnext<cr>", { desc = "Next" } },
        { "l", "<cmd>bnext<cr>", { desc = "Next" } },
        { "p", "<cmd>bprev<cr>", { desc = "Prev" } },
        { "h", "<cmd>bprev<cr>", { desc = "Prev" } },
        { "d", "<cmd>bdelete<cr>", { desc = "Delete" } },
        { "b", "<cmd>Telescope buffers<cr>", { desc = "List", exit = true } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })

    -- ============================================
    -- FIND/TELESCOPE HYDRA (pink - exits on action)
    -- ============================================
    Hydra({
      name = "Find",
      hint = [[
 _f_: files    _g_: grep     _b_: buffers
 _h_: help     _o_: recent   _k_: keymaps
 _t_: todos    _c_: commits  _s_: git status
 ^
 _<Esc>_: exit
      ]],
      config = {
        color = "pink",  -- exits after any non-head key
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          border = "rounded",
        },
      },
      mode = "n",
      body = "<leader>f",
      heads = {
        { "f", "<cmd>Telescope find_files<cr>", { desc = "Files" } },
        { "g", "<cmd>Telescope live_grep<cr>", { desc = "Grep" } },
        { "b", "<cmd>Telescope buffers<cr>", { desc = "Buffers" } },
        { "h", "<cmd>Telescope help_tags<cr>", { desc = "Help" } },
        { "o", "<cmd>Telescope oldfiles<cr>", { desc = "Recent" } },
        { "k", "<cmd>Telescope keymaps<cr>", { desc = "Keymaps" } },
        { "t", "<cmd>TodoTelescope<cr>", { desc = "TODOs" } },
        { "c", "<cmd>Telescope git_commits<cr>", { desc = "Commits" } },
        { "s", "<cmd>Telescope git_status<cr>", { desc = "Git status" } },
        { "<Esc>", nil, { exit = true, nowait = true, desc = "Exit" } },
      },
    })
  end,
}
