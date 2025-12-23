-- Hydra.nvim: Comprehensive demo showing all features
-- Main menu at <leader>H with submenus for different operations

return {
  "nvimtools/hydra.nvim",
  lazy = false,
  config = function()
    local Hydra = require("hydra")

    -- ============================================
    -- SUBMENU: Window Management (pink = sticky)
    -- Position: middle-right, stays active until Esc
    -- ============================================
    local window_hydra = Hydra({
      name = "Window",
      hint = [[
 _h_ _j_ _k_ _l_  navigate
 _H_ _J_ _K_ _L_  move window
 _=_ equalize  _o_ only
 _s_ split     _v_ vsplit
 _c_ close     _q_ quit
 ^
 _<Esc>_ back
]],
      config = {
        color = "pink",  -- stays active, foreign keys work
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "middle-right",
          float_opts = { border = "rounded" },
        },
      },
      mode = "n",
      body = "<leader>Hw",  -- accessed from main menu
      heads = {
        -- Navigate
        { "h", "<C-w>h" },
        { "j", "<C-w>j" },
        { "k", "<C-w>k" },
        { "l", "<C-w>l" },
        -- Move window
        { "H", "<C-w>H" },
        { "J", "<C-w>J" },
        { "K", "<C-w>K" },
        { "L", "<C-w>L" },
        -- Actions
        { "=", "<C-w>=" },
        { "o", "<cmd>only<cr>" },
        { "s", "<cmd>split<cr>" },
        { "v", "<cmd>vsplit<cr>" },
        { "c", "<cmd>close<cr>", { exit = true } },
        { "q", "<cmd>qa<cr>", { exit = true } },
        { "<Esc>", nil, { exit = true } },
      },
    })

    -- ============================================
    -- SUBMENU: Resize (amaranth = trapped, must Esc)
    -- Position: bottom, disallows foreign keys
    -- ============================================
    local resize_hydra = Hydra({
      name = "Resize",
      hint = [[
 _h_ narrower   _l_ wider
 _j_ shorter    _k_ taller
 _=_ equalize
 ^
 _<Esc>_ back
]],
      config = {
        color = "amaranth",  -- trapped! foreign keys blocked
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          float_opts = { border = "double" },  -- double border = trapped!
        },
      },
      mode = "n",
      body = "<leader>Hr",
      heads = {
        { "h", "<C-w><" },
        { "l", "<C-w>>" },
        { "j", "<C-w>-" },
        { "k", "<C-w>+" },
        { "=", "<C-w>=" },
        { "<Esc>", nil, { exit = true } },
      },
    })

    -- ============================================
    -- SUBMENU: Scroll (teal = trapped, exits on action)
    -- Position: top-right
    -- ============================================
    local scroll_hydra = Hydra({
      name = "Scroll",
      hint = [[
 _j_ _k_  scroll line
 _d_ _u_  scroll half
 _f_ _b_  scroll page
 _g_ top  _G_ bottom
 ^
 _<Esc>_ back
]],
      config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "top-right",
          float_opts = { border = "rounded" },
        },
      },
      mode = "n",
      body = "<leader>Hs",
      heads = {
        { "j", "<C-e>" },
        { "k", "<C-y>" },
        { "d", "<C-d>" },
        { "u", "<C-u>" },
        { "f", "<C-f>" },
        { "b", "<C-b>" },
        { "g", "gg", { exit = true } },
        { "G", "G", { exit = true } },
        { "<Esc>", nil, { exit = true } },
      },
    })

    -- ============================================
    -- SUBMENU: Options toggle (blue = one-shot)
    -- Each action exits immediately
    -- ============================================
    local options_hydra = Hydra({
      name = "Options",
      hint = [[
 _n_ numbers    _r_ relative
 _w_ wrap       _s_ spell
 _l_ list       _c_ cursorline
 _h_ hlsearch
 ^
 _<Esc>_ back
]],
      config = {
        color = "blue",  -- exits after each action
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "middle",
          float_opts = { border = "rounded" },
        },
      },
      mode = "n",
      body = "<leader>Ho",
      heads = {
        { "n", "<cmd>set number!<cr>" },
        { "r", "<cmd>set relativenumber!<cr>" },
        { "w", "<cmd>set wrap!<cr>" },
        { "s", "<cmd>set spell!<cr>" },
        { "l", "<cmd>set list!<cr>" },
        { "c", "<cmd>set cursorline!<cr>" },
        { "h", "<cmd>set hlsearch!<cr>" },
        { "<Esc>", nil, { exit = true } },
      },
    })

    -- ============================================
    -- MAIN MENU: Demo hub (red = foreign keys exit)
    -- Position: bottom-left
    -- ============================================
    Hydra({
      name = "DEMO",
      hint = [[
 HYDRA DEMO - Submenus & Features

 _w_ Window     (pink - sticky, middle-right)
 _r_ Resize     (amaranth - trapped, bottom)
 _s_ Scroll     (pink - sticky, top-right)
 _o_ Options    (blue - one-shot, middle)
 ^
 _g_ Git        (original test hydra)
 ^
 _<Esc>_ exit
]],
      config = {
        color = "red",  -- foreign keys allowed but exit
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom-left",
          float_opts = { border = "rounded" },
        },
      },
      mode = "n",
      body = "<leader>H",
      heads = {
        { "w", "<leader>Hw", { exit = true, desc = "Window submenu" } },
        { "r", "<leader>Hr", { exit = true, desc = "Resize submenu" } },
        { "s", "<leader>Hs", { exit = true, desc = "Scroll submenu" } },
        { "o", "<leader>Ho", { exit = true, desc = "Options submenu" } },
        { "g", "<leader>G", { exit = true, desc = "Git hydra" } },
        { "<Esc>", nil, { exit = true } },
      },
    })

    -- ============================================
    -- ORIGINAL: Git operations (keep for reference)
    -- ============================================
    Hydra({
      name = "Git",
      hint = [[
 _b_ blame     _d_ diff
 _s_ status    _c_ commits
 _l_ lazygit
 ^
 _<Esc>_ exit
]],
      config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
          type = "window",
          position = "bottom",
          float_opts = { border = "rounded" },
        },
      },
      mode = "n",
      body = "<leader>G",
      heads = {
        { "b", "<cmd>Git blame<cr>", { exit = true } },
        { "d", "<cmd>Gitsigns diffthis<cr>" },
        { "s", "<cmd>Telescope git_status<cr>", { exit = true } },
        { "c", "<cmd>Telescope git_commits<cr>", { exit = true } },
        { "l", "<cmd>LazyGit<cr>", { exit = true } },
        { "<Esc>", nil, { exit = true } },
      },
    })
  end,
}
