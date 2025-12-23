-- Lualine with reactive hints
-- Shows nothing normally, shows hints after prefix keypress, clears after action

return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "VeryLazy",
  config = function()
    -- State for reactive hints
    local pending_prefix = nil
    local hint_text = ""

    -- Hint definitions for each prefix
    local prefix_hints = {
      ["g"] = "d=def r=refs i=impl c=comment D=decl",
      ["["] = "d=diag b=buf q=qfix t=todo",
      ["]"] = "d=diag b=buf q=qfix t=todo",
      ["z"] = "a=toggle R=open-all M=close-all o=open c=close",
    }

    -- Track keypresses to detect prefix and completion
    vim.on_key(function(key)
      local char = vim.fn.keytrans(key)

      -- Only in normal mode
      if vim.fn.mode() ~= "n" then
        if pending_prefix ~= nil then
          pending_prefix = nil
          hint_text = ""
          vim.schedule(function() vim.cmd("redrawstatus") end)
        end
        return
      end

      -- Check if this is a prefix key we care about
      if prefix_hints[char] and pending_prefix == nil then
        pending_prefix = char
        hint_text = char .. ": " .. prefix_hints[char]
        vim.schedule(function() vim.cmd("redrawstatus") end)
      elseif pending_prefix ~= nil then
        -- A second key was pressed, action complete
        pending_prefix = nil
        hint_text = ""
        vim.schedule(function() vim.cmd("redrawstatus") end)
      end
    end, nil)

    -- Also clear on mode change
    vim.api.nvim_create_autocmd("ModeChanged", {
      pattern = "*",
      callback = function()
        pending_prefix = nil
        hint_text = ""
      end,
    })

    -- Reactive hints component
    local function reactive_hints()
      -- Show Hydra status if active
      local ok, hydra_statusline = pcall(require, "hydra.statusline")
      if ok and hydra_statusline.is_active() then
        return hydra_statusline.get_name() .. " MODE"
      end
      -- Otherwise show pending prefix hints (or nothing)
      return hint_text
    end

    require("lualine").setup({
      options = {
        theme = "vscode",
        globalstatus = true,
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = {
          {
            reactive_hints,
            color = function()
              local ok, hydra_statusline = pcall(require, "hydra.statusline")
              if ok and hydra_statusline.is_active() then
                return { fg = "#ff9e64", gui = "bold" }
              end
              if hint_text ~= "" then
                return { fg = "#7aa2f7" }
              end
              return nil
            end,
          },
          "filetype",
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      extensions = { "lazy", "mason", "oil", "trouble" },
    })
  end,
}
