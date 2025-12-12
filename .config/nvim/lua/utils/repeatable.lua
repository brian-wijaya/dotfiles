-- Utility to make navigation commands repeatable with .
-- Provides a consistent way to make any command repeatable
local M = {}

-- Make a function repeatable using vim-repeat
-- @param fn: Function to execute
-- @param cmd_string: String representation of the command (for repeat#set)
function M.make_repeatable(fn, cmd_string)
  return function()
    fn()
    -- Use vim-repeat if available
    if vim.fn.exists('*repeat#set') == 1 then
      -- Get the command string or construct it
      local cmd = cmd_string or vim.fn.getchar()
      vim.fn['repeat#set'](cmd, -1)
    end
  end
end

-- Make percentage jumps repeatable
-- Overrides % to be repeatable when used with count (e.g., 10%)
function M.setup_percent_repeat()
  -- Override % to be repeatable when used with count
  vim.keymap.set('n', '%', function()
    local count = vim.v.count
    if count > 0 then
      -- Has a count (e.g., 10%)
      -- Build the key sequence: count + %
      local keys = tostring(count) .. '%'
      -- Execute the jump
      vim.cmd('normal! ' .. keys)
      -- Register with vim-repeat so . can repeat it
      -- We need to tell vim-repeat what to repeat
      if vim.fn.exists('*repeat#set') == 1 then
        -- Convert to proper key sequence format
        local keyseq = vim.api.nvim_replace_termcodes(keys, true, false, true)
        -- Set the repeat - this tells vim-repeat what to repeat when . is pressed
        vim.fn['repeat#set'](keyseq, -1)
      end
    else
      -- No count, use default % behavior (matching bracket)
      -- Feed the key normally so vim-repeat can track it naturally
      vim.fn.feedkeys('%', 'n')
    end
  end, { expr = false, desc = 'Jump to percentage or matching bracket' })
end

return M

