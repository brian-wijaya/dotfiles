-- Smart cursor position history (Cursor-style)
-- Tracks where you pause/read, not just edits or explicit jumps
-- Alt+Left/Right to navigate back/forward

local M = {}

-- Configuration
local config = {
  max_history = 100,      -- max positions to remember
  min_distance = 5,       -- minimum line distance to record new position
}

-- State
local history = {}        -- list of {bufnr, lnum, col}
local index = 0           -- current position in history (0 = at head)
local last_recorded = nil -- last recorded position (for deduplication)
local ignore_next = false -- flag to ignore position after our own jump

-- Helper: get current position
local function get_pos()
  local bufnr = vim.api.nvim_get_current_buf()
  local pos = vim.api.nvim_win_get_cursor(0)
  return { bufnr = bufnr, lnum = pos[1], col = pos[2] }
end

-- Helper: check if position is valid (buffer still exists)
local function is_valid_pos(pos)
  return vim.api.nvim_buf_is_valid(pos.bufnr)
end

-- Helper: check if two positions are "close" (same file, within min_distance lines)
local function is_close(pos1, pos2)
  if not pos1 or not pos2 then return false end
  if pos1.bufnr ~= pos2.bufnr then return false end
  return math.abs(pos1.lnum - pos2.lnum) < config.min_distance
end

-- Record current position to history
local function record_position()
  if ignore_next then
    ignore_next = false
    return
  end

  local pos = get_pos()

  -- Skip special buffers
  local buftype = vim.bo[pos.bufnr].buftype
  if buftype ~= "" then return end

  -- Skip if too close to last recorded position
  if is_close(pos, last_recorded) then
    return
  end

  -- If we're in the middle of history (user navigated back), truncate forward history
  if index > 0 then
    for _ = 1, index do
      table.remove(history, 1)
    end
    index = 0
  end

  -- Add new position at the front
  table.insert(history, 1, pos)
  last_recorded = pos

  -- Trim history if too long
  while #history > config.max_history do
    table.remove(history)
  end
end

-- Jump backward in history
local function jump_back()
  if #history == 0 then
    vim.notify("No position history", vim.log.levels.INFO)
    return
  end

  if index >= #history - 1 then
    vim.notify("At oldest position", vim.log.levels.INFO)
    return
  end

  -- Record current position before jumping (if not already recorded)
  if index == 0 then
    local pos = get_pos()
    if not is_close(pos, history[1]) then
      table.insert(history, 1, pos)
      last_recorded = pos
      index = 1  -- we're now at position 1, not 0
    else
      index = index + 1
    end
  else
    index = index + 1
  end

  local target = history[index + 1]

  if not target or not is_valid_pos(target) then
    -- Skip invalid positions
    table.remove(history, index + 1)
    index = index - 1
    jump_back()
    return
  end

  ignore_next = true
  if vim.api.nvim_get_current_buf() ~= target.bufnr then
    vim.api.nvim_set_current_buf(target.bufnr)
  end
  vim.api.nvim_win_set_cursor(0, { target.lnum, target.col })
end

-- Jump forward in history
local function jump_forward()
  if index <= 0 then
    vim.notify("At newest position", vim.log.levels.INFO)
    return
  end

  index = index - 1
  local target = history[index + 1]

  if not target or not is_valid_pos(target) then
    -- Skip invalid positions
    table.remove(history, index + 1)
    jump_forward()
    return
  end

  ignore_next = true
  if vim.api.nvim_get_current_buf() ~= target.bufnr then
    vim.api.nvim_set_current_buf(target.bufnr)
  end
  vim.api.nvim_win_set_cursor(0, { target.lnum, target.col })
end

function M.setup()
  -- Set updatetime for faster CursorHold (default 4000ms is too slow)
  vim.opt.updatetime = 300

  -- Create autocommands
  local group = vim.api.nvim_create_augroup("SmartJump", { clear = true })

  -- Record on CursorHold (user paused)
  vim.api.nvim_create_autocmd("CursorHold", {
    group = group,
    callback = record_position,
  })

  -- Always record on file switch
  vim.api.nvim_create_autocmd("BufEnter", {
    group = group,
    callback = function()
      -- Small delay to let cursor position settle
      vim.defer_fn(record_position, 50)
    end,
  })

  -- Keymaps
  vim.keymap.set("n", "<A-Left>", jump_back, { desc = "Smart jump back" })
  vim.keymap.set("n", "<A-Right>", jump_forward, { desc = "Smart jump forward" })

  -- Also add to insert mode for convenience
  vim.keymap.set("i", "<A-Left>", function()
    vim.cmd("stopinsert")
    jump_back()
  end, { desc = "Smart jump back" })
  vim.keymap.set("i", "<A-Right>", function()
    vim.cmd("stopinsert")
    jump_forward()
  end, { desc = "Smart jump forward" })

  -- Debug command
  vim.api.nvim_create_user_command("SmartJumpHistory", function()
    if #history == 0 then
      print("No position history yet")
      return
    end
    print("History (" .. #history .. " positions, index=" .. index .. "):")
    for i, pos in ipairs(history) do
      local marker = (i == index + 1) and " <-- current" or ""
      local name = vim.api.nvim_buf_get_name(pos.bufnr)
      name = vim.fn.fnamemodify(name, ":~:.")
      if name == "" then name = "[No Name]" end
      print(string.format("  %d: %s:%d%s", i, name, pos.lnum, marker))
    end
  end, {})
end

return M
