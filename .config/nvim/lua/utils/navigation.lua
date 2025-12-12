-- Navigation utilities for LSP results and file operations
local M = {}

-- State for LSP navigation (definitions, references, implementations)
local nav_state = {
  results = nil,
  current_index = 0,
  type = nil, -- 'definition', 'reference', 'implementation'
}

-- Navigate through LSP results (next/previous)
function M.navigate_lsp_results(direction)
  if not nav_state.results or #nav_state.results == 0 then
    vim.notify("No LSP results. Use gd/gr/gi first.", vim.log.levels.WARN)
    return
  end

  if direction == "next" then
    nav_state.current_index = (nav_state.current_index % #nav_state.results) + 1
  else
    nav_state.current_index = nav_state.current_index - 1
    if nav_state.current_index < 1 then
      nav_state.current_index = #nav_state.results
    end
  end

  local location = nav_state.results[nav_state.current_index]
  if location.uri then
    vim.api.nvim_command("edit " .. vim.uri_to_fname(location.uri))
    if location.range then
      vim.api.nvim_win_set_cursor(0, {
        location.range.start.line + 1,
        location.range.start.character,
      })
    end
    vim.notify(string.format("%s %d/%d", nav_state.type, nav_state.current_index, #nav_state.results), vim.log.levels.INFO)
  end
end

-- Store LSP results for navigation
function M.setup_nav(type, results)
  nav_state.results = results
  nav_state.current_index = 1
  nav_state.type = type
  if #results > 1 then
    vim.notify(string.format("%d %s found. Use ]d/[d to navigate.", #results, type), vim.log.levels.INFO)
  end
end

-- Enhanced definition with navigation
function M.goto_definition()
  vim.lsp.buf.definition(function(_, result)
    if result then
      if result[1] then
        M.setup_nav("definition", result)
        -- Jump to first result
        vim.lsp.util.jump_to_location(result[1], "utf-8")
      end
    end
  end)
end

-- Enhanced references with navigation
function M.goto_references()
  vim.lsp.buf.references(nil, function(_, result)
    if result then
      M.setup_nav("reference", result)
      if result[1] then
        vim.lsp.util.jump_to_location(result[1], "utf-8")
      end
    end
  end)
end

-- Enhanced implementation with navigation
function M.goto_implementation()
  vim.lsp.buf.implementation(function(_, result)
    if result then
      M.setup_nav("implementation", result)
      if result[1] then
        vim.lsp.util.jump_to_location(result[1], "utf-8")
      end
    end
  end)
end

-- Navigate to next/prev word with LSP definition
-- This finds the next occurrence of the word under cursor that has LSP info
function M.next_word_with_definition()
  local word = vim.fn.expand("<cword>")
  if word == "" then
    return
  end
  
  -- Enable document highlight to see all occurrences
  vim.lsp.buf.document_highlight()
  
  -- Search forward for the word
  vim.cmd("normal! *")
  
  -- Check if current word has LSP info
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
  if #clients > 0 then
    -- Try to get hover info to verify it has definition
    vim.lsp.buf.hover()
  end
end

function M.prev_word_with_definition()
  local word = vim.fn.expand("<cword>")
  if word == "" then
    return
  end
  
  vim.lsp.buf.document_highlight()
  vim.cmd("normal! #")
  
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
  if #clients > 0 then
    vim.lsp.buf.hover()
  end
end

return M

