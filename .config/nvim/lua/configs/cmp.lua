-- Custom cmp config to integrate tailwindcss-colorizer-cmp
-- Get NvChad's default cmp config (it returns a table directly)
local default_opts = require "nvchad.configs.cmp"

-- Override formatting to include Tailwind colorizer
local original_format = default_opts.formatting and default_opts.formatting.format
default_opts.formatting = default_opts.formatting or {}
default_opts.formatting.format = function(entry, item)
  -- First apply NvChad's formatting (icons, etc.)
  if original_format then
    item = original_format(entry, item)
  end
  -- Then add Tailwind color preview
  local ok, colorizer = pcall(require, "tailwindcss-colorizer-cmp")
  if ok then
    item = colorizer.formatter(entry, item)
  end
  return item
end

return default_opts



