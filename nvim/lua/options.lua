-- Set highlight on search
vim.o.hlsearch = false
vim.opt.clipboard="unnamedplus"
-- Make line numbers default
vim.wo.number = true

vim.o.shell ="bash"

-- Enable mouse mode
vim.o.mouse = 'a'
vim.o.tabstop=4
vim.o.shiftwidth=4

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme
vim.o.termguicolors = true

-- Set completeopt to have a better completion experience
-- vim.o.completeopt = 'menu,menuone,noselect'

vim.o.guifont = "JetBrainsMono Nerd Font:h6"
vim.g.neovide_refresh_rate = 60
vim.g.neovide_fullscreen = true
vim.g.neovide_remember_window_size = true
vim.g.neovide_refresh_rate_idle = 6

vim.g.neovide_scale_factor = 1.0
local change_scale_factor = function(delta)
  vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
end
vim.keymap.set("n", "<C-=>", function()
  change_scale_factor(1.25)
end)
vim.keymap.set("n", "<C-->", function()
  change_scale_factor(1/1.25)
end)

