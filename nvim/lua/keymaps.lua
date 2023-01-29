-- [[ Basic Keymaps ]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ';'
vim.g.maplocalleader = ';'
-- My keymaps
vim.keymap.set('n','<leader>h', ':set hlsearch!<CR>')

--vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Tabs
vim.keymap.set('n','<A-t>', ':tabnew<CR>')
vim.keymap.set('n','<A-e>', ':tabnext<CR>')
vim.keymap.set('n','<A-q>', ':tabclose<CR>')
vim.keymap.set('n','dq', ':q!<CR>')
vim.keymap.set('n','wq', ':wq!<CR>')

-- terminal
--vim.keymap.set('n','<A-d>', ':belowright new term://fish<CR>')
vim.keymap.set('n','<A-d>', ':ToggleTerm <CR>')
vim.keymap.set('t','<Esc>','<C-\\><C-n>')

-- visual mode indenting
vim.keymap.set('v','<','<gv')
vim.keymap.set('v','>','>gv')

-- escape
vim.keymap.set('i','jj','<ESC>')

-- escape
vim.keymap.set('n','<Space>cc',':make -j8 <CR>');


-- Move Selected line  / block of text in Visual mode

vim.keymap.set("x","K",":move '<-2<CR>gv-gv")
vim.keymap.set("x","J",":move '>+1<CR>gv-gv")

-- Telescope 
--
local builtin = require('telescope.builtin')
vim.keymap.set('n','<c-p>',builtin.find_files,{})
vim.keymap.set('n','<c-h>',':Telescope help_tags<CR>',{})
vim.keymap.set('n','<Space>fg',builtin.live_grep,{})



-- Keymaps for better default experience
-- See `:help vim.keymap.set()`

vim.keymap.set('n','<leader>s', ':NvimTreeToggle<CR>')
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

