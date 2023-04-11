

vim.g.mapleader = ';'
vim.g.maplocalleader = ';'

vim.keymap.set('n','<leader>h', ':set hlsearch!<CR>')

-- Tabs
vim.keymap.set('n','<A-t>', ':tabnew<CR>')
vim.keymap.set('n','<A-e>', ':tabnext<CR>')
vim.keymap.set('n','<A-q>', ':tabclose<CR>')
vim.keymap.set('n','dq', ':q!<CR>')
vim.keymap.set('n','wq', ':wq!<CR>')

-- terminal
vim.keymap.set('n','<A-d>', ':belowright new term://bash<CR>')
vim.keymap.set('n','<A-d>', ':ToggleTerm <CR>')
vim.keymap.set('t','<Esc>','<C-\\><C-n>')




-- visual mode indenting
vim.keymap.set('v','<','<gv')
vim.keymap.set('v','>','>gv')



-- Move Selected line  / block of text in Visual mode

vim.keymap.set("x","K",":move '<-2<CR>gv-gv")
vim.keymap.set("x","J",":move '>+1<CR>gv-gv")


-- Keymaps for better default experience
-- See `:help vim.keymap.set()`

vim.keymap.set('n','<leader>s', ':Telescope<CR>')
vim.keymap.set('n','<space>.', ':Dired<CR>')
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })


-- In insert mode, pressing ctrl + numpad's+ increases the font
--inoremap <Esc>:call AdjustFontSize(1)<CR>a
--inoremap <C-kMinus> <Esc>:call AdjustFontSize(-1)<CR>a

vim.keymap.set({'i','n'},'<C-kPlus>','<Esc>:call AdjustFontSize(1)<CR>a')
vim.keymap.set({'i','n'},'<C-kMinus>','<Esc>:call AdjustFontSize(-1)<CR>a')



