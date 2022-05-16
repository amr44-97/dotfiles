
vim.keymap.set('n','<Space>', '<NOP>')
vim.g.mapleader = ';'
vim.keymap.set('n','<leader>h', ':set hlsearch!<CR>')


vim.keymap.set('n','dq', '<cmd>:q!<CR>')
vim.keymap.set('n','wq', '<cmd>:wq!<CR>')
vim.keymap.set('n','<leader>s', '<cmd>:NvimTreeToggle<CR>')
-- buffers
vim.keymap.set('n','<C-]>', ':bnext<CR>')
vim.keymap.set('n','<C-[>', ':bprevious<CR>')

-- Tabs
vim.keymap.set('n','<A-t>', ':tabnew<CR>')
vim.keymap.set('n','<A-e>', ':tabnext<CR>')
vim.keymap.set('n','<A-q>', ':tabclose<CR>')

-- terminal
vim.keymap.set('n','<A-d>', ':belowright new term://fish<CR>')
vim.keymap.set('t','<Esc>','<C-\\><C-n>')

-- visual mode indenting
vim.keymap.set('v','<','<gv')
vim.keymap.set('v','>','>gv')

-- escape
vim.keymap.set('i','jj','<ESC>')


-- Move Selected line  / block of text in Visual mode

vim.keymap.set("x","K",":move '<-2<CR>gv-gv")
vim.keymap.set("x","J",":move '>+1<CR>gv-gv")

