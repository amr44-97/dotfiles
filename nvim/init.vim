syntax enable 

set guifont=DroidSansMono\ Font\ 11
set encoding=utf-8
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set incsearch
set complete-=i
set nowrap
set laststatus=2
set wildmenu
set scrolloff=15
set number
set noswapfile
set noerrorbells
set mouse=a
set background=dark
set title
set shell=/bin/zsh 



call plug#begin('~/.vim/plugged')
Plug 'neovim/nvim-lspconfig'
Plug 'junegunn/fzf'
Plug 'preservim/nerdtree'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'hrsh7th/nvim-compe'
Plug 'jorengarenar/fauxclip'
Plug 'wolfgangmehner/lua-support'
call plug#end()


colorscheme dracula

luafile ~/.config/nvim/lua/plugins/compe-config.lua
luafile ~/.config/nvim/lua/lsp/lua-ls.lua

noremap  <C-c> :'<,'>:!xclip -f -i -selection clipboard <CR>

" LSP config (the mappings used in the default file don't quite work right)
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>


" My Key bindings
nnoremap <C-w> :FZF <CR>
nnoremap <C-n> :NERDTree <CR>
nnoremap <C-t> :tabnew <CR>
nnoremap <C-e> :tabnext <CR>
nnoremap <A-c> :tabclose <CR>



" auto-format
autocmd BufWritePre *.js lua vim.lsp.buf.formatting_sync(nil, 100)
autocmd BufWritePre *.jsx lua vim.lsp.buf.formatting_sync(nil, 100)
autocmd BufWritePre *.py lua vim.lsp.buf.formatting_sync(nil, 100)


"" fauxclip
let g:fauxClip_copy_cmd         = 'xclip -f -i -selection clipboard'
let g:fauxClip_copy_primary_cmd = 'xclip -f -i'

let g:fauxClip_paste_cmd         = 'xclip -o -selection clipboard'
let g:fauxClip_paste_primary_cmd = 'xclip -o'
