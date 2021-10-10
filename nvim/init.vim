syntax enable 


set clipboard+=unnamedplus
filetype plugin indent on
set guifont=Source\Code\Pro\Bold
set encoding=utf-8
set nohlsearch
set termguicolors
set noshowmode
set autoindent
set laststatus=2
set expandtab
set tabstop=4
set shiftwidth=4
set incsearch
set shortmess+=c
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
set shell=/bin/bash 



call plug#begin('~/.vim/plugged')
Plug 'neovim/nvim-lspconfig'
Plug 'jackguo380/vim-lsp-cxx-highlight'
Plug 'itchyny/lightline.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'vim-syntastic/syntastic'
Plug 'morhetz/gruvbox'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf'
Plug 'liuchengxu/vim-which-key'
Plug 'joshdick/onedark.vim' 
Plug 'preservim/nerdtree'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tomasr/molokai'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'hrsh7th/nvim-compe'
Plug 'ryanoasis/vim-devicons'
Plug 'mcchrish/nnn.vim'
Plug 'wolfgangmehner/lua-support'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'nvim-lua/telescope.nvim'
Plug 'glepnir/dashboard-nvim'
call plug#end()


luafile /home/amr/.config/nvim/lua/plug-colorizer.lua

""""""""" Theme
colorscheme molokai 
let mapleader = ";"


let g:dashboard_default_executive ='telescope'

let g:dashboard_custom_shortcut={
\ 'last_session'       : 'SPC s l',
\ 'find_history'       : 'SPC f h',
\ 'find_file'          : 'SPC f f',
\ 'new_file'           : 'SPC c n',
\ 'change_colorscheme' : 'SPC t c',
\ 'find_word'          : 'SPC f a',
\ 'book_marks'         : 'SPC f b',
\ }



let g:dashboard_custom_shortcut['last_session'] = ' '
let g:dashboard_custom_shortcut['find_history'] = 'ﭯ '
let g:dashboard_custom_shortcut['find_file'] = ' '
let g:dashboard_custom_shortcut['new_file'] = ' '
let g:dashboard_custom_shortcut['change_colorscheme'] = ' '
let g:dashboard_custom_shortcut['find_word'] = ' '
let g:dashboard_custom_shortcut['book_marks'] = ' '


"" Lightline
let g:lightline = {
      \ 'colorscheme': 'powerline',
      \ }


" template for some vim commands
"nnoremap silent> <keys> <cmd>command<CR>

"nnoremap <silent>y<cmd>:!xclip -f -i -selection clipboard<CR>
" My Key bindings
nnoremap <C-f> :FZF <CR>
nnoremap <leader>s <cmd>NERDTree <CR>
nnoremap <A-t> :tabnew <CR>
nnoremap <A-q> :tabprev <CR>
nnoremap <A-e> :tabnext <CR>
nnoremap <A-c> :tabclose <CR>
nnoremap <C-s>:source %<CR>
nnoremap <silent>dq <cmd>:q!<CR>
nnoremap <silent>wq <cmd>:wq<CR>
nnoremap <silent>sw <cmd>:source ~/.config/nvim/init.vim<CR>
nnoremap <leader>t :vertical terminal<CR>
nnoremap <leader>f <cmd>NnnPicker<cr>

"""""""""""""" NnnPicker 
let g:nnn#command = 'nnn -e'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" c++ syntax highlighting
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
   if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
        else
    set signcolumn=yes
       endif


