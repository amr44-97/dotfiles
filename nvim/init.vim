set clipboard=unnamedplus
filetype plugin indent on
set guifont=Source\Code\Pro\Bold\:h17
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
"Plug 'itchyny/lightline.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'fatih/vim-go'
Plug 'vim-syntastic/syntastic'
Plug 'morhetz/gruvbox'
Plug 'prabirshrestha/vim-lsp'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf'
Plug 'liuchengxu/vim-which-key'
Plug 'joshdick/onedark.vim'
Plug 'preservim/nerdtree'
Plug 'tomasr/molokai'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'hrsh7th/nvim-compe'
Plug 'ryanoasis/vim-devicons'
Plug 'mcchrish/nnn.vim'
Plug 'wolfgangmehner/lua-support'
Plug 'nvim-lua/plenary.nvim'
Plug 'vimwiki/vimwiki'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'nvim-lua/telescope.nvim'
Plug 'glepnir/dashboard-nvim'
Plug 'romgrk/doom-one.vim'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'ajh17/VimCompletesMe'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" Rust 


Plug 'simrat39/rust-tools.nvim'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'

" Completion framework
Plug 'hrsh7th/nvim-cmp'

" LSP completion source for nvim-cmp
Plug 'hrsh7th/cmp-nvim-lsp'

" Snippet completion source for nvim-cmp
Plug 'hrsh7th/cmp-vsnip'

" Color scheme used in the GIFs!
Plug 'arcticicestudio/nord-vim'

""""""""""
set shortmess+=c


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""" Nim
Plug 'alaviss/nim.nvim'
Plug 'prabirshrestha/asyncomplete.vim'

""" Zig
Plug 'ziglang/zig.vim'
Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}
let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-css', 'coc-html', 'coc-json', 'coc-prettier']  " list of CoC extensions needed

Plug 'jiangmiao/auto-pairs' "this will auto close ( [ {

" these two plugins will add highlighting and indenting to JSX and TSX files.
Plug 'yuezk/vim-js'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'maxmellon/vim-jsx-pretty'



call plug#end()

tnoremap <Esc> <C-\><C-n>

luafile /home/amr/.config/nvim/lua/plug-colorizer.lua


""""""""" Theme
colorscheme doom-one
let g:doom_one_terminal_colors = v:true
let mapleader = ";"
"""""""""""""""""'' Terminal """""""""""""

tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l


let g:dashboard_default_executive ='telescope'

let g:neovide_transparency=0.95


let g:dashboard_custom_shortcut={
\ 'last_session'       : 'SPC s l',
\ 'find_history'       : 'SPC f h',
\ 'find_file'          : 'SPC f f',
\ 'new_file'           : 'SPC c n',
\ 'change_colorscheme' : 'SPC t c',
\ 'find_word'          : 'SPC f a',
\ 'book_marks'         : 'SPC f b',
\ }


let g:LanguageClient_serverCommands = {
\   'nim': ['~/.nimble/bin/nimlsp'],
\   'rust': ['~/.cargo/bin/rust-analyzer'],
\ }

let g:dashboard_custom_shortcut['last_session'] = ' '
let g:dashboard_custom_shortcut['find_history'] = 'ﭯ '
let g:dashboard_custom_shortcut['find_file'] = ' '
let g:dashboard_custom_shortcut['new_file'] = ' '
let g:dashboard_custom_shortcut['change_colorscheme'] = ' '
let g:dashboard_custom_shortcut['find_word'] = ' '
let g:dashboard_custom_shortcut['book_marks'] = ' '


"" Lightline
"let g:lightline = {
"      \ 'colorscheme': 'powerline',
"      \ }

"
"////////////////////// Airline neovim \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:airline#extensions#tabline#enabled = 1 " Enable the list of buffers

let g:airline#extensions#tabline#formatter = 'default'  " f/p/file-name.js
let g:airline#extensions#tabline#formatter = 'jsformatter' " path-to/f
let g:airline#extensions#tabline#formatter = 'unique_tail' " file-name.js
let g:airline#extensions#tabline#formatter = 'unique_tail_improved' " f/p/file-name.js

let g:airline_theme='tomorrow'

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '☰ '
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.dirty='⚡'





" template for some vim commands
"nnoremap silent> <keys> <cmd>command<CR>

"nnoremap <silent>y<cmd>:!xclip -f -i -selection clipboard<CR>
" My Key bindings
nnoremap <C-f> :FZF <CR>
nnoremap <leader>s <cmd>NERDTree <CR>
nnoremap <A-t> :tabnew <CR>
nnoremap <A-q> :tabprev <CR>
nnoremap <A-e> :tabnext <CR>
nnoremap <A-d> :belowright new term://bash<CR>
nnoremap <A-c> :tabclose <CR>
nnoremap <C-s>:source %<CR>
nnoremap <silent>dq <cmd>:q!<CR>
nnoremap <A-w>:w! <CR>
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

"" Nim setup

"au User asyncomplete_setup call asyncomplete#register_source({
"    \ 'name': 'nim',
"    \ 'whitelist': ['nim'],
"    \ 'completor': {opt, ctx -> nim#suggest#sug#GetAllCandidates({start, candidates -> asyncomplete#complete(opt['name'], ctx, start, candidates)})}
"    \ })


let g:asyncomplete_auto_popup = 0

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ asyncomplete#force_refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" allow modifying the completeopt variable, or it will
" be overridden all the time
let g:asyncomplete_auto_completeopt = 0

set completeopt=menuone,noinsert,noselect,preview

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif


if executable('nimlsp')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'nimlsp',
        \ 'cmd': {server_info->['nimlsp']},
        \ 'allowlist': ['nim'],
        \ })
endif



""""""""""""""""""""""""' Rust 


lua <<EOF
local nvim_lsp = require'lspconfig'

local opts = {
    tools = { -- rust-tools options
        autoSetHints = true,
        hover_with_actions = true,
        inlay_hints = {
            show_parameter_hints = false,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
            }
        }
    },
}

EOF

"""Setup Completion
" See https://github.com/hrsh7th/nvim-cmp#basic-configuration







