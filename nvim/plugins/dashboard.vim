

set dashboard_session_directory= "~/.cache/vim/session"

let g:dashboard_default_executive ='telescope'
eg : "SPC mean the leaderkey
let g:dashboard_custom_shortcut={
\ 'last_session'       : 'SPC s l',
\ 'find_history'       : 'SPC f h',
\ 'find_file'          : 'SPC f f',
\ 'new_file'           : 'SPC c n',
\ 'change_colorscheme' : 'SPC t c',
\ 'find_word'          : 'SPC f a',
\ 'book_marks'         : 'SPC f b',
\ }


nmap <Leader>ss :<C-u>SessionSave<CR>
nmap <Leader>sl :<C-u>SessionLoad<CR>
nnoremap <silent> <Leader>fh :DashboardFindHistory<CR>
nnoremap <silent> <Leader>ff :DashboardFindFile<CR>
nnoremap <silent> <Leader>tc :DashboardChangeColorscheme<CR>
nnoremap <silent> <Leader>fa :DashboardFindWord<CR>
nnoremap <silent> <Leader>fb :DashboardJumpMark<CR>
nnoremap <silent> <Leader>cn :DashboardNewFile<CR>


let g:dashboard_custom_shortcut_icon['last_session'] = ' '
let g:dashboard_custom_shortcut_icon['find_history'] = 'ﭯ '
let g:dashboard_custom_shortcut_icon['find_file'] = ' '
let g:dashboard_custom_shortcut_icon['new_file'] = ' '
let g:dashboard_custom_shortcut_icon['change_colorscheme'] = ' '
let g:dashboard_custom_shortcut_icon['find_word'] = ' '
let g:dashboard_custom_shortcut_icon['book_marks'] = ' '


let g:dashboard_custom_section={
  \ 'buffer_list': {
      \ 'description': [' Recently lase session                 SPC b b'],
      \ 'command': 'Some Command' or function('your funciton name') }
  \ }
