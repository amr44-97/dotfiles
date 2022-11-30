

export PATH="/opt/zig-linux-x86_64-dev/:$PATH"
export PATH="/opt/OdinLang-amd64-dev:$PATH"
export PATH="/opt/lua-language-server/bin:/opt/zls-x86_64:$PATH"
export PATH="/opt/Lua-Language-Server/bin:$PATH"
export PATH="/opt/zls/zig-out/bin:$PATH"
export PATH="/opt/go/bin:$PATH"
#export PATH="$PATH:$(go env GOPATH)/bin"


export BUN_INSTALL="/home/amr/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="/home/amr/.cargo/bin:$PATH"
export EDITOR='emacsclient -tty -a emacs'

[ -n "$SSH_CONNECTION" ] && unset SSH_ASKPASS
export GIT_ASKPASS=





# aliases 
alias vi        'nvim'
alias ed        'emacsclient -tty -a emacs -nw'

alias vim      'nvim'
alias v        'nvim'
alias nv       'neovide'
alias cls      'clear'
alias clr      'clear'

alias nivm      'nvim' 
alias ls        'lsd'
alias cat       'bat'
alias ll        'lsd -al'
alias mpc       'mpv --no-video *'   
alias grep      'grep --colour=auto'   
alias egrep     'grep -E --colour=auto'   
alias fgrep     'grep -F --colour=auto'   



function fish_greeting
end

