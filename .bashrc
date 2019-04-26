#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias dots='git --work-tree=$HOME --git-dir=$HOME/dots/'

chere_libs="-lm"
chere_flags="-g -Wall -I/home/rmn/src/c -include allheads.h -O3"
alias chere="c99 -xc '-' $chere_libs $chere_flags"

export PATH="/home/rmn/bin:$PATH"

alias ]=xdg-open
alias sys='sudo systemctl'
alias usys='systemctl --user'

if [ $INSIDE_EMACS ]; then
        export PAGER="emacs-pager"
else
        export PAGER=less
fi
