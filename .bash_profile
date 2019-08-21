# Bash completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# bin dir
export PATH="/Users/rnagasam/bin:$PATH"

# Opam
test -r /Users/rnagasam/.opam/opam-init/init.sh && . /Users/rnagasam/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# SML
export SMACKAGE_HOME="/Users/rnagasam/.smackage"
export PATH="/usr/local/smlnj/bin:$SMACKAGE_HOME/bin:$PATH"

# Coq -- not through OPAM
# COQ_BIN="/Applications/CoqIDE_8.10+beta2.app/Contents/Resources/bin"
# export PATH="$COQ_BIN:$PATH"

export PS1="\w> "
export HISTIGNORE="ls:cd:clear"
export HISTTIMEFORMAT="%c "
export HISTCONTROL="ignoredups"

alias dots="git --work-tree=$HOME --git-dir=$HOME/dots.git/"
