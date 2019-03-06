# Bash completion
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# opam
test -r /Users/rmn/.opam/opam-init/init.sh && . /Users/rmn/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# ATS2
export PATSHOME=/Users/rmn/ats2
export PATH=${PATSHOME}/bin:${PATH}

# TCL/TK
export PATH="/usr/local/opt/tcl-tk/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/tcl-tk/lib"
export CPPFLAGS="-I/usr/local/opt/tcl-tk/include"

# cabal
CABALHOME=/Users/rmn/.cabal
export PATH=${CABALHOME}/bin:${PATH}
alias config='/usr/bin/git --git-dir=/Users/rmn/dots/ --work-tree=/Users/rmn'
