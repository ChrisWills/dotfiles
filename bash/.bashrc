# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Maximum number of history lines in memory
export HISTSIZE=50000
# Maximum number of history lines on disk
export HISTFILESIZE=50000
# Ignore duplicate lines
export HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file
#  instead of overwriting it
shopt -s histappend

# After each command, append to the history file
#  and reread it
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"


# fix color-233 to be #161616
#(trick emacs into using my exact background color)
if [ -z ${TMUX+x} ]; then
	#TMUX not in use pass directly to xterm
	printf '\033]4;233;rgb:16/16/16\007'
else
	#TMUX in use, wrap in special pass-thru sequence
	printf '\ePtmux;\e\033]4;233;rgb:16/16/16\007\e\\'
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias emacs='emacs -nw'
# Install Ruby Gems to ~/gems
export GEM_HOME=$HOME/gems
export CABAL_HOME=$HOME/.cabal-sandbox/bin/
export PATH=$HOME/gems/bin:$CABAL_HOME:$PATH
