# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

# fix color-233 to be #161616
#(trick emacs into using my exact background color)
if [ -z ${TMUX+x} ]; then
	#TMUX not in use pass directly to xterm	
	printf '\033]4;233;rgb:16/16/16\007'
else
	#TMUX in use, wrap in special pass-thru sequence
	printf '\ePtmux;\e\033]4;233;rgb:16/16/16\007\e\\'
fi

PATH=$HOME/.local/bin:$HOME/bin:$PATH

export PATH

# opam configuration
test -r /home/cwills/.opam/opam-init/init.sh && . /home/cwills/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

