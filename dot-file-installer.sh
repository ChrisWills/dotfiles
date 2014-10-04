#!/bin/bash

for a in .xinitrc .Xdefaults .fonts ; do 
  if ([[ -L ~/${a} ]] && [[ `readlink ~/${a}` = ~/.xmonad/${a} ]]) || ([[ -L ~/${a} ]] && [[ -d ~/${a} ]] && [[ `readlink ~/${a}` = ~/.xmonad/${a}/ ]]);then
    echo "${a} [OK]" 
  elif ! [[ -L ~/${a} ]] ; then 
    echo "${a} is not a link! [WARNING]"
  else
    echo "${a} doesn't exist! You should install it [CRITICAL]"
  fi
done
