XMONAD CONFIG
-------------
Xmonad is basically a library for writing your own custom window manager. Here
is my personal window manager writen in Haskel using XMonad and XMonad-contrib

REQUIREMENTS
------------
This should run with the darcs verisons of both XMonad and XMonad-contrib. I
also eliminated dependencies on my own code in XMonad-contrib so that this
should run with older verisons of XMonad and XMonad-contrib. This has yet to be
tested though. 

* dzen2
* stalonetray
* nm-applet (NetworkMonitor applet)
* battery status app - (currently custom gtk2 based ruby script that isn't available online, haskell version is in the
  works and should be available soon)
* libXft - Fonts will silently render weird if you don't have libXft installed. 
* fonts - You also need the fonts referenced in this config installed on you
  system. You should be able to find these fonts in my dot-files repo. 

FEATURES
--------
* dzen top bar 
* The doInYoFace manageHook for urgent windows (also ability to use regular
  manageHooks for urgen windows)
* spawnNamedPipe (this has been accepted to XMonad-contrib tip)
* startup apps are properly killed and cleaned up on shutdown/restart
