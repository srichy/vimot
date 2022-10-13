# Really?  Why?

Sometimes I like vi-style motion.  At the same time, I really just
want Emacs to be Emacs.  I can't deal with viper or evil or whatever.
I just wanted the ability to quickly turn on vi-style motion, move
around, and then seamlessly hit Emacs-native keys and have them work
w/o any drama.

set-transient-map allows this exact functionality.  I'm sure that
there are 100 other implementations just like this, but hadn't
stumbled across them (didn't really look that hard), and I had 30
minutes to kill.

# Quick Start

1. Put vimot.el in a suitable for Emacs (e.g., ~/.emacs.d)
2. Add to your ~/.emacs.d/init.el (or ~/.emacs, etc.):
```cl
;; vimot
(load-file "~/.emacs.d/vimot.el")
(vimotion-enable t)
(global-set-key (kbd "C-^") 'vimotion-activate)
```

I should probably use a more sophisticated loading technique, but it's
a low priority for me.
