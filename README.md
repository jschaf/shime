# Superior Haskell Interaction Mode for Emacs

## Example setup

Here is my setup:

    (add-to-list 'load-path "~/Emacs/me/shime")
    (require 'shime)
    (setq shime-program "/home/chris/Programs/bin/ghci")

To start Shime run `M-x shime`. Right now it's preferrable to
start Shime before loading any files.

## Loading files

    (define-key haskell-mode-map [f5] 'shime-load-file)

Shime has one project root. By default it uses the directory of
the first file loaded. All files loaded under that directory are
loaded relatively.

If you load a file above the project root, it will prompt you,

    Do you want to change the root directory? (y or n) 

Choosing `yes` will allow you to enter a new path, `no` will load
the file with an absolute path.

If, for some reason, you have a project within a project and thus
you need to set a different directory, use `M-x
shime-choose-root`. It will prompt with the current root and you
can change it to whatever you like.

This should fit most workflows.
