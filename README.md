# Superior Haskell Interaction Mode for Emacs

## Example setup

Here is my setup:

    (add-to-list 'load-path "~/Emacs/me/shime")
    (require 'shime)

Something like:

    (custom-set-variables
     '(shime-default-ghci-path "/home/chris/Programs/bin/ghci")
     '(shime-default-session-name "shime"))

Or just `M-x customize-group RET shime RET` and do it from there.

To start Shime run `M-x shime`. You can do this multiple times if
you want more GHCi sessions and it will prompt for different
session names. To kill particular sessions, right now, you can't
just kill the buffer, run `M-x shime-kill-session` and you can
choose which to kill. A session can have many buffers and many
processes, so just killing a buffer doesn't make much sense
unless I add a "master buffer" concept in future versions.

## Loading files

    (define-key haskell-mode-map [f5] 'shime-load-file)

Shime has one project root (right now). By default it uses the
directory of the first file loaded. All files loaded under that
directory are loaded relatively.

If you load a file above the project root, it will prompt you,

    Do you want to change the root directory? (y or n) 

Choosing `yes` will allow you to enter a new path, `no` will load
the file with an absolute path.

If, for some reason, you have a project within a project and thus
you need to set a different directory, use `M-x
shime-choose-root`. It will prompt with the current root and you
can change it to whatever you like.

This should fit most workflows. My next patch will associate
project roots with individual sessions, thus you can load many
projects at once with their own GHCi instances and project roots.
