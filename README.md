# Superior Haskell Interaction Mode for Emacs

## Introduction

Shime handles GHCi processes and Cabal processes associated with a
*session*.

## Setup

    (add-to-list 'load-path "~/Emacs/me/shime")
    (require 'shime)

Use `M-x customize-group RET shime RET` to configure Shime. You are
likely to want to configure this variable: `shime-default-ghci-path`

Example recommended key bindings:

    (define-key haskell-mode-map [f5] 'shime-load-file)
    (define-key haskell-mode-map (kbd "C-c C-c") 'shime-cabal-build)
    (define-key haskell-mode-map (kbd "C-c c") 'shime-cabal-ido)

## Sessions

Shime's main strength is the concept of multiple project *sessions*.

To start a Shime session, run

    M-x shime-start-named-session

which will prompt for the name of the session and automatically start
a GHCi and Cabal process. Run this command again to start another.

To start a default session named `shime-default-session-name`, run

    M-x shime

If a process is already started, you will be prompted:

> Shime session(s) already started. Start a new session? (y or n)

This makes `M-x shime` an alternative to achieving the same goal as
`M-x shime-start-named-session`.

To kill particular sessions, run `M-x shime-kill-session` and you can
choose which to kill. A session can have many buffers and many
processes, so just killing a buffer doesn't make much sense unless I
add a "master buffer" concept in future versions.

## Loading files

Every buffer has an associated session and an associated GHCi and
Cabal process. When you first run

    M-x shime-load-file

You will be prompted for the particular session and GHCi process to
use. (If there is only one choice Shime defaults to the first one.)
Each process (GHCi and Cabal) have a root directory, by default it
picks the current directory for loading.

If you load a file above the project root, it will prompt you,

    Do you want to change the root directory? (y or n) 

Choosing `yes` will allow you to enter a new path, `no` will load
the file with an absolute path.

When you first run `shime-cabal-build` or any other `shime-cabal-`
command, you will be prompted for the session and Cabal process
(again, it will default to the only Cabal process if there is only
one, which is default). Shime will ask you what directory Cabal
commands should be ran from, defaulting to the same as the load root.

If you need to change the session or process associated with a buffer,
run

    M-x shime-choose-buffer-cabal-process

or

    M-x shime-choose-buffer-ghci-process

If you need to change the load root or Cabal root of a session, run:

    M-x shime-choose-load-root

or

    M-x shime-choose-cabal-root

## All commands

* `shime-start-named-session` -- Start a new session and prompts for name.

* `shime` -- Start a defaultly-named session or prompt for a new name.

These do what you expect:

* `shime-cabal-build`
* `shime-cabal-clean`
* `shime-cabal-command`
* `shime-cabal-configure`
* `shime-cabal-install`

* `shime-cabal-ido` -- Prompts to choose a Cabal command to run.

* `shime-choose-buffer-cabal-process` -- Allows you to choose the
  Cabal process associated with this buffer.

* `shime-choose-buffer-ghci-process` -- Allows you to choose the GHCi
  process associated with this buffer.

* `shime-choose-buffer-session` -- Choose the session associated with
  this buffer.

* `shime-choose-load-root` -- Choose the GHCi load root for this
  session.

* `shime-choose-cabal-root` -- Choose the Cabal project root for this
  session.

* `shime-kill-buffer` -- Kill a Shime buffer.

* `shime-kill-process` -- Kill a Shime process.

* `shime-kill-session` -- Kill a Shime session and all associated
  buffers and processes.

* `shime-load-file` -- Load the current buffer with GHCi.

* `shime-reset-everything-because-it-broke` -- Just reset everything
  in Shime ready for you to run `M-x shime` or `M-x
  shime-start-named-session` again.

## Restarting GHCi

If you need to restart GHCi, just run :q in the prompt and you will be
asked

    The GHCi process "shime-ghci" ended. Restart it? (y or n)

Which will restart it in place.
