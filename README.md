# bun.el
[![Actions Status](https://github.com/shaneikennedy/bun.el/workflows/check/badge.svg)](https://github.com/shaneikennedy/bun.el/actions)


This is a [bun.sh](https://bun.sh/) client for emacs.

This package makes use of [transient.el](https://github.com/magit/transient) to offer an interactive experience with bun that I find to be advantageous for exploration and in general when you aren't totally familiar with bun or your current javascript/typescript project.

## Install
This package is not yet available on MELPA

### Using `use-package`

`git clone git@github.com:shaneikennedy/bun.el.git /path/to/bun.el`

``` emacs-lisp
(use-package bun
    :ensure t
    :load-path "/path/to/bun.el")
```

## Usage
`M-x bun`


As of right now this package only supports the `bun run, test, install, and update` commands but I hope to add more functionality soon. If you have a command you would really like to see added here please open an issue!
