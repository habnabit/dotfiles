#!/bin/sh
set -ev
git subtree pull --prefix oh-my-zsh --squash https://github.com/habnabit/oh-my-zsh.git master
git subtree pull --prefix emacs.d/nxhtml --squash https://github.com/habnabit/nxhtml.git master
git subtree pull --prefix emacs.d/magit --squash https://github.com/magit/magit.git master
git subtree pull --prefix emacs.d/notmuch --squash git://notmuchmail.org/git/notmuch master
git subtree pull --prefix emacs.d/popwin-el --squash https://github.com/m2ym/popwin-el.git master
git subtree pull --prefix emacs.d/git-gutter --squash https://github.com/syohex/emacs-git-gutter.git master
git subtree pull --prefix emacs.d/rainbow-delimiters --squash https://github.com/jlr/rainbow-delimiters.git master
git subtree pull --prefix emacs.d/markdown-mode --squash git://jblevins.org/git/markdown-mode.git master
git subtree pull --prefix emacs.d/web-mode --squash https://github.com/fxbois/web-mode.git master
git subtree pull --prefix emacs.d/pymacs --squash https://github.com/pinard/Pymacs.git master
git subtree pull --prefix emacs.d/autocomplete --squash https://github.com/auto-complete/auto-complete.git master
git subtree pull --prefix emacs.d/dash --squash https://github.com/magnars/dash.el.git master
git subtree pull --prefix emacs.d/color-identifiers-mode --squash https://github.com/ankurdave/color-identifiers-mode.git master
git subtree pull --prefix emacs.d/flycheck --squash https://github.com/flycheck/flycheck.git master
git subtree pull --prefix emacs.d/solarized-emacs --squash https://github.com/bbatsov/solarized-emacs master
git subtree pull --prefix emacs.d/git-modes --squash https://github.com/magit/git-modes master
git subtree pull --prefix emacs.d/rust-mode --squash https://github.com/rust-lang/rust-mode master
git subtree pull --prefix emacs.d/flycheck-rust --squash https://github.com/flycheck/flycheck-rust master
