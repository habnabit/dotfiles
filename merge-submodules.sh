#!/bin/sh
set -ev
git subtree pull --prefix oh-my-zsh --squash git://github.com/habnabit/oh-my-zsh.git master
git subtree pull --prefix emacs.d/nxhtml --squash git://github.com/habnabit/nxhtml.git master
git subtree pull --prefix emacs.d/twittering-mode --squash git://github.com/hayamiz/twittering-mode.git master
git subtree pull --prefix emacs.d/magit --squash git://github.com/magit/magit.git master
git subtree pull --prefix emacs.d/flymake --squash git://github.com/illusori/emacs-flymake.git master
git subtree pull --prefix emacs.d/circe --squash git://github.com/retroj/circe.git master
