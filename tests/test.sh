#!/bin/sh
EMACS=emacs
OPTIONS="-L . -L $HOME/emacs/lisp -L .. -L ./init -l ./init/init.el -l ./test-*.el"
$EMACS -q --no-site-file --batch $OPTIONS -f ert-run-tests-batch-and-exit "$@"
ret=$?
exit $ret
