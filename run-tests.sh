#!/bin/bash

set -x
set -e

EMACS="${EMACS:=emacs}"


${EMACS} -Q -batch \
         -L . \
         --eval "(setq byte-compile-error-on-warn t)" \
         -f batch-byte-compile basic-stats.el basic-stats.t.el

${EMACS} -Q -batch \
         -L . \
         -l basic-stats.elc -l basic-stats.t.elc \
         -f ert-run-tests-batch-and-exit
