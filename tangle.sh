#!/bin/sh
# tangle files with org-mode
#
# (info-other-window "(org) Batch execution")
#
exec emacs -Q --batch --eval "
(progn
  (require 'ob-tangle)
  (dolist (file command-line-args-left)
    (with-current-buffer (find-file-noselect file)
      (org-babel-tangle))))" "$@"
