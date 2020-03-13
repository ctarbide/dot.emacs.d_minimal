# -*- mode:org -*-

#+TITLE: Emacs Minimal Configuration
#+STARTUP: indent

This document assumes a basic understanding of GNU Emacs and [[info:org#Summary][Org Mode - Summary]]
and uses literate programming for generation of configuration files,
for more information see [[info:org#Extracting source code][Org Mode - Extracting Source Files]].

This is targeted to minimal installations, it assumes a recent emacs
(tested on v26.3) and it require no package installation nor external
dependencies.

Use =M-x org-babel-tangle= to generate =init.el= and =eshell/alias=.

* Preamble

#+BEGIN_SRC emacs-lisp :tangle init.el
  ;; -*- mode:emacs-lisp; coding:utf-8; lexical-binding:t -*-

  ;; WARNING: This is generated automatically from README.txt using
  ;; Org-Mode. All changes here will be lost, eventually.

  (let ((custom-settings (expand-file-name "~/.emacs.d/custom-settings.el")))
    (when (file-exists-p custom-settings)
      (load-file custom-settings)))
#+END_SRC

* Remove distractions

Taken from [[http://whattheemacsd.com/][whattheemacsd.com]].

#+BEGIN_SRC emacs-lisp :tangle init.el
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
#+END_SRC

* Prefer a sane encoding on all platforms
#+BEGIN_SRC emacs-lisp :tangle init.el
(prefer-coding-system 'utf-8-unix)
#+END_SRC
* Tabs or spaces? Spaces!

#+BEGIN_SRC emacs-lisp :tangle init.el
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
#+END_SRC

* Set some important environment variables

#+BEGIN_SRC emacs-lisp :tangle init.el
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")
#+END_SRC

* Customization

#+BEGIN_SRC emacs-lisp :tangle init.el
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(confirm-kill-emacs (quote yes-or-no-p))
   '(inhibit-startup-screen t)
   '(show-paren-delay 0.0)
   '(show-paren-mode t)
   '(show-paren-when-point-inside-paren t))
#+END_SRC

* Load 'wombat theme

It is such a nice theme.

#+BEGIN_SRC emacs-lisp :tangle init.el
(add-hook 'after-init-hook '(lambda () (load-theme 'wombat t)))
(when (and (memq system-type '(ms-dos windows-nt)) (> emacs-major-version 24))
  (add-hook 'window-setup-hook '(lambda () (load-theme 'wombat t))))
#+END_SRC

* Eshell setup

Forget about silly shells, use an elisp enabled ultra powerful shell.

#+BEGIN_SRC emacs-lisp :tangle init.el
(require 'eshell)

(setq eshell-buffer-maximum-lines 10000)
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(setq eshell-history-size 1000) ;; default is 128

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq
    eshell-prefer-lisp-functions t
    pcomplete-cycle-completions nil
    pcomplete-cycle-cutoff-length 0
    pcomplete-dir-ignore nil
    pcomplete-file-ignore nil
    pcomplete-use-paring nil)))
#+END_SRC

* IDO

IDO will save you a lot of time in finding files and buffers, use =C-x
C-f= and =C-x C-b= to fall back to standard minibuffer. More
information in [[https://www.masteringemacs.org/article/introduction-to-ido-mode][Mastering Emacs Book]].

#+BEGIN_SRC emacs-lisp :tangle init.el
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
#+END_SRC

* Show Trailing Whitespaces

Found in [[https://github.com/mbriggs/.emacs.d-v3][M. Briggs dot files]].

#+BEGIN_SRC emacs-lisp :tangle init.el
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (and
                   (not (eq major-mode 'Custom-mode))
                   (not (eq major-mode 'shell-mode))
                   (not (eq major-mode 'emacs-pager-mode))
                   (not (eq major-mode 'term-mode))
                   (not (eq major-mode 'eshell-mode))
                   (not (eq major-mode 'ibuffer-mode))
                   (not (eq major-mode 'rspec-compilation-mode))
                   (not (eq major-mode 'Buffer-menu-mode))
                   (not (eq major-mode 'prodigy-mode)))
              (setq show-trailing-whitespace t))))
#+END_SRC

* Org-Mode Customizations

#+BEGIN_SRC emacs-lisp :tangle init.el
;;(setq org-ellipsis " ● ● ●")
;;(setq org-ellipsis " ○ ○ ○")
(setq org-ellipsis " ◦◦◦")

;; (info-other-window "(org) Clean view")
(setq org-hide-leading-stars t)
(setq org-indent-mode t)
#+END_SRC

* Nice Utilities and Key Bindings

Special thanks to [[http://www.howardism.org/Technical/Emacs/eshell-fun.html][Howard Abrams]] for =eshell-here=. Here is a slightly
"improved" version.

#+BEGIN_SRC emacs-lisp :tangle init.el
  (defun dos2unix ()
    "Say that hard goodby to CRLF"
    (interactive)
    (set-buffer-file-coding-system 'utf-8-unix 't))

  (defun eshell/lspath-exec-path ()
    "list path from exec-path"
    (mapconcat 'identity exec-path "\n"))

  (defun eshell/lspath-path ()
    "list path from PATH environment variable"
    (mapconcat 'identity (split-string (getenv "PATH") path-separator) "\n"))

  (defun eshell/lspath-eshell-path-env ()
    "list path from eshell-path-env variable"
    (mapconcat 'identity (split-string eshell-path-env path-separator) "\n"))

  (defun associated-buffer-directory-name ()
    "get a file name associated with the buffer"
    (let ((bfn (buffer-file-name))
          (lbd (bound-and-true-p list-buffers-directory)))
      (or (and bfn (abbreviate-file-name (file-name-directory bfn)))
          (and lbd (abbreviate-file-name lbd)))))

  (defun eshell-here-maybe-reuse-existing ()
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (let* ((parent (or
                    (associated-buffer-directory-name)
                    default-directory
                    (error "could not determine parent directory")))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t))))
           (eshell-buffer-name (concat "*eshell: " name "*"))
           (buffer-already-exist (get-buffer eshell-buffer-name))
           (buffer (or buffer-already-exist (get-buffer-create eshell-buffer-name)))
           (window (or (get-buffer-window buffer 'visible)
                       (split-window-vertically (- height)))))
      (select-window window)
      (switch-to-buffer buffer nil t)
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode))
      (when (not buffer-already-exist)
        (insert "ls")
        (eshell-send-input))))

  (defun eshell-here-force-new ()
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (let* ((parent (or
                    (associated-buffer-directory-name)
                    default-directory
                    (error "could not determine parent directory")))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t))))
           (eshell-buffer-name (concat "*eshell: " name "*"))
           (buffer (generate-new-buffer eshell-buffer-name)))
      (select-window (split-window-vertically (- height)))
      (switch-to-buffer buffer nil t)
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode))
      (insert "ls")
      (eshell-send-input)))

  (defun eshell-here (&optional arg)
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (interactive "P")
    (if arg (eshell-here-force-new) (eshell-here-maybe-reuse-existing)))

  (defun kill-buffer-dont-ask ()
    "as it name implies"
    (interactive)
    (kill-buffer))

  (defun eshell/new-eshell-at (arg)
    "create a new eshell with the directory name in the buffer name"
    (if (not (file-directory-p arg)) (error "\"%s\" is not a directory" arg))
    (let* ((dir (abbreviate-file-name (expand-file-name arg)))
           (default-directory dir)
           (name (car (last (split-string dir "/" t))))
           (buf (generate-new-buffer (format "*eshell: %s*" name))))
      (cl-assert (and buf (buffer-live-p buf)))
      (pop-to-buffer-same-window buf)
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode))
      (insert "ls")
      (eshell-send-input)
      buf))
#+END_SRC

And some key bindings:

#+BEGIN_SRC emacs-lisp :tangle init.el
(global-set-key (kbd "C-x x") 'eshell-here)
(global-set-key (kbd "C-<f4>") 'kill-buffer-dont-ask)
#+END_SRC

* All Done!
Just a simple debuging message.
#+BEGIN_SRC emacs-lisp :tangle init.el
(message "All done with %s!" "init.el")
#+END_SRC
* Some aliases

#+BEGIN_SRC text :tangle eshell/alias
alias bl (pop-to-buffer-same-window (list-buffers-noselect))
alias echo1-cmd echo $1
alias echo1-elisp (princ (car eshell-command-arguments))
alias el (pop-to-buffer-same-window (list-buffers-noselect nil (seq-filter (lambda (e) (string-prefix-p "*eshell" (buffer-name e) t)) (buffer-list))))
alias fl (pop-to-buffer-same-window (list-buffers-noselect t))
alias gi git status; git branch -a; git remote -v
alias git-diff-nocr git diff $* | perl -lpe's,\r,,'
alias git-repack-and-prune git repack -d && git prune
alias ll ls -alh $*
alias localstamp (format-time-string "%Y-%m-%d_%Hh%Mm%S")
alias lspath-perl-colon echo $PATH | perl -l -072 -pe1
alias lspath-perl-semicolon echo $PATH | perl -l -073 -pe1
alias rm-tilde rm -fv *~ .??*~
alias runemacs-exe "$emacs_dir/bin/runemacs.exe" $*
#+END_SRC
* Automated Extraction
See [[info:org#Batch%20execution][Batch Execution]].
