# -*- mode:org; coding:utf-8-unix -*-

#+TITLE: Emacs Minimal Configuration
#+STARTUP: indent

This document assumes a basic understanding of GNU Emacs and [[info:org#Summary][Org Mode - Summary]]
and uses literate programming for generation of configuration files,
for more information see [[info:org#Extracting source code][Org Mode - Extracting Source Files]].

This is targeted to minimal installations, it assumes a recent emacs
(tested on v26.3) and it require no package installation nor external
dependencies.

Use =M-x org-babel-tangle= to generate =init.el=.

* Preamble

#+BEGIN_SRC emacs-lisp :tangle init.el
  ;; -*- mode:emacs-lisp; coding:utf-8-unix; lexical-binding:t -*-

  ;; WARNING: This is generated automatically from README.txt using
  ;; Org-Mode. All changes here will be lost, eventually.

  ;; Lastest revision at https://github.com/ctarbide/dot.emacs.d_minimal/blob/master/README.txt

  (let* ((thisdir (file-name-directory (or load-file-name buffer-file-name)))
         (custom-settings (expand-file-name "custom-settings.el" thisdir)))
    (when (file-exists-p custom-settings)
      (load-file custom-settings)))

  (when (getenv "INSIDE_EMACS")
    (error "Running emacs inside emacs? Are you sure about that?")
    (kill-emacs 1))

  (defun clear-kill-ring-and-gc ()
    (interactive)
    (setq kill-ring nil) (garbage-collect))
#+END_SRC


* Some Improvements

- https://www.sandeepnambiar.com/my-minimal-emacs-setup/

- As of 27.1.50:

  - gc-cons-threshold :: it was 800000 (#o3032400, #xc3500) bytes

  - large-file-warning-threshold :: 10000000 (#o46113200, #x989680)

#+begin_src emacs-lisp :tangle init.el
  (setq gc-cons-threshold 50000000)
  (setq large-file-warning-threshold 100000000)
#+end_src


* Cd to a more convenient place

On MS Windows =emacs_dir= is a special variable, see [[info:emacs#Misc Variables][Misc Variables]]
for more information.

#+BEGIN_SRC emacs-lisp :tangle init.el
  (when (string-prefix-p (expand-file-name "bin" (getenv "emacs_dir")) default-directory t)
    (setq default-directory (expand-file-name "~")))
#+END_SRC


* Remove distractions

Taken from [[http://whattheemacsd.com/][whattheemacsd.com]] and [[https://www.sandeepnambiar.com/my-minimal-emacs-setup/][www.sandeepnambiar.com]].

#+BEGIN_SRC emacs-lisp :tangle init.el
  (if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
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


* Original value of 70 is a bit too narrow

#+BEGIN_SRC emacs-lisp :tangle init.el
  (set-fill-column 90)
#+END_SRC


* Set some important environment variables

https://stackoverflow.com/questions/2183900/how-do-i-prevent-git-diff-from-using-a-pager/2183920

#+BEGIN_SRC emacs-lisp :tangle init.el
  (setenv "PAGER" "cat")
  (setenv "GIT_PAGER" "cat")

  ;; configure git color with:
  ;;  git config --global color.ui always
  ;;  git config --global color.log always
  ;;  git config --global color.diff always
  ;;  git config --global color.status always
  ;;  git config --global color.branch always
  ;;  git config -l | grep color
#+END_SRC


* Customization

#+BEGIN_SRC emacs-lisp :tangle init.el
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(blink-cursor-blinks 0)
   '(confirm-kill-emacs 'y-or-n-p)
   '(inhibit-startup-screen t)
   '(show-paren-delay 0.0)
   '(show-paren-mode t)
   '(show-paren-when-point-inside-paren t))
#+END_SRC


* Load 'wombat theme

It is such a nice theme.

#+BEGIN_SRC emacs-lisp :tangle init.el
  (when (and (display-graphic-p) (equal custom-known-themes '(user changed)))
    (load-theme 'wombat t t)
    (add-hook 'after-init-hook '(lambda () (enable-theme (car custom-known-themes))))
    (when (and (memq system-type '(ms-dos windows-nt)) (> emacs-major-version 24))
      (add-hook 'window-setup-hook '(lambda () (enable-theme (car custom-known-themes))))))
#+END_SRC

It is possible to customize a theme using these commands in =custom-settings.el=

#+BEGIN_SRC emacs-lisp
  (load-theme 'tango-dark t t)
  (add-hook 'after-init-hook '(lambda () (enable-theme (car custom-known-themes))))
#+END_SRC

See also:

- (describe-variable 'custom-enabled-themes)
- (describe-function 'disable-theme)


* Highlight Selected Window

- https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe

- TODO: highlight-selected-window is not maintaining text-scale adjustment (C-x C-=)

#+begin_src emacs-lisp :tangle no
  (defun highlight-selected-window ()
    "Highlight selected window with a different background color."
    (walk-windows (lambda (w)
                    (unless (eq w (selected-window))
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set '(:background "#111" :foreground "#444"))))))
    (buffer-face-set 'default))

  (add-hook 'window-state-change-hook 'highlight-selected-window)  ;; ideal hook
#+end_src


** Testing

#+begin_src emacs-lisp
  (view-echo-area-messages)
  (describe-function 'stringp)
  (describe-variable 'indent-tabs-mode)
#+end_src


* IDO

IDO will save you a lot of time in finding files and buffers, use
=C-f= (after =C-x C-f=) and =C-b= (after =C-x b=) to fall back to
standard minibuffer. More information in [[https://www.masteringemacs.org/article/introduction-to-ido-mode][Mastering Emacs Book]].

See also:

- https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode


#+BEGIN_SRC emacs-lisp :tangle init.el
  (require 'ido)
  (setq ido-enable-flex-matching t)
  (setq ido-auto-merge-work-directories-length -1)
  (ido-everywhere)
  (ido-mode)
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
                     (not (eq major-mode 'ibuffer-mode))
                     (not (eq major-mode 'rspec-compilation-mode))
                     (not (eq major-mode 'Buffer-menu-mode))
                     (not (eq major-mode 'prodigy-mode))
                     (not (eq major-mode 'Info-mode)))
                (setq show-trailing-whitespace t))))
#+END_SRC


* Org-Mode Customizations


** Better ellipsis

#+BEGIN_SRC emacs-lisp :tangle init.el
  ;;(setq org-ellipsis " ● ● ●")
  ;;(setq org-ellipsis " ○ ○ ○")
  (setq org-ellipsis " ◦◦◦")
#+END_SRC


** Some nice settings

#+BEGIN_SRC emacs-lisp :tangle init.el
  ;; (info-other-window "(org) Clean view")
  (setq org-hide-leading-stars t)
  (setq org-indent-mode t)
#+END_SRC


** Pre-load org and some org-babel modules useful for org-babel-tangle

#+begin_src emacs-lisp :tangle init.el
  (require 'org)
  (message "Using Org (org-mode) version %s" (org-version))
  (require 'ob-shell nil t)
  (require 'ob-perl nil t)
#+end_src


* Nice Utilities and Key Bindings

#+BEGIN_SRC emacs-lisp :tangle init.el
  (defun dos2unix ()
    "CR-LF to LF"
    (set-buffer-file-coding-system 'utf-8-unix 't))

  (defun associated-buffer-directory-name ()
    "get a file name associated with the buffer"
    (let ((bfn (buffer-file-name))
          (lbd (bound-and-true-p list-buffers-directory)))
      (or (and bfn (abbreviate-file-name (file-name-directory bfn)))
          (and lbd (abbreviate-file-name lbd)))))

  (defun rtrim-and-remove-last-slash (path)
    (replace-regexp-in-string "[\\/[:space:]]+$" "" path nil nil nil))

  (defun friendly-path (path)
    (rtrim-and-remove-last-slash (abbreviate-file-name (expand-file-name path))))

  (defun unique-buffer-name-from-path (path)
    (let ((p (friendly-path path)))
      (concat (substring (secure-hash 'sha1 p) 0 4) " " (file-name-nondirectory p))))

  (defun pop-to-ansi-term-line-mode (buffer-name &rest args)
    "pop-to-buffer-same-window and ansi-term, use C-c C-k to use char-mode, C-x C-j to bring back line-mode"
    (pop-to-buffer-same-window
     (with-current-buffer
         (apply 'term-ansi-make-term
                (or buffer-name (concat "*" (unique-buffer-name-from-path (car args)) "*"))
                (car args) nil (cdr args))
       (current-buffer))))

  (defun pop-to-ansi-term-char-mode (buffer-name &rest args)
    "pop-to-buffer-same-window and ansi-term, use C-x C-j to use line-mode, C-c C-k to bring back char-mode"
    (pop-to-buffer-same-window
     (with-current-buffer
         (apply 'term-ansi-make-term
                (or buffer-name (concat "*" (unique-buffer-name-from-path (car args)) "*"))
                (car args) nil (cdr args))
       (term-char-mode)
       (current-buffer))))

  (defun kill-ring-clear ()
    (setq kill-ring nil)
    (garbage-collect))
#+END_SRC

Listing of pop-to-ansi-term-* usage examples.

Useful shortcuts:

- C-x C-j :: switch ansi-term to line-mode
  
- C-c C-k :: switch ansi-term to char-mode

#+BEGIN_SRC emacs-lisp
  (require 'term)

  (pop-to-ansi-term-char-mode (generate-new-buffer-name "*top*") "top")

  (with-current-buffer (pop-to-ansi-term-char-mode nil "top")
    (display-line-numbers-mode 0))

  (pop-to-ansi-term-char-mode nil "/usr/bin/top")

  ;; C-c C-c to send SIGINT
  (pop-to-ansi-term-char-mode nil "watch" "-n5" "-d" "ls" "-lh")

  (pop-to-ansi-term-line-mode
   (generate-new-buffer-name "*sh*")
   "/bin/sh" "-c" "echo running $0; for i in \"$@\"; do echo \"[$i]\"; done" "inline-script" "a" "b c" " d ")
#+END_SRC

Eshell is nice and got me some workflow improvements, but grueling
bugs and anoyances appear between emacs releases, and stuff that
worked previously, stops working, using =shell= (thank you so much
Olin Shivers and Simon Marshall) and a real shell (=zsh= below) is a
more future proof and sane approach.

#+begin_src emacs-lisp :tangle init.el
  ;; "-ic" flag enable zsh aliases in shell commands, https://github.com/syl20bnr/spacemacs/issues/13401
  (when (string= "zsh" (file-name-nondirectory shell-file-name))
    (setq shell-command-switch "-ic"))

  (require 'shell) ;; define shell and comint variables
  (defun create-custom-shell (program shell-args where echoes force-new)
    "versatile custom shell creation"
    (let* ((where (expand-file-name where))
           (default-directory where)
           (path (executable-find program))
           (bname (format "*shell* %s" (unique-buffer-name-from-path where)))
           (comint-terminfo-terminal "linux")
           (explicit-shell-file-name path)
           (shell-file-name path))
      (set (intern (format "explicit-%s-args" (file-name-nondirectory path))) shell-args)
      (with-current-buffer (shell (if force-new (generate-new-buffer bname) (get-buffer-create bname)))
        ;; local variables must be set after setting buffer mode, due to
        ;; a kill-all-local-variables being issued when entering a major
        ;; mode, look at comint.el for the list of buffer local
        ;; variables
        (setq-local comint-buffer-maximum-size 100000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t)
        (setq-local comint-process-echoes echoes))))

  ;; zsh -V is for NO_PROMPT_CR (unsetopt prompt_cr)
  (defun create-zsh-shell (where &optional force-new)
    (interactive "DWhere? \nP")
    (let* ((default-directory where))
      (create-custom-shell "zsh" '("-lV") where t force-new)))

  (defun create-bash-shell (where &optional force-new)
    (interactive "DWhere? \nP")
    (let* ((default-directory where))
      (create-custom-shell "bash" '("-l") where nil force-new)))

  (defun buffer-list-shell-mode ()
    (seq-filter (lambda (b) (eq 'shell-mode (buffer-local-value 'major-mode b))) (buffer-list)))

  (defun buffer-list-files ()
    (seq-filter #'buffer-file-name (buffer-list)))

  (defun sort-predicate-has-process (a b)
    (and (get-buffer-process a) (not (get-buffer-process b))))

  (defun sort-predicate-is-writable (a b)
    (not (buffer-local-value 'buffer-read-only a)))

  (defun buffer-list-shell-mode-running-first ()
    (sort (buffer-list-shell-mode) #'sort-predicate-has-process))

  (defun buffer-list-files-writable-first ()
    (sort (buffer-list-files) #'sort-predicate-is-writable))

  (defun list-shells (&optional arg)
    (interactive "P")
    (let* ((buffers (if arg (buffer-list-files-writable-first) (buffer-list-shell-mode-running-first)))
           (buffer-list (list-buffers-noselect nil buffers))
           (column-title (if arg "Buffer (files only, writable first)" "Buffer (shells only, running first)")))
      (when buffers
        (with-current-buffer buffer-list
          (aset tabulated-list-format 3 `(,column-title 35 t))
          (tabulated-list-init-header)
          (tabulated-list-print t)))
      (pop-to-buffer-same-window buffer-list)))

  (global-set-key (kbd "C-x x") #'create-zsh-shell)

  ;; use 'T' twice (or 'g' once) in buffer list to list all buffers
  (global-set-key (kbd "C-x C-b") #'list-shells)
#+end_src


* Other (Sandeep Nambiar)

- https://www.sandeepnambiar.com/my-minimal-emacs-setup/


** line numbers

#+begin_src emacs-lisp :tangle init.el
  (global-hl-line-mode 0)
  (line-number-mode)
  (global-display-line-numbers-mode)
  (column-number-mode)
  (size-indication-mode)
  (blink-cursor-mode)
#+end_src


** current file's name to model-line

#+begin_src emacs-lisp :tangle init.el
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
#+end_src


** y-or-n for everything

- https://www.emacswiki.org/emacs/YesOrNoP

#+begin_src emacs-lisp :tangle init.el
  (defalias 'yes-or-no-p 'y-or-n-p)
#+end_src


* All Done!

Just a simple debuging message.

#+BEGIN_SRC emacs-lisp :tangle init.el
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )

  (message "All done with %s!" (file-name-nondirectory (or load-file-name buffer-file-name)))
#+END_SRC


* Automated Extraction

See [[info:org#Batch%20execution][Batch Execution]].

