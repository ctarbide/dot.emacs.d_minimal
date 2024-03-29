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

(defmacro z--defun-name-aware (name arglist &optional docstring &rest body)
  (declare (doc-string 3) (indent 2))
  (if (or (stringp docstring)
          (eq 'interactive (car-safe docstring))
          (eq 'declare (car-safe docstring)))
      `(defun ,name ,arglist ,docstring (let ((defun-name ',name)) ,@body))
    ;; assume docstring is part of the body
    `(defun ,name ,arglist (let ((defun-name ',name)) ,docstring ,@body))))

(when (<= gc-cons-threshold 800000)
  (setq gc-cons-threshold 10000000))
(when (<= large-file-warning-threshold 10000000)
  (setq large-file-warning-threshold 100000000))

(when (string-prefix-p (expand-file-name "bin" (getenv "emacs_dir")) default-directory t)
  (setq default-directory (expand-file-name "~")))

;; enable (menu is didactic)
(if (fboundp 'menu-bar-mode) (menu-bar-mode))

;; disable
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(prefer-coding-system 'utf-8-unix)

(setq-default indent-tabs-mode nil)

(set-fill-column 90)

(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")

;; configure git color with:
;;  git config --global color.ui always
;;  git config --global color.log always
;;  git config --global color.diff always
;;  git config --global color.status always
;;  git config --global color.branch always
;;  git config -l | grep color

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
 '(show-paren-when-point-inside-paren t)
 ;; '(org-ellipsis " ● ● ●")
 ;; '(org-ellipsis " ○ ○ ○")
 '(org-ellipsis " ◦◦◦")
 '(org-hide-leading-stars t)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (perl . t)
     (shell . t))))

(when (and (display-graphic-p) (equal custom-known-themes '(user changed)))
  (load-theme 'wombat t t)
  (add-hook 'after-init-hook '(lambda () (enable-theme (car custom-known-themes))))
  (when (and (memq system-type '(ms-dos windows-nt)) (> emacs-major-version 24))
    (add-hook 'window-setup-hook '(lambda () (enable-theme (car custom-known-themes))))))

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(ido-everywhere)
(ido-mode)

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

;; "-ic" flag enable zsh aliases in shell commands, https://github.com/syl20bnr/spacemacs/issues/13401
(when (string= "zsh" (file-name-nondirectory shell-file-name))
  (setq shell-command-switch "-ic"))

(require 'shell) ;; define shell and comint variables

;; TODO: error if 'where does not exist
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
    (create-custom-shell "zsh" '("-iV") where t force-new)))

(defalias 'zsh #'create-zsh-shell)

(defun create-bash-shell (where &optional force-new)
  (interactive "DWhere? \nP")
  (let* ((default-directory where))
    (create-custom-shell "bash" '("-i") where nil force-new)))

(defalias 'bash #'create-bash-shell)

(defun buffer-list-shell-mode ()
  (seq-filter (lambda (b) (eq 'shell-mode (buffer-local-value 'major-mode b))) (buffer-list)))

(defun buffer-list-files ()
  (seq-filter #'buffer-file-name (buffer-list)))

(defun z--buffer-is-writable-p (buf)
  (not (buffer-local-value 'buffer-read-only buf)))

(defun sort-predicate-has-process (a b)
  (and (get-buffer-process a) (not (get-buffer-process b))))

(defun sort-predicate-writable (a b)
  (and (z--buffer-is-writable-p a) (not (z--buffer-is-writable-p b))))

(defun sort-predicate-modified (a b)
  (and (buffer-modified-p a) (not (buffer-modified-p b))))

(defun buffer-list-shell-mode-running-first (&optional buffer-list)
  (sort (or buffer-list (buffer-list-shell-mode)) #'sort-predicate-has-process))

(defun buffer-list-files-writable-first (&optional buffer-list)
  (sort (or buffer-list (buffer-list-files)) #'sort-predicate-writable))

(defun buffer-list-files-modified-first (&optional buffer-list)
  (sort (or buffer-list (buffer-list-files)) #'sort-predicate-modified))

(defun buffer-list-files-modified-writable-first (&optional buffer-list)
  (sort (or buffer-list (buffer-list-files))
        (lambda (a b)
          (let ((m (sort-predicate-modified a b))
                (w (sort-predicate-writable a b)))
            (or (and m w) m w)))))

(defun list-shells (&optional arg)
  (interactive "P")
  (let* ((buffers (if arg (buffer-list-files-modified-writable-first)  (buffer-list-shell-mode-running-first)))
         (buffer-list (list-buffers-noselect nil buffers))
         (column-title (if arg "Buffer (files only, writable first)" "Buffer (shells only, running first)")))
    (when buffers
      (with-current-buffer buffer-list
        (aset tabulated-list-format 3 `(,column-title 35 t))
        (tabulated-list-init-header)
        (tabulated-list-print t)))
    (pop-to-buffer-same-window buffer-list)))

(z--defun-name-aware z--create-shell (where &optional force-new)
  (interactive "DWhere? \nP")
  (defalias defun-name
    (if (equal 0 (call-process "which" nil nil nil "zsh"))
        #'create-zsh-shell
      #'create-bash-shell))
  (funcall (symbol-function defun-name) where force-new))

(global-set-key (kbd "C-x x") 'z--create-shell)

;; use 'T' twice (or 'g' once) in buffer list to list all buffers
(global-set-key (kbd "C-x C-b") #'list-shells)

(global-hl-line-mode 0)
(line-number-mode)
(global-display-line-numbers-mode 0)
(global-linum-mode 0)
(column-number-mode)
(size-indication-mode)
(blink-cursor-mode)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "All done with %s!" (file-name-nondirectory (or load-file-name buffer-file-name)))
