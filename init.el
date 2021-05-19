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

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(when (string-prefix-p (expand-file-name "bin" (getenv "emacs_dir")) default-directory t)
  (setq default-directory (expand-file-name "~")))

(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(prefer-coding-system 'utf-8-unix)

(setq indent-tabs-mode nil)
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
 '(show-paren-when-point-inside-paren t))

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

;;(setq org-ellipsis " ● ● ●")
;;(setq org-ellipsis " ○ ○ ○")
(setq org-ellipsis " ◦◦◦")

;; (info-other-window "(org) Clean view")
(setq org-hide-leading-stars t)
(setq org-indent-mode t)

(require 'org)
(message "Using Org (org-mode) version %s" (org-version))
(require 'ob-shell nil t)
(require 'ob-perl nil t)

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

(global-hl-line-mode 0)
(line-number-mode)
(global-display-line-numbers-mode 0)
(global-linum-mode)
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
