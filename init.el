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
;;  git config -l | *grep color

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(buffers-menu-buffer-name-length 50)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cursor-type 'hbar)
 '(inhibit-startup-screen t)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(show-paren-when-point-inside-paren t))

(when (equal custom-known-themes '(user changed))
  (load-theme 'wombat t t)
  (add-hook 'after-init-hook '(lambda () (enable-theme (car custom-known-themes))))
  (when (and (memq system-type '(ms-dos windows-nt)) (> emacs-major-version 24))
    (add-hook 'window-setup-hook '(lambda () (enable-theme (car custom-known-themes))))))

(require 'eshell)
(require 'em-basic)
(require 'em-unix)
(require 'esh-var)

(setq eshell-buffer-maximum-lines 10000)
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(setq eshell-history-size 1000) ;; default is 128

;; blacklist eshell non-standard sub-par re-implementations on
;; non-windows systems
(when (not (eshell-under-windows-p))
  (fmakunbound 'eshell/basename)
  (fmakunbound 'eshell/cat)
  (fmakunbound 'eshell/cp)
  (fmakunbound 'eshell/date)
  (fmakunbound 'eshell/diff)
  (fmakunbound 'eshell/dirname)
  (fmakunbound 'eshell/du)
  (fmakunbound 'eshell/echo)
  (fmakunbound 'eshell/egrep)
  (fmakunbound 'eshell/env)
  (fmakunbound 'eshell/fgrep)
  (fmakunbound 'eshell/grep)
  (fmakunbound 'eshell/ln)
  (fmakunbound 'eshell/locate)
  (fmakunbound 'eshell/ls)
  (fmakunbound 'eshell/make)
  (fmakunbound 'eshell/man)
  (fmakunbound 'eshell/mkdir)
  (fmakunbound 'eshell/mv)
  (fmakunbound 'eshell/rm)
  (fmakunbound 'eshell/rmdir)
  (fmakunbound 'eshell/sudo)
  (fmakunbound 'eshell/time)
  (fmakunbound 'eshell/umask)
  (fmakunbound 'eshell/whoami))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq
    pcomplete-cycle-completions nil
    pcomplete-cycle-cutoff-length 0
    pcomplete-dir-ignore nil
    pcomplete-file-ignore nil
    pcomplete-use-paring nil)))

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
                   (not (eq major-mode 'eshell-mode))
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
  "as it's name implies"
  (replace-regexp-in-string "[\\/[:space:]]+$" "" path nil nil nil))

(defun unique-buffer-name-from-path (path)
  (let ((p (rtrim-and-remove-last-slash path)))
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

(defun eshell/lspath-exec-path ()
  "list path from exec-path"
  (mapconcat 'identity exec-path "\n"))

(defun eshell/lspath-path ()
  "list path from PATH environment variable"
  (mapconcat 'identity (split-string (getenv "PATH") path-separator) "\n"))

(defun eshell/lspath-eshell-path-env ()
  "list path from eshell-path-env variable"
  (mapconcat 'identity (split-string eshell-path-env path-separator) "\n"))

(defun eshell-here-maybe-reuse-existing ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (let* ((parent (or
                  (associated-buffer-directory-name)
                  (and (bound-and-true-p default-directory)
                       (abbreviate-file-name default-directory))
                  (error "could not determine parent directory")))
         (height (/ (window-total-height) 3))
         (name (unique-buffer-name-from-path parent))
         (eshell-buffer-name (concat "*eshell: " name "*"))
         (buffer (get-buffer-create eshell-buffer-name))
         (window (or (get-buffer-window buffer 'visible)
                     (split-window-vertically (- height)))))
    (select-window window)
    (switch-to-buffer buffer nil t)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))))

(defun eshell-here-force-new ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (let* ((parent (or
                  (associated-buffer-directory-name)
                  (and (bound-and-true-p default-directory)
                       (abbreviate-file-name default-directory))
                  (error "could not determine parent directory")))
         (height (/ (window-total-height) 3))
         (name (unique-buffer-name-from-path parent))
         (eshell-buffer-name (concat "*eshell: " name "*"))
         (buffer (generate-new-buffer eshell-buffer-name)))
    (select-window (split-window-vertically (- height)))
    (switch-to-buffer buffer nil t)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))))

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
  (if (listp arg) (setq arg (car arg)))
  (if (not (file-directory-p arg)) (error "\"%s\" is not a directory" arg))
  (let* ((dir (abbreviate-file-name (expand-file-name arg)))
         (default-directory (file-name-as-directory (expand-file-name dir)))
         (name (unique-buffer-name-from-path dir))
         (buf (generate-new-buffer (format "*eshell: %s*" name))))
                                        ; (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

(defun eshell/get-eshell-at (arg)
  "get or create a new eshell with the directory name in the buffer name"
  (if (listp arg) (setq arg (car arg)))
  (if (not (file-directory-p arg)) (error "\"%s\" is not a directory" arg))
  (let* ((dir (abbreviate-file-name (expand-file-name arg)))
         (default-directory (file-name-as-directory (expand-file-name dir)))
         (name (unique-buffer-name-from-path dir))
         (buf (get-buffer-create (format "*eshell: %s*" name))))
                                        ; (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))
(put 'eshell/get-eshell-at 'eshell-no-numeric-conversions t)

(defalias 'eshell/e 'eshell/get-eshell-at)
(put 'eshell/e 'eshell-no-numeric-conversions t)

;; "-ic" flag enable zsh aliases in shell commands, https://github.com/syl20bnr/spacemacs/issues/13401
(when (string= "zsh" (file-name-nondirectory shell-file-name))
  (setq shell-command-switch "-ic"))

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
      (setq-local comint-process-echoes echoes))))

(defun create-zsh-shell (where &optional force-new)
  (interactive "DWhere? \nP")
  (let* ((default-directory where))
    (create-custom-shell "zsh" '("-l") where t force-new)))

(defun create-bash-shell (where &optional force-new)
  (interactive "DWhere? \nP")
  (let* ((default-directory where))
    (create-custom-shell "bash" '("-l") where nil force-new)))

(defun buffer-list-shell-mode ()
  (seq-filter (lambda (b) (eq 'shell-mode (buffer-local-value 'major-mode b))) (buffer-list)))

(defun sort-predicate-has-process (a b)
  (and (get-buffer-process a) (not (get-buffer-process b))))

(defun buffer-list-shell-mode-running-first ()
  (sort (buffer-list-shell-mode) #'sort-predicate-has-process))

(defun list-shells (&optional arg)
  (interactive "P")
  (pop-to-buffer-same-window
   (if arg
       (list-buffers-noselect arg)
     (list-buffers-noselect nil (buffer-list-shell-mode-running-first)))))

(global-set-key (kbd "C-x x") #'create-zsh-shell)

;; use 'T' twice (or 'g' once) in buffer list to list all buffers
(global-set-key (kbd "C-x C-b") #'list-shells)

(global-hl-line-mode 0)
(line-number-mode)
(global-display-line-numbers-mode)
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

(message "All done with %s!" "init.el")
