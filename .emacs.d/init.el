(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmod.org/elpa"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("03d416a22ba4ebc1aeb330affb412e5601cca05bd90b1da7e62d907b1c7c4390" default)))
 '(package-selected-packages
   (quote
    (ido-vertical-mode ## ace-window avy unicode-fonts cider rainbow-delimiters paredit dired-open dired+ org-bullets diminish diff-hl))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'after-init-hook '(lambda () (load-theme 'monokai t)))

(when (> emacs-major-version 24)
  (add-hook 'window-setup-hook '(lambda () (load-theme 'monokai t))))


(load-file "~/.emacs.d/lisp/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(setq user-full-name "Rick Saenz"
      user-mail-address "richard.m.saenz@gmail.com"
      calendar-location-name "Frankfort, KY")

(defun rms/view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun rms/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun rms/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun rms/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun rms/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
     (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun rms/beautify-json ()
  "Pretty-print the JSON in the marked region. Currently shells
     out to `jsonpp'--be sure that's installed!"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jsonpp" (buffer-name) t)))

(defun rms/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun rms/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun rms/visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

(defun rms/visit-last-migration ()
  "Open the last file in 'db/migrate/'. Relies on projectile. Pretty sloppy."
  (interactive)
  (dired (expand-file-name "db/migrate" (projectile-project-root)))
  (rms/visit-last-dired-file)
  (kill-buffer "migrate"))

(defun rms/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun rms/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun rms/insert-random-string (len)
  "Insert a random alphanumeric string of length len."
  (interactive)
  (let ((mycharset "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstyvwxyz"))
    (dotimes (i len)
      (insert (elt mycharset (random (length mycharset)))))))

(defun rms/generate-password ()
  "Insert a good alphanumeric password of length 30."
  (interactive)
  (rms/insert-random-string 30))

(tool-bar-mode 0)
;(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(global-prettify-symbols-mode t)

(setq rms/default-font "Inconsolata")
(setq rms/default-font-size 10)
(setq rms/current-font-size rms/default-font-size)

(setq rms/font-change-increment 1.1)

(defun rms/set-font-size ()
  "Set the font to `rms/default-font' at `rms/current-font-size'."
  (set-frame-font
   (concat rms/default-font "-" (number-to-string rms/current-font-size))))

(defun rms/reset-font-size ()
  "Change font size back to `rms/default-font-size'."
  (interactive)
  (setq rms/current-font-size rms/default-font-size)
  (rms/set-font-size))

(defun rms/increase-font-size ()
  "Increase current font size by a factor of `rms/font-change-increment'."
  (interactive)
  (setq rms/current-font-size
        (ceiling (* rms/current-font-size rms/font-change-increment)))
  (rms/set-font-size))

(defun rms/decrease-font-size ()
  "Decrease current font size by a factor of `rms/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq rms/current-font-size
        (max 1
             (floor (/ rms/current-font-size rms/font-change-increment))))
  (rms/set-font-size))

(define-key global-map (kbd "C-)") 'rms/reset-font-size)
(define-key global-map (kbd "C-+") 'rms/increase-font-size)
(define-key global-map (kbd "C-=") 'rms/increase-font-size)
(define-key global-map (kbd "C-_") 'rms/decrease-font-size)
(define-key global-map (kbd "C--") 'rms/decrease-font-size)

(rms/reset-font-size)

(when window-system
  (global-hl-line-mode))

(require 'paredit)
(require 'rainbow-delimiters)
(require 'diminish)

(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'ruby-end 'ruby-end-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yard-mode 'yard-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(setq-default tab-width 4)

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)
            (setq css-indent-offset 4)))

(add-hook 'scss-mode-hook 'rainbow-mode)

(setq scss-compile-at-save nil)

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'parenthesis)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(setq magit-push-always-verify nil)

(defun rms/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'rms/search-project-for-symbol-at-point)


(require 'unicode-fonts)
(unicode-fonts-setup)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-index-file (org-file-path "index.org"))

(setq org-agenda-files (list org-index-file))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done 'time)

(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* TODO %?\n")

        ("g" "Groceries"
         checkitem
         (file (org-file-path "groceries.org")))

        ("l" "Today I Learned..."
         entry
         (file+datetree (org-file-path "til.org"))
         "* %?\n")

        ("r" "Reading"
         checkitem
         (file (org-file-path "to-read.org")))

        ("t" "Todo"
         entry
         (file org-index-file)
         "* TODO %?\n")))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)

(require 'dired-x)
(require 'dired+)
(require 'dired-open)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

(global-set-key (kbd "C-x k") 'rms/kill-current-buffer)
(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)

;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
;; (yas-global-mode 1)

;; (setq yas/indent-line nil)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
(ido-mode 1)
;; (ido-ubiquitous)
;(flx-ido-mode 1) ; better/faster matching
;; (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; (smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-x 2") 'rms/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'rms/split-window-right-and-switch)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(setq wgrep-auto-save-buffer t)

(defun rms/split-horizontally-for-temp-buffers ()
  (when (one-window-p t)
    (split-window-horizontally)))

(add-hook 'temp-buffer-window-setup-hook
          'rms/split-horizontally-for-temp-buffers)

(projectile-global-mode)


;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(when window-system
  (set-frame-size (selected-frame) 120 45)
  (set-frame-position (selected-frame) 0 0))

(cua-mode 1)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)
;; (setq avy-all-windows nil) ;; uncomment this to restrict to current window

(global-set-key (kbd "M-p") 'ace-window)
(setq aw-dispatch-always t)
