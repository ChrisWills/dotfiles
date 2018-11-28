(package-initialize)

(let ((work-init "~/.emacs.d/work-init.el"))
  (when (file-exists-p work-init)
    (load-file work-init)))

;; When the cursor goes outside the window emacs usualy recenters the
;; point. This is a bit jarring so this behaves a lot nicer.
(setq scroll-conservatively 100)

;; Stop ivy minibuffers from leaving a weird blank space after a multi-line
;; result
;; This turned out to be more annoying than the original behavior...
;;(setq resize-mini-windows t)

;; This fixes some weird behavoir that I cannot recall...
(setq evil-want-keybinding nil)
;; Get TAB functionality back in evil-org
(setq evil-want-C-i-jump nil)
;; Automatically move cursor to help windows
(setq help-window-select t)

;; Save location within a file
(save-place-mode t)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(column-number-mode 1)                       ; show column numbers in mode line
(global-hl-line-mode 1)                      ; highlight current line
(setq-default indent-tabs-mode nil)          ; use spaces instead of tabs
(setq-default tab-width 2)                   ; 2-space tabs
(setq-default fill-column 80)                ; 80 character line width
(electric-indent-mode 1)                     ; smart auto-indent
(setq-default electric-indent-inhibit t)     ; ... disable indenting previous line (WHY?!)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda () (set-fill-column 80)))

;; disable automatic type signature in echo area by default
(setq global-eldoc-mode nil)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path "~/.emacs.d/user-recipes")
(el-get 'sync
	'(ace-window
	  evil
	  evil-leader
	  evil-surround
	  evil-collection
	  use-package
	  highlight-parentheses
	  swiper
    ivy-rich
	  smartparens
	  helm
    helm-gtags
	  general
	  cwills-jbeans-theme
	  help-fns+
	  org-mode
	  org-bullets
	  evil-org-mode
	  which-key
    shackle
    s
    company-mode
    ox-twbs
    tuareg-mode
    haskell-mode
    ghc-mod
    ))

(load-theme 'jbeans t)

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'ivy-rich)
(ivy-rich-mode)
(require 'general)
(require 'help-fns+)
(require 's)

;; Disable automatic completion for now
(setq company-idle-delay nil)
;; Enable company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; Starting to question how helpful this actually is...
(require 'smartparens-config)
(eval-after-load 'emacs-lisp-mode
  '((require 'smartparens)
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions :rem)))
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'haskell-mode-hook #'smartparens-mode)

;; Always create a new full-width window on the bottom third of the screen for
;; helm and help windows
(use-package shackle
  :init
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
                        ("\\`\\*Help\\*\\'" :regexp t :align t :size 0.3)
                        ("\\`\\*Haskell Presentation\\*\\'" :regexp t :align t :size 0.3)))
  :config
  (shackle-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package evil
  :config
  (progn
    (use-package evil-collection
      :config
      (evil-collection-init)))
  (progn
    (use-package evil-surround
      :general
      (:keymaps '(evil-surround-mode-map)
       :states '(visual)
       "S" nil 
       "s" #'evil-surround-region)))
      :config 
  (evil-mode 1))

(defconst cw/normal-prefix "SPC")
(defconst cw/non-normal-prefix "M-SPC")

(defun cw/counsel-rg-type-list ()
  (thread-last (shell-command-to-string "rg --type-list")
    (s-split "\n")))

(defun cw/counsel-rg-with-prefix-arg (&rest args)
  (let ((current-prefix-arg '(4)))
    (apply #'counsel-rg args)))

(defun cw/counsel-rg-prompt-dir (&rest args)
  "A wrapper around `counsel-rg' that always prompts for a directory,
but if a prefix arg has already been specified, just passes that
through to the underlying function"
  (interactive)
  (if current-prefix-arg
      (call-interactively #'counsel-rg)
    (cw/counsel-rg-with-prefix-arg "" nil "" nil)))

(defun cw/counsel-rg-cwd (&rest args)
  "A wrapper around `counsel-rg' that always prompts for a directory,
but if a prefix arg has already been specified, just passes that
through to the underlying function"
  (interactive)
  (if current-prefix-arg
      (call-interactively #'counsel-rg)
    (cw/counsel-rg-with-prefix-arg "" default-directory "" nil)))

(defun cw/counsel-rg-with-type (&optional types prompt)
  "Prompt for a supported file type from rg and then run
`counsel-rg' as if a prefix arg was passed, but explicitly
setting the args to `-t TYPE' instead of prompting."
  (interactive)
  (let* ((file-types
          (or types
              (thread-last (ivy-read "File type: " (cw/counsel-rg-type-list) :require-match t)
                (s-split ":")
                (nth 0)
                (list))))
         (extra-rg-args
          (s-join " " (seq-map (lambda (type) (format "-t%s" type)) file-types))))
    (cw/counsel-rg-with-prefix-arg nil nil extra-rg-args prompt)))

(defun cw/counsel-rg-with-type-ocaml ()
  (interactive)
  (cw/counsel-rg-with-type '("ocaml") "rg (ocaml)"))

(defun cw/counsel-rg-with-type-c ()
  (interactive)
  (cw/counsel-rg-with-type '("c") "rg (c)"))

(defun cw/counsel-rg-with-type-ocaml-or-c ()
  (interactive)
  (cw/counsel-rg-with-type '("ocaml" "c") "rg (ocaml or c)"))

(defun cw/counsel-rg-with-type-elisp ()
  (interactive)
  (cw/counsel-rg-with-type '("elisp") "rg (elisp)"))

(defun cw/counsel-rg-with-type-lisp ()
  (interactive)
  (cw/counsel-rg-with-type '("lisp") "rg (lisp)"))

;;(general-define-key
;; :keymaps '(insert)
;; "C-c c" '(company-complete :which-key "company-complete"))

(general-define-key
 :keymaps '(normal motion)
  "SPC" nil)

(general-define-key
 :keymaps '(visual)
  "M->" '(eval-region :which-key "eval-region"))

(general-define-key
 :keymaps '(haskell-mode-map)
 :states '(normal)
 :prefix ","
 "'" '(haskell-interactive-bring :which-key "repl")
;; "c" compile-sub-tree
;; "c c" compile
;; "c p" list builds
;; "e n" next-error
;; "e d" type mismatch-diff
;; "e a" other-error-location
;; "g ."
;; "g a" find alternate file
;; "g f" jump fun
;; "g I" jump to interface in new window
;; "g i" jump to interface
;; "g m" jump to module
;; "g o" jump occurance
;; "g G" def in new window
 "g d" '(haskell-mode-jump-to-def-or-tag :which-key "jump-to-def")
 "t"   '(haskell-process-do-type :which-key "type-of-expr")
;; "H t" type enclosing
;; "h T" type-expression
;; "h h" find documentation
;; "m" merlin/repl start/stop
;; "s" send to repl/eval in repl jump to repl eval region in repl
 "s f" '(haskell-process-load-file :which-key "send-file-to-repl")
 )

(general-define-key
 :keymaps '(Info-mode-map)
 :states '(normal motion)
 "h" #'evil-backward-char
 "l" #'evil-forward-char)

(general-define-key
 :keymaps '(haskell-presentation-mode-map)
 :states '(normal motion insert)
 "q" (lambda () (interactive) (haskell-presentation-clear) (quit-window)))

(general-define-key
 :keymaps '(override)
 :states '(normal motion insert emacs)
 :prefix cw/normal-prefix
 :non-normal-prefix cw/non-normal-prefix
 "SPC"   '(helm-M-x :which-key "M-X")
 ;;"SPC"   '(counsel-M-x :which-key "M-X")
 ;; Files
 "f"     '(:ignore t :which-key "files")
 "f f"   #'helm-find-files
 ;;"f f"   #'counsel-find-file
 "f j"   #'dired-jump
 "f e d" #'(lambda () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
 ;; Buffers
 "b"   '(:ignore t :which-key "buffers")
 "TAB"   '(evil-switch-to-windows-last-buffer :which-key "switch-to-last-buffer")
 "b b"   #'helm-mini
 ;;"b b"   #'ivy-switch-buffer
 "b d"   #'kill-this-buffer
 "b r"   #'revert-buffer
 ;; Windows
 "w"     #'evil-window-map  
 "w TAB" #'evil-window-next
 "w -"	 #'evil-window-split
 "w /"	 #'evil-window-vsplit
 "w m"	 #'delete-other-windows
 "w d"   #'delete-window
 "a"     '(:ignore t :which-key "Applications")
 "a c"   '(company-complete :which-key "company-complete")
 "a g"   '(:ignore t :which-key "Grep")
 "a g r" '(cw/counsel-rg-prompt-dir :which-key "counsel-rg-prompt-dir")
 "a g R" '(cw/counsel-rg-with-type :which-key "counsel-rg-prompt-type")
 "a g O" '(cw/counsel-rg-with-type-ocaml :which-key "counsel-rg-ocaml")
 "a g C" '(cw/counsel-rg-with-type-c :which-key "counsel-rg-c")
 "a g J" '(cw/counsel-rg-with-type-ocaml-or-c :which-key "counsel-rg-ocaml/c")
 "a g E" '(cw/counsel-rg-with-type-elisp :which-key "counsel-rg-elisp")
 "a g L" '(cw/counsel-rg-with-type-lisp :which-key "counsel-rg-lisp")
 "a o"   '(:ignore t :which-key "Org")
 "a o c" #'org-capture 
 "a o a" #'org-agenda
 )

(general-define-key
 :keymaps '(helm-map)
 "TAB" #'helm-execute-persistent-action
 "<tab>" #'helm-execute-persistent-action
 "C-z" #'helm-select-action)

;; Make <escape> quit as much as possible 
;; Stolen from spacemacs/layers/+spacemacs/spacemacs-defaults/keybindings.el
(general-define-key
 :keymaps '(minibuffer-local-map minibuffer-local-ns-map minibuffer-local-completion-map minibuffer-local-must-match-map minibuffer-local-isearch-map ivy-minibuffer-map)
 "<escape>" #'keyboard-escape-quit)

(general-define-key
 :keymaps '(help-map)
 "<escape>" #'help-quit)

(general-define-key
 :keymaps '(ivy-minibuffer-map)
 [remap ivy-next-history-element] 'ignore
 [remap ivy-previous-history-element] 'ignore
 [remap ivy-yank-word] 'ignore
 "<up>"   #'ivy-previous-line
 "<down>" #'ivy-next-line
 "M-k"    #'ivy-previous-line
 "M-j"    #'ivy-next-line)

(setq ivy-height 15)
(setq ivy-initial-inputs-alist nil)
(setq ivy-format-function 'ivy-format-function-arrow)
(setq ivy-count-format "%d/%d ")

(use-package highlight-parentheses
  :config
  (setq hl-paren-delay 0.2)
  (setq hl-paren-colors '("Springgreen3"  
			  "IndianRed1" 
			  "IndianRed3" 
			  "IndianRed4"))                                                                                                                 
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold) 
  (add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode))

(use-package org
  :demand t
  :general
  (:keymaps '(org-mode-map)
   :states  '(normal motion emacs)
   "C-c t" #'org-todo)
  :config
  (progn
    (use-package org-tempo))
  (progn
    (use-package evil-org
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'evil-org-mode-hook
                (lambda ()
                  (evil-org-set-key-theme)))
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys)))
  (progn
    (use-package org-bullets
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
      (setcdr org-bullets-bullet-map nil)))
  (setq org-todo-keywords
        '((sequence "NEXT(n!)" "WAIT(w@\|)" "DPND(x@)" "DFER(r@)" "|" "DONE(d!)" "CNCL(c@)")))
  (setq org-todo-keyword-faces
        '(("NEXT" . (:weight bold :foreground "Pink"))
          ("WAIT" . (:weight bold :foreground "Pink"))
          ("DONE" . (:foreground "PaleGreen" :weight bold))))
  (setq org-capture-templates
        '(("n" "Next Action" entry
           (file "~/org/capture.org") "* NEXT %?\n  captured: %U"
           :empty-lines 1)
          ("N" "Next Action with Gmail Id" entry
           (file "~/org/capture.org") "* NEXT %?\n  captured: %U\n  [[gmail:%^{gmail id}][%\\1]]"
           :empty-lines 1)
          ("c" "Conversation memo" entry
           (file "~/org/conversations.org") "* %U\n %?"
	   :empty-lines 1)))
  (setq org-link-abbrev-alist
        '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")
          ("jira" . "https://jira.delacy.com:8443/browse/%s")))
  (setq org-agenda-files '("~/org/")))

;; Requires "global" to be installed on the OS
(use-package helm-gtags
  :after helm
  :init
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-suggested-key-mapping t)
  :general
  (:keymaps '(helm-gtags-mode-map)
   :states  '(normal motion emacs)
   "C-c g c" #'helm-gtags-create-tags
   "C-c g a" #'helm-gtags-tags-in-this-function
   "C-c g ." #'helm-gtags-select
   "M-."     #'helm-gtags-dwim
   "M-,"     #'helm-gtags-pop-stack
   "C-c <"   #'helm-gtags-previous-history
   "C-c >"   #'helm-gtags-next-history))


(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

(setq ghc-debug t) ; enable debug logging
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook #'highlight-parentheses-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl)
  '(haskell-tags-on-save t)
  '(haskell-doc-mode 0))

(require 'haskell-interactive-mode)
(require 'haskell-process)

(setq haskell-process-use-presentation-mode t)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; Disable haskell-doc-mode here because the haskell types in the notification
;; area are really annoying. This is really just a hack that I don't fully understand.
;; The first time you run C-c C-t after loading the repl, an error is thrown but after that everything works great
(add-hook 'interactive-haskell-mode-hook '(setq haskell-doc-mode 0))

(let ((work-settings "~/.emacs.d/work-settings.el"))
  (when (file-exists-p work-settings)
    (load-file work-settings)))
