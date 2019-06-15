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
    tramp
    helm-tramp
    color-theme-solarized
    highlight-numbers
    magit
    emacs-bind-map
    projectile
    helm-projectile
    helm-rg
    ))

;;(load-theme 'jbeans t)
(load-theme 'solarized t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'dark 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'ivy-rich)
(ivy-rich-mode)
(require 'general)
(require 'help-fns+)
(require 's)
(require 'evil-evilified-state)

(server-start)
(require 'org-protocol)

(global-set-key (kbd "M-x") #'helm-M-x)

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
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'highlight-numbers-mode)


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
 "RET" #'Info-follow-nearest-node
 "h" #'evil-backward-char
 "l" #'evil-forward-char)

(defun cw/copy-key (keymap-from state key)
     "Moves key binding from one keymap to another, deleting from the old location. "
     (evil-define-key state keymap-from key (lookup-key keymap-from key)))
;;(my-move-key minibuffer-local-map 'normal (kbd "C-j"))

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

(require 'projectile)

(use-package projectile
  :general
  (:keymaps '(projectile-mode-map)
            :states '(normal)
            :prefix cw/normal-prefix
            "a p" #'projectile-command-map)
  :config
  (projectile-mode +1))

(defun cw/compile-ragefurnace ()
  (org-publish-project "ragefurnace") )

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

(general-define-key
 :keymaps '(python-mode-map)
 "C-c c" #'comment-or-uncomment-region
 )

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
        '(
          ("n" "Next Action" entry
           (file "~/org/capture.org") "* NEXT %?\n  captured: %U"
           :empty-lines 1)
          ("N" "Next Action with Gmail Id" entry
           (file "~/org/capture.org") "* NEXT %?\n  captured: %U\n  [[gmail:%^{gmail id}][%\\1]]"
           :empty-lines 1)
          ("c" "Conversation memo" entry
           (file "~/org/conversations.org") "* %U\n %?"
	         :empty-lines 1)
          ("p" "Protocol" entry (file+headline "~/org/capture_from_web.org" "Inbox")
           "* %a\nCaptured On: %U\nWebsite: %l\n\n%i\n%?")
          ("l" "Protocol Link" entry (file+headline "~/org/capture_from_web.org" "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")))
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-doc-mode 0)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote auto))
 '(haskell-tags-on-save t)
 '(package-selected-packages (quote (projectile)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . cw/compile-ragefurnace)
     (projectile-project-name . ragefurnace\.))))
 '(solarized-bold nil)
 '(solarized-termcolors 256)
 '(solarized-underline nil))

(require 'haskell-interactive-mode)
(require 'haskell-process)

(setq haskell-process-use-presentation-mode t)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; Disable haskell-doc-mode here because the haskell types in the notification
;; area are really annoying. This is really just a hack that I don't fully understand.
;; The first time you run C-c C-t after loading the repl, an error is thrown but after that everything works great
(add-hook 'interactive-haskell-mode-hook '(setq haskell-doc-mode 0))

;; Org-babel stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

;; Makes <tab> behave as expected in source blocks
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

(let ((private-init "~/.emacs.d/private/private.el"))
  (when (file-exists-p private-init)
    (load-file private-init)))

(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together"))))
     ;;((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      ;;(setq args "-d en_US"))
     ))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

(let ((work-settings "~/.emacs.d/work-settings.el"))
  (when (file-exists-p work-settings)
    (load-file work-settings)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:background "brightcyan" :foreground "black" :inverse-video t))))
 '(helm-selection ((t (:foreground "black"))))
 '(isearch ((t (:foreground "black" :background "yellow" :inverse-video nil))))
 '(match ((t (:background "brightcyan" :foreground "black" :inverse-video t))))
 '(region ((t (:foreground "black" :background "brightcyan" :inverse-video nil)))))

;; This is a dirty hack to make the forground colors show through the
;; highlight due to some buggyness in the solarized theme
;;(set-face-attribute
;; 'region nil :background "brightmagenta"
;; :foreground 'unspecified :inverse-video nil) 

;;(set-face-attribute
;; 'region nil :background "brightcyan"
;; :foreground "black" :inverse-video nil) 
