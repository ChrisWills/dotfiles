(package-initialize)

(setq evil-want-keybinding nil)

;; Automatically move cursor to help windows
(setq help-window-select t)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
	  evil-surround
	  evil-collection
	  use-package
	  ivy-rich
	  swiper
	  helm
	  general
	  cwills-jbeans-theme
	  help-fns+
	  helm-descbinds
	  which-key))


(require 'general)
(require 'help-fns+)
(require 'helm-descbinds)
(require 'which-key)

(evil-mode 1)

(use-package evil-collection
    :after (evil)
    :config
    (evil-collection-init))
(helm-descbinds-mode)
(which-key-mode)

(load-theme 'jbeans t)

(defconst cw/normal-prefix "SPC")
(defconst cw/non-normal-prefix "M-SPC")

(general-define-key
 :keymaps '(motion)
  "SPC" nil
  ","   nil)

(general-define-key
 :keymaps '(override)
 :states '(normal motion insert emacs)
 :prefix cw/normal-prefix
 :non-normal-prefix cw/non-normal-prefix
 "SPC"   '(helm-M-x :which-key "M-X")
 ;; Files
 "f"     '(:ignore t :which-key "files")
 "f f"   #'helm-find-files
 "f j"   #'dired-jump
 ;; Buffers
 "b"   '(:ignore t :which-key "buffers")
 "TAB"   #'evil-switch-to-windows-last-buffer
 "b b"   #'helm-mini
 "b d"   #'kill-this-buffer
 "b r"   #'revert-buffer
 ;; Windows
 "w"     #'evil-window-map  
 "w TAB" #'evil-window-next
 "w -"	 #'evil-window-split
 "w /"	 #'evil-window-vsplit
 "w m"	 #'delete-other-windows
 "w d"   #'delete-window
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



;; "a"   '(:ignore t :which-key "applications")
;; "b"   '(:ignore t :which-key "buffers")
;; "w"   '(:ignore t :which-key "windows")
;; "f"   '(:ignore t :which-key "files")
;; "h"   #'help
;; "f f" #'cp/find-file
;; "f r" #'find-file-read-only
;; "f j" #'dired-jump
;; "b b" #'switch-to-buffer
;; "b k" #'kill-buffer
;; "b K" #'kill-buffer-and-window
;; "b r" #'revert-buffer
;; "b R" #'cp/revert-buffer-all
;; "b f" #'(lambda () (interactive) (message (buffer-file-name)))
;; "w s" #'split-window-vertically
;; "w v" #'split-window-horizontally
;; "w K" #'kill-buffer-and-window
;; "w o" #'delete-other-windows
;; "w x" #'delete-window
;; "w =" #'balance-windows)
