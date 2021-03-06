;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;;dotspacemacs-configuration-layer-path '("/j/office/app/emacs/dev/jane-elisp/elisp/spacemacs-layers/")
   ;; dotspacemacs-configuration-layer-path '("/usr/local/home/cwills/workspaces/jane/emacs/dev/fix-wave-style-face-error-in-terminal/+share+/app/emacs/elisp/spacemacs-layers")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(ruby
     yaml
     ;; ----------------------------------------------------------------
     html
     markdown
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;;ivy
     auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     org
     cscope
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;;syntax-checking
     ;;(syntax-checking :variables
     ;;                 syntax-checking-enable-by-default nil)
     ;;python
     version-control
     haskell
     ;;jane
     ;;cwills
     ;;themes-megapack
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '((ios-config-mode :location
                                       (recipe :fetcher github
                                               :repo "nibrahim/IOS-config-mode"
                                               :commit "ef1d26a3983006cb4574e4c14fcf36ceeda4a260"))
                                      (s :location
                                         (recipe :fetcher github
                                                 :repo "magnars/s.el"
                                                 :commit "03410e6a7a2b11e47e1fea3b7d9899c7df26435e"))
                                      (helm-gtags :location
                                                  (recipe :fetcher github
                                                          :repo "syohex/emacs-helm-gtags"
                                                          :commit "108e93d0d099ebb7b98847388f368311cf177033")))
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(evil-escape)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;;dotspacemacs-themes '(spacemacs-dark)
   ;;dotspacemacs-themes '((jbeans :location local))
   dotspacemacs-themes '((jbeans :location (recipe :fetcher github
                                                   :repo "ChrisWills/cwills-jbeans-theme")))
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  (setq epg-gpg-program "gpg")
  (setenv "GPG_AGENT_INFO" nil)
  (setq epa-pinentry-mode 'loopback)
  (fset 'evil-visual-update-x-selection 'ignore)
  (xterm-mouse-mode -1)
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
;;  (add-to-list 'custom-theme-load-path "~/.emacs.d/private/local/cwillsjelly-theme")
;;  (load-theme 'cwillsjelly t)
(defmacro cp/generate-category-agenda-cmds (letter desc categories include days-out &optional options)
  "Generate a set of commands for org-agenda-custom-commands.

Generate a form that looks like (key desc (cmd1 cmd2 ...)
general-settings-for-whole-set) where cmd1 is an agenda command, cmd2
though cmd5 are tags-todo searches and all of them are restricted to
just the categories that we've been passed.  Whether or not we're
including those categories, or excluding those categories is
controlled by `include'."
  (let* ((options (or options ()))
         (include-exclude (if include "+" "-"))
         (category-regex (s-join "\|" categories))
         (todo-keywords '("NEXT" "WAIT" "DPND" "DFER"))
         (scheduled-or-deadline-days-out (1- days-out))
         (search-string-fmt
          (apply-partially
           #'format
           (s-concat
            (s-join
             "|"
             '("-DEADLINE={.+}&-SCHEDULED={.+}&%sCATEGORY={%s}"
               "+DEADLINE>=\"<+%dd>\"&%sCATEGORY={%s}"
               "+SCHEDULED>=\"<+%dd>\"&%sCATEGORY={%s}"))
            "/%s")
           include-exclude category-regex
           scheduled-or-deadline-days-out include-exclude category-regex
           scheduled-or-deadline-days-out include-exclude category-regex))
         (tags-todo-cmds
          (-map
           (lambda (todo)
             `(tags-todo
              ,(funcall search-string-fmt todo)
              ((org-agenda-overriding-header
                ,(format
                  "%s No deadline, not scheduled, or deadline/scheduled is %d days out or more"
                  todo
                  days-out))
               (org-agenda-sorting-strategy '(deadline-up tsia-up)))))
           todo-keywords))
         (category-filter
          (-map
           (lambda (category) (s-concat include-exclude category))
           categories))
         (agenda-header-fmt
          (format "Agenda for the next %dd (W%%W) (generated %%Y-%%m-%%d %%H:%%M:%%S)" days-out))
         (agenda-header `(format-time-string ,agenda-header-fmt))
         (forms
          `(,letter
            ,desc
            ,(cons
              `(agenda ""
                       ((org-agenda-span ,days-out)
                        (org-deadline-warning-days ,days-out)
                        (org-agenda-overriding-header ,agenda-header)
                        (org-agenda-sorting-strategy '(time-up deadline-up tsia-up))))
              tags-todo-cmds)
            ; CR cperl: It would be nice to use org-agenda-category-filter-preset here,
            ; but it has issues when using sticky agenda and generating multiple agendas
            ; and the filter seems to be global and pressing "r" doesn't set it back to
            ; this value
            ((org-agenda-category-filter (quote ,category-filter))
             ,@options))))
    `(quote ,forms)))

(defun cp/org-right-align-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/tmp/browse-wrapper.sh")
;
(setq-default company-idle-delay nil)
(with-eval-after-load 'org

  (setq org-publish-project-alist
        '(

          ("org-millennial-basic-blog"
           ;; Path to your org files.
           :base-directory "~/cwills_blog/org/"
           :base-extension "org"

           ;; Path to your Jekyll project.
           :publishing-directory "~/cwills_blog/millennial/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t ;; Only export section between <body> </body>
           :with-toc nil
           )

          ("org-static-millennial-basic-blog"
           :base-directory "~/cwills_blog/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/cwills_blog/millennial/"
           :recursive t
           :publishing-function org-publish-attachment)

          ("millennial" :components ("org-millennial-basic-blog" "org-static-millennial-basic-blog"))

          ))

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
            :empty-lines 1)
          ))
  (setq org-link-abbrev-alist
        '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")
          ("jira" . "https://jira.delacy.com:8443/browse/%s")))
  (setq org-agenda-files '("~/org/"))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿"))
  (defun my-org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-tags-column -90)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-sorting-strategy '(category-keep todo-state-up time-up))

  (defun cw/org-agenda-todo-prefix ()
    (defun helper (category timestamp)
      (format "%-16s"
        (if timestamp
          (format "%-8s:%dd:" category (abs (org-time-stamp-to-now timestamp)))
          (format "%-8s:N/A:" category)
          )))
    (helper
      (org-get-category (point))
      (org-entry-get (point) "TIMESTAMP_IA" t))
    )

  (setq org-agenda-prefix-format
        '((agenda . "%-12:c%?-12t% s")
          (todo . "%-12(cw/org-agenda-todo-prefix)")
          (tags . "%-12:c %s")
          (search . "%-12:c %s")))

  (setq org-agenda-scheduled-leaders '("   Scheduled:" "  Sched %3dx:"))
  (setq org-agenda-deadline-leaders
        '("Deadline due:"
          "     In %3dd:"
          "   %4dd ago:"))

  (setq org-agenda-custom-commands
        `(,(cp/generate-category-agenda-cmds "c" "Captured" ("capture") t 7)

          ,(cp/generate-category-agenda-cmds "h" "House" ("house") t 7)

          ,(cp/generate-category-agenda-cmds "e" "Everything else" ("capture" "house") nil 7)

          ("p" "By person"
            ((tags-todo
              "+{^@.*}"
              ((org-agenda-overriding-header "All TODOs tagged with a person (Any tag starting with @)")
              (org-agenda-sorting-strategy '(tag-up todo-state-up ts-up tsia-up))))))))

  (add-hook 'org-finalize-agenda-hook #'cp/org-right-align-agenda-tags))

  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  (defun cp/jane-switch-config-action (switch)
    (require 'ios-config-mode)
    (let ((buffer (get-buffer-create (format "*%s*" switch)))
          (content (shell-command-to-string (format "jsnet config cat %s" switch))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert content)
        (delete-trailing-whitespace)
        (ios-config-mode)
        (pop-to-buffer buffer)
              (goto-char (point-min)))))

  (defun cp/jane-switch-config-list-switches ()
    (thread-last
        (shell-command-to-string "jadmin dns dump | awk '$2 ~ /-(nsw|nmg|nmx)-/ {print $2}'")
      (s-split "\n")))

  (defun cw/jane-switch-config ()
    (interactive)
    (helm :sources (helm-build-async-source "Switch config"
                    :candidates-process (cp/jane-switch-config-list-switches)
                    :action '(("Switch config" . cp/jane-switch-config-action)))
          :buffer "*Switch config*"))

;;(setq
;; helm-gtags-ignore-case t
;; helm-gtags-auto-update t
;; helm-gtags-use-input-at-cursor t
;; helm-gtags-pulse-at-cursor t
;; helm-gtags-prefix-key "\C-cg"
;; helm-gtags-suggested-key-mapping t
;; )

;;(require 'helm-gtags)
;;;; Enable helm-gtags-mode
;;(add-hook 'dired-mode-hook 'helm-gtags-mode)
;;(add-hook 'eshell-mode-hook 'helm-gtags-mode)
;;(add-hook 'c-mode-hook 'helm-gtags-mode)
;;(add-hook 'c++-mode-hook 'helm-gtags-mode)
;;(add-hook 'asm-mode-hook 'helm-gtags-mode)
;;
;;(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;(define-key helm-gtags-mode-map (kbd "C-c g .") 'helm-gtags-select)
;;(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;;(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;
;;(local-unset-key (kbd "SPC f F"))
;;(define-key evil-normal-state-local-map (kbd "SPC f F") 'counsel-file-jump)
;;(define-key evil-normal-state-local-map (kbd "SPC a g r") 'counsel-rg)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spinner adaptive-wrap ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-gtags spinner adaptive-wrap ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)

