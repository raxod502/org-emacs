(unless (gnutls-available-p)
  (error "Emacs wasn't installed with networking support!"))

;; I may be biased here but I prefer to use the package manager I
;; wrote (called straight.el) rather than the one that comes with
;; Emacs (called package.el). This code sets it up; it's from
;; https://github.com/raxod502/straight.el#getting-started.

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Because I have not had enough time to fix all the bugs, there are
;; still some problems with straight.el. One of them causes Org to
;; print a warning, unless we use the following code to work around
;; the bug. This code is from
;; https://github.com/raxod502/straight.el#installing-org-with-straightel.

(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; The main feature of this Emacs configuration is Org, so naturally
;; we want to install the `org' package, which we can do using
;; straight.el.

(straight-use-package 'org)

;; This is the file you would like to use for your main todo list.
;; Customize to taste. (This is not a special Org variable, but rather
;; just a variable we are defining for future use.)

(setq my-todo-file "~/todo.org")

;; We will also want to set up some keybindings. While it is not hard
;; to do this right out of the box, there is a nice library called
;; `bind-key' which makes it easier.

(straight-use-package 'bind-key)

;; We make a convenient function for accessing your todo list.

(defun my-goto-todo-file ()
  "Edit `my-todo-file' in the current window."
  (interactive)
  (find-file my-todo-file))

;; You can call the `my-goto-todo-file' function with M-x
;; my-goto-todo-file. But it is faster to use C-c t, so let's do that
;; instead.

(bind-key "C-c t" 'my-goto-todo-file)

;; You can get to the agenda view in Org by typing M-x org-agenda.
;; However, since we want to view the agenda more easily, we also bind
;; that command to the sequence C-c a.

(bind-key "C-c a" 'org-agenda)

;; Similarly, we'd like to capture a TODO item by pressing just C-c c
;; rather than M-x org-capture.

(bind-key "C-c c" 'org-capture)

;; We want Org files to be shown as an indented tree, rather than just
;; as a flat file. This enables that functionality by default; you can
;; toggle it temporarily by running M-x org-indent-mode.
;;
;; To explain the following code, Emacs allows for editing different
;; kinds of files by using a different "major mode" for each. Org
;; files are edited using `org-mode', for example. When you edit an
;; Org file, that causes `org-mode-hook' to be run. Thus, by adding
;; functions to this hook, you can customize what happens when editing
;; Org files. In this case, we tell Emacs to run the function
;; `org-indent-mode', which activates the "minor mode" by that name.
;; Minor modes are like additional filters that can be enabled within
;; a particular major mode, which customize various behaviors. By
;; activating `org-indent-mode', we tell Org to show us an indented
;; view rather than a flat one.

(add-hook 'org-mode-hook 'org-indent-mode)

;; Tell the agenda view to display your todo list. (The
;; `org-agenda-files' variable contains a list of files from which
;; TODO entries are taken to display.)

(setq org-agenda-files (list my-todo-file))

;; Show the week starting at the current day, not starting at Monday.

(setq org-agenda-start-on-weekday nil)

;; Hide scheduled items on the agenda view until the day they are
;; scheduled.

(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; This is some neat code that allows you to capture a TODO item to
;; any heading in the Org file which is tagged with :refile:. If you
;; to add additional capture targets, you can customize the setting of
;; `org-capture-templates' below.

(setq org-refile-targets
      `((,my-todo-file . (:tag . "refile"))))

(defun my-org-make-capture-template ()
  (format "* %s %%?"
          (completing-read
           "Item type: "
           (with-current-buffer (find-file-noselect my-todo-file)
             org-todo-keywords-1))))

(setq org-capture-templates
      `(("c" "Capture" entry
         (file+function
          ,my-todo-file
          (lambda ()
            (goto-char
             (nth 3 (org-refile-get-location "Capture to")))))
         (function my-org-make-capture-template)
         :unnarrowed t)))

;; If you close Emacs while a clock is running, then the next time you
;; open an Org file, ask if the clock should be resumed (instead of
;; forgetting about it).

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Magit, the magical version-control frontend.

(straight-use-package 'magit)

;; Convenient keybinding for accessing Magit.

(bind-key "C-x g" 'magit-status)

;; A nice color theme. You can change this to a different color theme
;; if you don't like it. To test out another color theme, first run
;; M-x disable-theme to turn off this one, and then run M-x load-theme
;; to turn on a different one. Emacs comes with some color themes, but
;; you can install more by finding them online and then installing
;; then using M-x straight-use-package.

(straight-use-package 'zerodark-theme)
(load-theme 'zerodark 'no-confirm)

;; Make it easier to select from a list of options, for example when
;; you're selecting a file to edit or a heading under which to capture
;; a TODO item.

(straight-use-package 'ivy)
(ivy-mode +1)

(straight-use-package 'counsel)
(counsel-mode +1)

(straight-use-package 'ivy-prescient)
(ivy-prescient-mode +1)
