;;; aweshell.el --- Awesome eshell

;; Filename: aweshell.el
;; Description: Awesome eshell
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-13 23:18:35
;; Version: 1.2
;; Last-Updated: 2018-08-14 14:50:37
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/aweshell.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `eshell' `eshell-prompt-extras' `esh-autosuggest' `exec-path-from-shell'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; I created `multi-term.el' and use it many years.
;; Now I'm a big fans of `eshell'.
;;
;; So i write `aweshell.el' to extension `eshell' with below features:
;; 1. Create and manage multiple eshell buffers.
;; 2. Add some useful commands, such as: clear buffer, toggle sudo etc.
;; 3. Display extra information and color like zsh, powered by `eshell-prompt-extras'
;; 4. Add Fish-like history autosuggestions, powered by `esh-autosuggest'
;; 5. Validate and highlight command before post to eshell.
;; 6. Change buffer name by directory change.
;; 7. Add completions for git command.
;; 8. Build-in some handy alias, such as: f (find-file), fo (find-file-other-window), d (dired), ll (list files)
;; 9. Fix error `command not found' in MacOS.
;;

;;; Installation:
;;
;; Put `aweshell.el', `esh-autosuggest.el', `eshell-prompt-extras.el', `exec-path-from-shell.el' to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aweshell)
;;
;; Binding your favorite key to functions:
;;
;; `aweshell-new'
;; `aweshell-next'
;; `aweshell-prev'
;; `aweshell-clear-buffer'
;; `aweshell-sudo-toggle'
;;

;;; Customize:
;;
;; `aweshell-complete-selection-key'
;; `aweshell-clear-buffer-key'
;; `aweshell-sudo-toggle-key'
;; `aweshell-valid-command-color'
;; `aweshell-invalid-command-color'
;;
;; All of the above can customize by:
;;      M-x customize-group RET aweshell RET
;;

;;; Change log:
;;
;; 2018/08/14
;;      * Save buffer in `aweshell-buffer-list', instead save buffer name.
;;      * Change aweshell buffer name by directory change.
;;      * Refacotry code.
;;      * Fix error "wrong-type-argument stringp nil" by `aweshell-validate-command'
;;      * Add some handy aliases.
;;      * Make `aweshell-validate-command' works with eshell aliases.
;;      * Synchronal buffer name with shell path by `epe-fish-path'.
;;      * Use `epe-theme-pipeline' as default theme.
;;      * Complete customize options in docs.
;;      * Redirect `clear' alias to `aweshell-clear-buffer'.
;;      * Add completions for git command.
;;
;; 2018/08/13
;;      * First released.
;;

;;; Acknowledgements:
;;
;; Samray: copy `aweshell-clear-buffer' and `aweshell-sudo-toggle'
;; casouri: copy `aweshell-validate-command' and `aweshell-sync-dir-buffer-name'
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'eshell)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup aweshell nil
  "Multi eshell manager."
  :group 'aweshell)

(defcustom aweshell-complete-selection-key "M-h"
  "The keystroke for complete history auto-suggestions."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-clear-buffer-key "C-l"
  "The keystroke for clear buffer."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-sudo-toggle-key "C-S-l"
  "The keystroke for toggle sudo"
  :type 'string
  :group 'aweshell)

(defcustom aweshell-valid-command-color "#98C379"
  "The color of valid command by `aweshell-validate-command'."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-invalid-command-color "#FF0000"
  "The color of invalid command by `aweshell-validate-command'."
  :type 'string
  :group 'aweshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aweshell-buffer-list nil
  "The list of non-dedicated eshell buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hook ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'kill-buffer-hook 'aweshell-kill-buffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aweshell-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'eshell-mode)
    (let ((killed-buffer (current-buffer)))
      (setq aweshell-buffer-list
            (delq killed-buffer aweshell-buffer-list)))))

(defun aweshell-get-buffer-index ()
  (let ((eshell-buffer-index-list (aweshell-get-buffer-index-list))
        (eshell-buffer-index-counter 1))
    (if eshell-buffer-index-list
        (progn
          (dolist (buffer-index eshell-buffer-index-list)
            (if (equal buffer-index eshell-buffer-index-counter)
                (setq eshell-buffer-index-counter (+ 1 eshell-buffer-index-counter))
              (return eshell-buffer-index-counter)))
          eshell-buffer-index-counter)
      1)))

(defun aweshell-get-buffer-names ()
  (let (eshell-buffer-names)
    (dolist (frame (frame-list))
      (dolist (buffer (buffer-list frame))
        (with-current-buffer buffer
          (if (eq major-mode 'eshell-mode)
              (add-to-list 'eshell-buffer-names (buffer-name buffer))))))
    eshell-buffer-names))

(defun aweshell-get-buffer-index-list ()
  (let ((eshell-buffer-names (aweshell-get-buffer-names)))
    (if eshell-buffer-names
        (let* ((eshell-buffer-index-strings
                (seq-filter (function
                             (lambda (buffer-index)
                               (and (stringp buffer-index)
                                    (not (equal 0 (string-to-number buffer-index))))))
                            (mapcar (function
                                     (lambda (buffer-name)
                                       (if (integerp (string-match "\\*eshell\\*\<\\([0-9]+\\)\>" buffer-name))
                                           (subseq buffer-name (match-beginning 1) (match-end 1))
                                         nil)))
                                    eshell-buffer-names)))
               (eshell-buffer-index-list (sort (seq-map 'string-to-number eshell-buffer-index-strings) '<)))
          eshell-buffer-index-list)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aweshell-new ()
  "Create new eshell buffer."
  (interactive)
  (setq aweshell-buffer-list (nconc aweshell-buffer-list (list (eshell (aweshell-get-buffer-index))))))

(defun aweshell-next ()
  "Select next eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not aweshell-buffer-list) (equal (length aweshell-buffer-list) 0))
      (aweshell-new)
    (let* ((current-buffer-index (cl-position (current-buffer) aweshell-buffer-list))
           (switch-index (if current-buffer-index
                             (if (>= current-buffer-index (- (length aweshell-buffer-list) 1))
                                 0
                               (+ 1 current-buffer-index))
                           0)))
      (switch-to-buffer (nth switch-index aweshell-buffer-list))
      )))

(defun aweshell-prev ()
  "Select previous eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not aweshell-buffer-list) (equal (length aweshell-buffer-list) 0))
      (aweshell-new)
    (let* ((current-buffer-index (cl-position (current-buffer) aweshell-buffer-list))
           (switch-index (if current-buffer-index
                             (if (<= current-buffer-index 0)
                                 (- (length aweshell-buffer-list) 1)
                               (- current-buffer-index 1))
                           (- (length aweshell-buffer-list) 1))))
      (switch-to-buffer (nth switch-index aweshell-buffer-list))
      )))

(defun aweshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun aweshell-sudo-toggle ()
  "Toggle sudo with current command."
  (interactive)
  (save-excursion
    (let ((commands (buffer-substring-no-properties
                     (eshell-bol) (point-max))))
      (if (string-match-p "^sudo " commands)
          (progn
            (eshell-bol)
            (while (re-search-forward "sudo " nil t)
              (replace-match "" t nil)))
        (progn
          (eshell-bol)
          (insert "sudo ")
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aweshell keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd aweshell-clear-buffer-key) 'aweshell-clear-buffer)
            (define-key eshell-mode-map (kbd aweshell-sudo-toggle-key) 'aweshell-sudo-toggle)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aweshell face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 '(epe-dir-face ((t (:foreground "green3"))))
 '(epe-git-face ((t (:foreground "systemRedColor"))))
 '(epe-pipeline-delimiter-face ((t (:foreground "green4"))))
 '(epe-pipeline-host-face ((t (:foreground "systemGreenColor"))))
 '(epe-pipeline-time-face ((t (:foreground "systemGrayColor"))))
 '(epe-pipeline-user-face ((t (:foreground "gold"))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Handy aliases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "f" "find-file $1")
            (eshell/alias "fo" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (eshell/alias "clear" "aweshell-clear-buffer")
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                        "/bin/ls")))
              (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EShell extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eshell-prompt-extras
;; Display extra information and color for your eshell prompt.
(require 'eshell-prompt-extras)
(with-eval-after-load "esh-opt"
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-pipeline))

;; esh-autosuggest
;; Fish-like history autosuggestions in eshell
(require 'esh-autosuggest)
(add-hook 'eshell-mode-hook
          (lambda ()
            (esh-autosuggest-mode)
            (define-key eshell-mode-map (kbd aweshell-complete-selection-key) 'company-complete-selection)))

;; Validate command before post to eshell.
(defun aweshell-validate-command ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward (format "%s\\([^ ]*\\)" eshell-prompt-regexp)
                       (line-end-position)
                       t)
    (let ((beg (match-beginning 1))
          (end (match-end 1))
          (command (match-string 1)))
      (when command
        (put-text-property
         beg end
         'face `(:foreground
                 ,(if
                      (or
                       ;; Command is exists?
                       (executable-find command)
                       ;; Or command is alias?
                       (seq-contains (eshell-alias-completions "") command))
                      aweshell-valid-command-color
                    aweshell-invalid-command-color)))))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-hook 'post-command-hook #'aweshell-validate-command t t)))


;; Synchronal buffer name by directory change.
(defun aweshell-sync-dir-buffer-name ()
  "Change aweshell buffer name by directory change."
  (when (equal major-mode 'eshell-mode)
    (rename-buffer (format "Aweshell: %s" (epe-fish-path default-directory))
                   t)))

(add-hook 'eshell-directory-change-hook #'aweshell-sync-dir-buffer-name)
(add-hook 'eshell-mode-hook #'aweshell-sync-dir-buffer-name)

;; Add completions for git command.
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
              "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
              nil t)
        (push (match-string 1) commands)
        (when (match-string 2)
          (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                             (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

(provide 'aweshell)

;;; aweshell.el ends here
