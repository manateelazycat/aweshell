;;; aweshell.el --- Awesome eshell

;; Filename: aweshell.el
;; Description: Awesome eshell
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-13 23:18:35
;; Version: 4.5
;; Last-Updated: 2020-03-28 10:03:43
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/aweshell.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `eshell' `eshell-prompt-extras' `exec-path-from-shell' `cl-lib' `subr-x'
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
;; 4. Add Fish-like history autosuggestions.
;; 5. Validate and highlight command before post to eshell.
;; 6. Change buffer name by directory change.
;; 7. Add completions for git command.
;; 8. Fix error `command not found' in MacOS.
;; 9. Integrate `eshell-up'.
;; 10. Unpack archive file.
;; 11. Open file with alias e.
;; 12. Output "did you mean ..." helper when you typo.
;; 13. Make cat file with syntax highlight.
;; 14. Alert user when background process finished or aborted.
;; 15. Complete shell command arguments like IDE feeling.
;; 16. Dedicated shell window like IDE bottom terminal window.
;;

;;; Installation:
;;
;; Put `aweshell.el', `eshell-prompt-extras.el', `exec-path-from-shell.el' to your load-path.
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
;; `aweshell-switch-buffer'
;; `aweshell-dedicated-toggle'
;; `aweshell-dedicated-open'
;; `aweshell-dedicated-close'
;;

;;; Customize:
;;
;; `aweshell-complete-selection-key'
;; `aweshell-clear-buffer-key'
;; `aweshell-sudo-toggle-key'
;; `aweshell-search-history-key'
;;
;; `aweshell-valid-command-color'
;; `aweshell-invalid-command-color'
;; `aweshell-use-exec-path-from-shell'
;; `aweshell-dedicated-window-height'
;;
;; All of the above can customize by:
;;      M-x customize-group RET aweshell RET
;;

;;; Change log:
;;;
;;
;; 2020/03/28
;;      * Hide hl-line in shell.
;;
;; 2020/01/14
;;      * Fix issue #49
;;
;; 2019/09/12
;;      * Use `cl-lib' instead of `cl'
;;
;; 2019/07/17
;;      * Fix #37 issue: aweshell-dedicated-toggle failed after user use cd command in aweshell.
;;
;; 2019/07/16
;;      * Add `aweshell-dedicated-toggle' command.
;;      * Refactory code.
;;
;; 2019/04/29
;;      * Add interactive command `aweshell-switch-buffer'.
;;
;; 2019/04/07
;;      * Increased startup speed, loaded `eshell-did-you-mean' plugin when idle
;;
;; 2019/1/2
;;      * When the command includes * or \ , the `pcomplete-completions' command will report an error,
;;        So company menu will be disabled when these characters are included.
;;
;; 2018/12/28
;;      * When the command includes " or [ , the `pcomplete-completions' command will report an error,
;;        So company menu will be disabled when these characters are included.
;;
;; 2018/12/27
;;      * Fix backtrace when type command: git clone "
;;
;; 2018/12/22
;;      * Mix best history and complete arguments just when history not exist in completion arguments.
;;
;; 2018/12/15
;;      * Mix best match history and shell completions in company's completion menu.
;;      * Fix error when completion.
;;
;; 2018/12/14
;;      * Add new option `aweshell-autosuggest-backend' to swtich between fish-style and company-style.
;;
;; 2018/11/12
;;      * Remove Mac color, use hex color instead.
;;
;; 2018/10/19
;;      * Alert user when background process finished or aborted.
;;
;; 2018/09/19
;;      * Make `exec-path-from-shell' optional. Disable with variable`aweshell-use-exec-path-from-shell'.
;;
;; 2018/09/17
;;      * Use `ido-completing-read' instead `completing-read' to provide fuzz match.
;;
;; 2018/09/10
;;      * Built-in `eshell-did-you-mean' plugin.
;;
;; 2018/09/07
;;      * Add docs about `eshell-up', `aweshell-emacs' and `aweshell-unpack'
;;      * Add `aweshell-cat-with-syntax-highlight' make cat file with syntax highlight.
;;
;; 2018/09/06
;;      * Require `cl' to fix function `subseq' definition.
;;
;; 2018/08/16
;;      * Just run git relative code when git in exec-path.
;;      * Use `esh-parse-shell-history' refacotry code.
;;      * Try to fix error "Shell command failed with code 1 and no output" cause by LANG environment variable.
;;
;; 2018/08/15
;;      * Remove face settings.
;;      * Add `aweshell-search-history' and merge bash/zsh history in `esh-autosuggest' .
;;      * Fix history docs.
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
;;      * Adjust `ls' alias.
;;
;; 2018/08/13
;;      * First released.
;;

;;; Acknowledgements:
;;
;; Samray: copy `aweshell-clear-buffer', `aweshell-sudo-toggle' and `aweshell-search-history'
;; casouri: copy `aweshell-validate-command' and `aweshell-sync-dir-buffer-name'
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'eshell)
(require 'cl-lib)
(require 'subr-x)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aweshell-use-exec-path-from-shell t)

(when (and aweshell-use-exec-path-from-shell
           (featurep 'cocoa))
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

(defcustom aweshell-search-history-key "M-'"
  "The keystroke for search history"
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

(defface aweshell-alert-buffer-face
  '((t (:foreground "#ff2d55" :bold t)))
  "Alert buffer face."
  :group 'aweshell)

(defface aweshell-alert-command-face
  '((t (:foreground "#ff9500" :bold t)))
  "Alert command face."
  :group 'aweshell)

(defcustom aweshell-dedicated-window-height 14
  "The height of `aweshell' dedicated window."
  :type 'integer
  :group 'aweshell)

(defcustom aweshell-auto-suggestion-p t
  "Whether auto suggestion like fish.
Default is enable, but sometimes, this feature will insert random indent char.

If this function affects you, disable this option."
  :type 'boolean
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

(defun aweshell-toggle (&optional arg)
  "Toggle Aweshell.
If called with prefix argument, open Aweshell buffer in current directory when toggling on Aweshell. If there exists an Aweshell buffer with current directory, use that, otherwise create one."
  (interactive "p")
  (if (equal major-mode 'eshell-mode)
      ;; toggle off
      (while (equal major-mode 'eshell-mode)
        (switch-to-prev-buffer))
    ;; toggle on
    (if (eq arg 4)
        ;; open in current dir
        (let* ((dir default-directory)
               (existing-buffer
                (catch 'found
                  (dolist (aweshell-buffer aweshell-buffer-list)
                    (with-current-buffer aweshell-buffer
                      (when (equal dir default-directory)
                        (throw 'found aweshell-buffer)))))))
          ;; found the buffer with the same dir
          ;; or create a new one
          (if existing-buffer
              (switch-to-buffer existing-buffer)
            (message "No Aweshell buffer with current dir found, creating a new one.")
            (switch-to-buffer (car (last (aweshell-new))))
            (eshell/cd dir)))
      ;; simply open
      (aweshell-next))))

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
                     (progn (eshell-bol) (point)) (point-max))))
      (if (string-match-p "^sudo " commands)
          (progn
            (eshell-bol)
            (while (re-search-forward "sudo " nil t)
              (replace-match "" t nil)))
        (progn
          (eshell-bol)
          (insert "sudo ")
          )))))

(defun aweshell-search-history ()
  "Interactive search eshell history."
  (interactive)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
           (input (eshell-get-old-input))
           (all-shell-history (aweshell-parse-shell-history)))
      (let* ((command (ido-completing-read "Search history: " all-shell-history)))
        (eshell-kill-input)
        (insert command)
        )))
  ;; move cursor to eol
  (end-of-line))

(defun aweshell-switch-buffer ()
  "Switch to another aweshell buffer."
  (interactive)
  (cond ((= 0 (length aweshell-buffer-list))
         (aweshell-new)
         (message "No Aweshell buffer yet, create a new one."))
        ((= 1 (length aweshell-buffer-list)) ; only one Aweshell buffer, just switch to it
         (switch-to-buffer (nth 0 aweshell-buffer-list)))
        (t
         (let* ((completion-extra-properties '(:annotation-function aweshell-switch-buffer--annotate))
                (buffer-alist (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer)) aweshell-buffer-list))
                (pwd default-directory)
                (preselect))
           ;; find most suitable preselect buffer
           (dolist (buffer aweshell-buffer-list)
             (with-current-buffer buffer
               (when (and
                      (or (not preselect) (< (length preselect) (length default-directory)))
                      (file-in-directory-p pwd default-directory))
                 (setq preselect (propertize default-directory :buffer-name (buffer-name buffer))))))
           (let ((result-buffer (completing-read "Switch to Aweshell buffer: " buffer-alist nil t nil nil
                                                 (get-text-property 0 :buffer-name (or preselect "")))))
             (switch-to-buffer (alist-get result-buffer buffer-alist nil nil #'equal)))))))

(defun aweshell-switch-buffer--annotate (candidate)
  (let* ((buffer-alist
          (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer)) aweshell-buffer-list))
         (candidate-buffer (alist-get candidate buffer-alist nil nil #'equal)))
    (with-current-buffer candidate-buffer
      ;; display the last command of aweshell buffer
      (format "  <%s> %s" (eshell-get-history 0) (if eshell-current-command "(Running)" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aweshell dedicated window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aweshell-dedicated-window nil
  "The dedicated `aweshell' window.")

(defvar aweshell-dedicated-buffer nil
  "The dedicated `aweshell' buffer.")

(defun aweshell-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun aweshell-dedicated-exist-p ()
  (and (aweshell-buffer-exist-p aweshell-dedicated-buffer)
       (aweshell-window-exist-p aweshell-dedicated-window)
       ))

(defun aweshell-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun aweshell-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun aweshell-dedicated-open ()
  "Open dedicated `aweshell' window."
  (interactive)
  (if (aweshell-buffer-exist-p aweshell-dedicated-buffer)
      (if (aweshell-window-exist-p aweshell-dedicated-window)
          (aweshell-dedicated-select-window)
        (aweshell-dedicated-pop-window))
    (aweshell-dedicated-create-window)))

(defun aweshell-dedicated-close ()
  "Close dedicated `aweshell' window."
  (interactive)
  (if (aweshell-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (aweshell-dedicated-select-window)
        (delete-window aweshell-dedicated-window)
        (if (aweshell-window-exist-p current-window)
            (select-window current-window)))
    (message "`AWESHELL DEDICATED' window is not exist.")))

(defun aweshell-dedicated-toggle ()
  "Toggle dedicated `aweshell' window."
  (interactive)
  (if (aweshell-dedicated-exist-p)
      (aweshell-dedicated-close)
    (aweshell-dedicated-open)))

(defun aweshell-dedicated-select-window ()
  "Select aweshell dedicated window."
  (select-window aweshell-dedicated-window)
  (set-window-dedicated-p (selected-window) t))

(defun aweshell-dedicated-pop-window ()
  "Pop aweshell dedicated window if it exists."
  (setq aweshell-dedicated-window (display-buffer (car (aweshell-get-buffer-names)) `(display-buffer-in-side-window (side . bottom) (window-height . ,aweshell-dedicated-window-height))))
  (select-window aweshell-dedicated-window)
  (set-window-buffer aweshell-dedicated-window aweshell-dedicated-buffer)
  (set-window-dedicated-p (selected-window) t))

(defun aweshell-dedicated-create-window ()
  "Create aweshell dedicated window if it not existing."
  (eshell)
  (setq aweshell-dedicated-buffer (current-buffer))
  (previous-buffer)
  (aweshell-dedicated-pop-window))

(defun aweshell-dedicated-split-window ()
  "Split dedicated window at bottom of frame."
  ;; Select bottom window of frame.
  (ignore-errors
    (dotimes (i 50)
      (windmove-down)))
  ;; Split with dedicated window height.
  (split-window (selected-window) (- (aweshell-current-window-take-height) aweshell-dedicated-window-height))
  (other-window 1)
  (setq aweshell-dedicated-window (selected-window)))

(defun aweshell-dedicated-create-buffer ()
  "Create aweshell dedicated buffer."
  (eshell)
  (setq header-line-format nil)
  (setq aweshell-dedicated-buffer (current-buffer)))

(defadvice delete-other-windows (around aweshell-delete-other-window-advice activate)
  "This is advice to make `aweshell' avoid dedicated window deleted.
Dedicated window can't deleted by command `delete-other-windows'."
  (unless (eq (selected-window) aweshell-dedicated-window)
    (let ((aweshell-dedicated-active-p (aweshell-window-exist-p aweshell-dedicated-window)))
      (if aweshell-dedicated-active-p
          (let ((current-window (selected-window)))
            (cl-dolist (win (window-list))
              (when (and (window-live-p win)
                         (not (eq current-window win))
                         (not (window-dedicated-p win)))
                (delete-window win))))
        ad-do-it))))

(defadvice other-window (after aweshell-dedicated-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want to select `sr-speedbar' window,
but use `other-window' and just make `aweshell' dedicated
window as a viewable sidebar.

This advice can make `other-window' skip `aweshell' dedicated window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (aweshell-window-exist-p aweshell-dedicated-window)
               (eq aweshell-dedicated-window (selected-window)))
      (other-window count))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (face-remap-add-relative 'hl-line :background (face-background 'default))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aweshell keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd aweshell-clear-buffer-key) 'aweshell-clear-buffer)
            (define-key eshell-mode-map (kbd aweshell-sudo-toggle-key) 'aweshell-sudo-toggle)
            (define-key eshell-mode-map (kbd aweshell-search-history-key) 'aweshell-search-history)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EShell extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eshell-up.el
;; Quickly go to a specific parent directory in eshell
(require 'eshell-up)
(defalias 'eshell/up 'eshell-up)
(defalias 'eshell/up-peek 'eshell-up-peek)

;; eshell-prompt-extras
;; Display extra information and color for your eshell prompt.
(require 'eshell-prompt-extras)
(with-eval-after-load "esh-opt"
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-pipeline))

;; Validate command before post to eshell.
(defun aweshell-validate-command ()
  (save-excursion
    (let (end (line-end-position))
      (forward-line 0)
      (re-search-forward (format "%s\\([^ \t\r\n\v\f]*\\)" eshell-prompt-regexp)
                         end
                         t))
    (let ((beg (match-beginning 1))
          (end (match-end 1))
          (command (match-string 1)))
      (when command
        (put-text-property
         beg end
         'face `(:foreground
                 ,(if
                      (or
                       ;; Command exists?
                       (executable-find command)
                       ;; Or command is an alias?
                       (seq-contains (eshell-alias-completions "") command)
                       ;; Or it is ../. ?
                       (or (equal command "..")
                           (equal command ".")
                           (equal command "exit"))
                       ;; Or it is a file in current dir?
                       (member (file-name-base command) (directory-files default-directory))
                       ;; Or it is a elisp function
                       (functionp (intern command)))
                      aweshell-valid-command-color
                    aweshell-invalid-command-color)))
        (put-text-property beg end 'rear-nonsticky t)))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-hook 'post-command-hook #'aweshell-validate-command t t)))

(defun aweshell-emacs (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'eshell/e 'aweshell-emacs)

(defun aweshell-unpack (file &rest args)
  "Unpack FILE with ARGS."
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))

(defalias 'eshell/unpack 'aweshell-unpack)

;; Synchronal buffer name by directory change.
(defun aweshell-sync-dir-buffer-name ()
  "Change aweshell buffer name by directory change."
  (when (equal major-mode 'eshell-mode)
    (rename-buffer (format "Aweshell: %s" (epe-fish-path default-directory))
                   t)))

(add-hook 'eshell-directory-change-hook #'aweshell-sync-dir-buffer-name)
(add-hook 'eshell-mode-hook #'aweshell-sync-dir-buffer-name)

;; Add completions for git command.
(when (executable-find "git")
  (defun pcmpl-git-commands ()
    "Return the most common git commands by parsing the git output."
    (with-temp-buffer
      (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
      (goto-char 0)
      (search-forward "\n\n")
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
  )

;; eshell-did-you-mean
;; command not found (“did you mean…” feature) in Eshell.
(add-hook 'eshell-mode-hook
          (lambda ()
            (run-with-idle-timer
             1 nil
             #'(lambda ()
                 (require 'eshell-did-you-mean)
                 (eshell-did-you-mean-setup)
                 ))))

;; Make cat with syntax highlight.
(defun aweshell-cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)

;; Alert user when background process finished or aborted.
(defun eshell-command-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let* ((cmd (process-command process))
         (buffer (process-buffer process))
         (msg (replace-regexp-in-string "\n" " " (string-trim (format "%s: %s" (mapconcat 'identity cmd " ")  status))))
         (buffer-visible (member buffer (mapcar #'window-buffer (window-list)))))
    (unless buffer-visible
      (message "%s %s"
               (propertize (format "[Aweshell Alert] %s" (string-remove-prefix "Aweshell: " (buffer-name buffer))) 'face 'aweshell-alert-buffer-face)
               (propertize msg 'face 'aweshell-alert-command-face)))))

(add-hook 'eshell-kill-hook #'eshell-command-alert)

;; Auto suggestion.
;; First completion candidate is best match that pick from shell history.
;; Rest is completion arguments from shell completion.
(when aweshell-auto-suggestion-p
  (defun aweshell-reload-shell-history ()
    (with-temp-message ""
      (cond ((string-equal shell-file-name "/bin/bash")
             (shell-command "history -r"))
            ((string-equal shell-file-name "/bin/zsh")
             (shell-command "fc -W; fc -R")))))

  (defun aweshell-parse-bash-history ()
    "Parse the bash history."
    (if (file-exists-p "~/.bash_history")
        (let (collection bash_history)
          (aweshell-reload-shell-history)
          (setq collection
                (nreverse
                 (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                                 (buffer-string))
                               "\n"
                               t)))
          (when (and collection (> (length collection) 0)
                     (setq bash_history collection))
            bash_history))
      nil))

  (defun aweshell-parse-zsh-history ()
    "Parse the bash history."
    (if (file-exists-p "~/.zsh_history")
        (let (collection zsh_history)
          (aweshell-reload-shell-history)
          (setq collection
                (nreverse
                 (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                                 (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
                               "\n"
                               t)))
          (when (and collection (> (length collection) 0)
                     (setq zsh_history collection))
            zsh_history))
      nil))

  (defun aweshell-parse-shell-history ()
    "Parse history from eshell/bash/zsh/ ."
    (delete-dups
     (mapcar
      (lambda (str)
        (string-trim (substring-no-properties str)))
      (append
       (ring-elements eshell-history-ring)
       (aweshell-parse-bash-history)
       (aweshell-parse-zsh-history)))))

  (defun aweshell-autosuggest--prefix ()
    "Get current eshell input.

If this function return non-nil prefix, aweshell will popup completion menu in aweshell buffer.
This function only return prefix when current point at eshell prompt line, avoid insert unnecessary indent char, such as ghci prompt. (See issue #49)."
    (when (save-excursion
            (beginning-of-line)
            (looking-at-p eshell-prompt-regexp))
      (string-trim-left
       (buffer-substring-no-properties
        (save-excursion
          (eshell-bol))
        (line-end-position)))))

  (defun aweshell-autosuggest-candidates (prefix)
    "Select the first eshell history candidate and shell completions that starts with PREFIX."
    (unless (or
             ;; When the command includes ", *, \ or [ characters, the `pcomplete-completions' command will report an error,
             ;; So company menu will be disabled when these characters are included.
             (cl-search "\"" prefix)
             (cl-search "[" prefix)
             (cl-search "\*" prefix)
             (cl-search "\\" prefix))
      (let* ((most-similar (cl-find-if
                            (lambda (str)
                              (string-prefix-p prefix str))
                            (aweshell-parse-shell-history)))
             (command-prefix-args (mapconcat 'identity (nbutlast (split-string prefix)) " "))
             (command-last-arg (car (last (split-string prefix))))
             (completions (ignore-errors (pcomplete-completions)))
             (shell-completions (if (cl-typep completions 'cons)
                                    (cl-remove-if-not (lambda (c) (string-prefix-p command-last-arg c)) completions)
                                  nil))
             (suggest-completions (mapcar (lambda (c) (string-trim (concat command-prefix-args " " c))) shell-completions)))
        ;; Mix best history and complete arguments just when history not exist in completion arguments.
        (if (and most-similar
                 (not (member most-similar suggest-completions)))
            (append (list most-similar) suggest-completions)
          suggest-completions)
        )))

  (defun aweshell-autosuggest (command &optional arg &rest ignored)
    "`company-mode' backend to provide eshell history suggestion."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'aweshell-autosuggest))
      (prefix (and (derived-mode-p 'eshell-mode)
                   (aweshell-autosuggest--prefix)))
      (candidates (aweshell-autosuggest-candidates arg))
      (sorted nil)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (company-mode 1)
              (setq-local company-idle-delay 0)
              (setq-local company-backends '(aweshell-autosuggest))
              ))
  )

(provide 'aweshell)

;;; aweshell.el ends here
