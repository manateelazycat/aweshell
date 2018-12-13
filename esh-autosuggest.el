;;; esh-autosuggest.el --- History autosuggestions for eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/esh-autosuggest
;; Git-Repository: git://github.com/dieggsy/esh-autosuggest.git
;; Created: 2017-10-28
;; Version: 1.2.2
;; Keywords: completion company matching convenience abbrev
;; Package-Requires: ((emacs "24.4") (company "0.9.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a company backend that implements functionality similar to fish
;; shell history autosuggestions.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup esh-autosuggest nil
  "Fish-like autosuggestions for eshell."
  :group 'company)

(defcustom esh-autosuggest-delay 0
  "Delay for history autosuggestion."
  :group 'esh-autosuggest
  :type 'number)

(defcustom esh-autosuggest-use-company-map nil
  "Instead of overriding `company-active-map', use as-is.

This is disabled by default, as bindings in `company-active-map'
to RET and TAB may interfere with command input and completion
respectively."
  :group 'esh-autosuggest
  :type 'boolean)

(defvar esh-autosuggest-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<right>") 'company-complete-selection)
    (define-key keymap (kbd "C-f") 'company-complete-selection)
    (define-key keymap (kbd "M-<right>") 'esh-autosuggest-complete-word)
    (define-key keymap (kbd "M-f") 'esh-autosuggest-complete-word)
    keymap)
  "Keymap that is enabled during an active history
  autosuggestion.")

(defun esh-reload-shell-history ()
  (with-temp-message ""
    (let* ((shell-command (getenv "SHELL")))
      (cond ((string-equal shell-command "/bin/bash")
             (shell-command "history -r"))
            ((string-equal shell-command "/bin/zsh")
             (shell-command "fc -W; fc -R"))))))

(defun esh-parse-bash-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.bash_history")
      (let (collection bash_history)
        (esh-reload-shell-history)
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

(defun esh-parse-zsh-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.zsh_history")
      (let (collection zsh_history)
        (esh-reload-shell-history)
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

(defun esh-parse-shell-history ()
  "Parse history from eshell/bash/zsh/ ."
  (delete-dups
   (mapcar
    (lambda (str)
      (string-trim (substring-no-properties str)))
    (append
     (ring-elements eshell-history-ring)
     (esh-parse-bash-history)
     (esh-parse-zsh-history)))))

(defun esh-autosuggest-candidates (prefix)
  "Select the first eshell history candidate that starts with PREFIX."
  (let* ((most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        (esh-parse-shell-history))))
    (when most-similar
      `(,most-similar))))

(defun esh-autosuggest-complete-word ()
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (company-complete-selection)
      (goto-char pos)
      (forward-word)
      (unless (or (eobp) (eolp))
        (kill-line))))
  (end-of-line)
  (ignore-errors
    (let ((inhibit-message t))
      (company-begin-backend 'esh-autosuggest))))

(defun esh-autosuggest--prefix ()
  "Get current eshell input."
  (let* ((input-start (progn
                        (save-excursion
                          (beginning-of-line)
                          (while (not (looking-at-p eshell-prompt-regexp))
                            (forward-line -1))
                          (eshell-bol))))
         (prefix
          (string-trim-left
           (buffer-substring-no-properties
            input-start
            (line-end-position)))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

;;;###autoload
(defun esh-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide eshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esh-autosuggest))
    (prefix (and (eq major-mode 'eshell-mode)
                 (esh-autosuggest--prefix)))
    (candidates (esh-autosuggest-candidates arg))))

;;;; Companyless

(defvar esh-autosuggest--companyless-overlay nil
  "Overlay used to display auto suggestion")

(defface esh-autosuggest-companyless '((((background dark)) . (:foreground "gray50"))
                                       (((background light)) . (:foreground "silver")))
  "Face of auto suggestion when using companyless mode.")

(defvar esh-autosuggest--companyless-override-map (let ((map (make-sparse-keymap)))
                                                    (define-key map (kbd "C-f") #'esh-autosuggest--companyless-complete)
                                                    map)
  "The map used on overlay so you can complete with C-f.")

(defun esh-autosuggest--companyless-post-command-hook ()
  "Add autosuggest to overlay."
  (when (and eshell-mode
             (not (minibufferp)))
    ;; remove overlay and later (if needed) create a new one
    (when esh-autosuggest--companyless-overlay
      (delete-overlay esh-autosuggest--companyless-overlay)
      (setq esh-autosuggest--companyless-overlay nil))
    (define-key esh-autosuggest-companyless-mode-map
      (kbd "C-f") nil)
    (let ((prefix (if (or (eq (char-after) nil) ; only complete when at end of symbol
                          (eq (char-after) ?\n))
                      (esh-autosuggest--prefix)
                    'stop))
          suggest)
      (when (and (not (eq prefix 'stop))
                 (setq suggest (car (esh-autosuggest-candidates prefix))))
        (setq esh-autosuggest--companyless-overlay
              (make-overlay (point) (point)))
        (define-key esh-autosuggest-companyless-mode-map
          (kbd "C-f") #'esh-autosuggest--companyless-complete)
        (overlay-put
         esh-autosuggest--companyless-overlay
         'after-string ; use after sting to display suggestion
         (propertize (substring suggest (length prefix)) ; remove prefix from suggestion
                     ;; without 'cursor property, the cursor is displayed at the end of
                     ;; the overlay
                     'cursor 0 'face 'esh-autosuggest-companyless))))))

(defun esh-autosuggest--companyless-complete ()
  "Insert the auto suggestion."
  (interactive)
  (when (and eshell-mode
             esh-autosuggest--companyless-overlay)
    (insert (substring-no-properties
             (or (overlay-get esh-autosuggest--companyless-overlay 'after-string)
                 "")))))

(define-minor-mode esh-autosuggest-companyless-mode
  "`esh-autosuggest-mode' but don't use company as front end."
  :keymap (make-sparse-keymap)
  :group 'esh-autosuggest
  (when esh-autosuggest-mode
    (esh-autosuggest-mode -1))
  (if esh-autosuggest-companyless-mode
      (add-hook 'post-command-hook #'esh-autosuggest--companyless-post-command-hook t t)
    (remove-hook 'post-command-hook #'esh-autosuggest--companyless-post-command-hook t)
    ;; clean up overlay
    (when esh-autosuggest--companyless-overlay
      (delete-overlay esh-autosuggest--companyless-overlay)
      (define-key esh-autosuggest-companyless-mode-map
        (kbd "C-f") nil))))

(define-minor-mode esh-autosuggest-mode
  "Enable fish-like autosuggestions in eshell.

You can use <right> to select the suggestion. This is
customizable through `esh-autosuggest-active-map'. If
you prefer to use the default value of `company-active-map', you
may set the variable
`esh-autosuggest-use-company-map', though this isn't
recommended as RET and TAB may not work as expected (send input,
trigger completions, respectively) when there is an active
suggestion.

The delay defaults to 0 seconds to emulate fish shell's
instantaneous suggestions, but is customizable with
`esh-autosuggest-delay'.

Note: This assumes you want to use something other than company
for shell completion, e.g. `eshell-pcomplete',
`completion-at-point', or helm-esh-pcomplete, since
`company-active-map', `company-backends', and `company-frontends'
will be locally overriden and company will be used solely for
history autosuggestions."
  :init-value nil
  :group 'esh-autosuggest
  (if esh-autosuggest-mode
      (progn
        (company-mode 1)
        (unless esh-autosuggest-use-company-map
          (setq-local company-active-map esh-autosuggest-active-map))
        (setq-local company-idle-delay esh-autosuggest-delay)
        (setq-local company-backends '(esh-autosuggest))
        (setq-local company-frontends '(company-preview-frontend)))
    (company-mode -1)
    (kill-local-variable 'company-active-map)
    (kill-local-variable 'company-idle-delay)
    (kill-local-variable 'company-backends)
    (kill-local-variable 'company-frontends)))

(provide 'esh-autosuggest)

;;; esh-autosuggest.el ends here
