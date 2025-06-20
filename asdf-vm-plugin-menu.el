;;; asdf-vm-plugin-menu.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Homepage: https://github.com/zellio/emacs-asdf-vm
;; Keywords: tools languages asdf-vm asdf

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'browse-url)

(require 'asdf-vm-plugin)

(defgroup asdf-vm-plugin-menu nil
  "ASDF-VM plugin tabulated list configuration group."
  :prefix "asdf-vm-plugin-menu-"
  :group 'asdf-vm)

(defcustom asdf-vm-plugin-menu-buffer-name "*ASDF-VM Plugins*"
  "Display buffer name for `asdf-vm-plugin-menu'."
  :group 'asdf-vm-plugin-menu
  :type 'string)

(defcustom asdf-vm-plugin-menu-list-padding 2
  "`tabulated-list-padding' for `asdf-vm-plugin-menu'."
  :group 'asdf-vm-plugin-menu
  :type 'integer
  :safe 'numberp)

(defcustom asdf-vm-plugin-menu-status-column-width 10
  "Column width for the status column of `asdf-vm-plugin-menu'."
  :group 'asdf-vm-plugin-menu
  :type 'integer
  :safe 'numberp)

(defcustom asdf-vm-plugin-menu-name-column-width 29
  "Column width for the name column of `asdf-vm-plugin-menu'."
  :group 'asdf-vm-plugin-menu
  :type 'integer
  :safe 'numberp)

(defcustom asdf-vm-plugin-menu-url-column-width 0
  "Column width for the repository url column of `asdf-vm-plugin-menu'."
  :group 'asdf-vm-plugin-menu
  :type 'integer
  :safe 'numberp)

(defun asdf-vm-plugin-menu--get-status ()
  "Return status of plugin at point in ASDF-VM Plugin Menu."
  (if-let* ((id (tabulated-list-get-id))
            (entry (and id (assoc id tabulated-list-entries))))
      (aref (cadr entry) 0)
    ""))

(defun asdf-vm-plugin-menu--get-name ()
  "Return name of plugin at point in ASDF-VM Plugin Menu."
  (if-let* ((id (tabulated-list-get-id))
            (entry (and id (assoc id tabulated-list-entries))))
      (aref (cadr entry) 1)
    ""))

(defun asdf-vm-plugin-menu--get-url ()
  "Return url of plugin at point in ASDF-VM Plugin Menu."
  (if-let* ((id (tabulated-list-get-id))
            (entry (and id (assoc id tabulated-list-entries))))
      (aref (cadr entry) 2)
    ""))

(defun asdf-vm-plugin-menu--ensure-mode ()
  "Signal an error if major mode is not `asfd-vm-plugin-menu-mode'."
  (unless (derived-mode-p 'asdf-vm-plugin-menu-mode)
    (signal 'asdf-vm-incorrect-mode-error (list major-mode))))

(defun asdf-vm-plugin-menu-mark-unmark (&optional _)
  "Clear any tags on the current plugin and move to the next line.

The current plugin is the plugin at point."
  (interactive "p" asdf-vm-plugin-menu-mode)
  (asdf-vm-plugin-menu--ensure-mode)
  (tabulated-list-put-tag " " t))

(defun asdf-vm-plugin-menu-backup-unmark (&optional _)
  "Back up one line and clear any tags on that line's plugin."
  (interactive "p" asdf-vm-plugin-menu-mode)
  (asdf-vm-plugin-menu--ensure-mode)
  (forward-line -1)
  (tabulated-list-put-tag " "))

(defun asdf-vm-plugin-menu-mark-delete (&optional _)
  "Mark the current plugin for deletion and move to the next line.

The current plugin is the plugin at point."
  (interactive "p" asdf-vm-plugin-menu-mode)
  (asdf-vm-plugin-menu--ensure-mode)
  (if (string= (asdf-vm-plugin-menu--get-status) "installed")
      (tabulated-list-put-tag "D" t)
    (forward-line)))

(defun asdf-vm-plugin-menu-mark-install (&optional _)
  "Mark the current plugin for installation and move to the next line.

The current plugin is the plugin at point."
  (interactive "p" asdf-vm-plugin-menu-mode)
  (asdf-vm-plugin-menu--ensure-mode)
  (if (string= (asdf-vm-plugin-menu--get-status) "available")
      (tabulated-list-put-tag "I" t)
    (forward-line)))

(defun asdf-vm-plugin-browse-url (url &optional secondary)
  "Open the repository URL of the plugin under point in a browser.

If SECONDARY (interactively, the prefix) use the secondary browser."
  (interactive
   (list
    (asdf-vm-plugin-menu--get-url)
    current-prefix-arg)
   asdf-vm-plugin-menu-mode)
  (when (string= url "")
    (signal
     'asdf-vm-plugin-menu-missing-url-error
     (list (asdf-vm-plugin-menu--get-name))))
  (if secondary
      (funcall browse-url-secondary-browser-function url)
    (browse-url url)))

(defun asdf-vm-plugin-menu-execute (&optional _)
  "Perform marked ASDF-VM Plugin Menu actions."
  (interactive "p" asdf-vm-plugin-menu-mode)
  (asdf-vm-plugin-menu--ensure-mode)
  (let* (install-list delete-list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (pcase (char-after)
          (?I (push
               (cons
                (asdf-vm-plugin-menu--get-name)
                (asdf-vm-plugin-menu--get-url))
               install-list))
          (?D (push (asdf-vm-plugin-menu--get-name) delete-list)))
        (forward-line)))
    (when delete-list
      (dolist (plugin delete-list)
        (asdf-vm-plugin-remove plugin t))
      (asdf-vm-message "Deleted plugins: %s" (string-join delete-list ", ")))
    (when install-list
      (pcase-dolist (`(,plugin . ,git-url) install-list)
        (asdf-vm-plugin-add plugin git-url t))
      (asdf-vm-message "Installed plugins: %s"
                       (string-join (seq-map #'car install-list) ", "))))
  (asdf-vm-plugin-menu--refresh))

(defface asdf-vm-plugin-menu-status-available
  '((t :inherit default))
  "Face used on the status and version of available plugins.")

(defface asdf-vm-plugin-menu-status-installed
  '((t :inherit font-lock-comment-face))
  "Face used on the status and version of installed plugins.")

(defun asdf-vm-plugin-menu--list-entries ()
  "Generate `tabulated-list-entries' for `asdf-vm-plugin-menu'."
  (let ((repository-alist (asdf-vm-plugin--repository-alist))
        (installed-plugins (asdf-vm-plugin-list)))
    (seq-map
     (lambda (cell)
       (pcase-let ((`(,plugin . ,url) cell))
         (let* ((status (if (member plugin installed-plugins) "installed" "available"))
                (face (pcase status
                        ("available" 'asdf-vm-plugin-menu-status-available)
                        ("installed" 'asdf-vm-plugin-menu-status-installed))))
           (list
            url
            (vector
             (propertize status 'font-lock-face face)
             (propertize plugin 'font-lock-face face)
             (propertize url 'font-lock-face face))))))
     repository-alist)))

(defun asdf-vm-plugin-menu--refresh (&rest _args)
  "Repopulated `tabulated-list-entries' for `asdf-vm-plugin-menu' and redraw."
  (tabulated-list-init-header)
  (setq
   asdf-vm-plugin--repository-alist nil
   tabulated-list-entries (asdf-vm-plugin-menu--list-entries))
  (tabulated-list-print t))

(defun asdf-vm-plugin-menu--imenu-prev-index-position-function (&rest _args)
  "Move point to previous line in `asdf-vm-plugin-menu' buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp) (forward-line -1)))

(defun asdf-vm-plugin-menu--status-predicate (a b)
  "Predicate to sort `asdf-vm-plugin-menu' buffer by the status column.

A and B are `tabulated-list-entries' records."
  (let ((a-status (aref (cadr a) 0))
        (b-status (aref (cadr b) 0)))
    (if (string= a-status b-status)
        (asdf-vm-plugin-menu--name-predicate a b)
      (string> a-status b-status))))

(defun asdf-vm-plugin-menu--name-predicate (a b)
  "Predicate to sort `asdf-vm-plugin-menu' buffer by the name column.

A and B are `tabulated-list-entries' records."
  (let ((a (aref (cadr a) 1)) (b (aref (cadr b) 1)))
    (string< a b)))

(defun asdf-vm-plugin-menu--url-predicate (a b)
  "Predicate to sort `asdf-vm-plugin-menu' buffer by the url column.

A and B are `tabulated-list-entries' records."
  (let ((a (aref (cadr a) 2)) (b (aref (cadr b) 1)))
    (string< a b)))

(defvar-keymap asdf-vm-plugin-menu-mode-map
  :doc "Local keymap for `asdf-vm-plugin-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "u"     #'asdf-vm-plugin-menu-mark-unmark
  "DEL"   #'asdf-vm-plugin-menu-backup-unmark
  "d"     #'asdf-vm-plugin-menu-mark-delete
  "i"     #'asdf-vm-plugin-menu-mark-install
  "r"     #'revert-buffer
  "w"     #'asdf-vm-plugin-browse-url
  "x"     #'asdf-vm-plugin-menu-execute)

(easy-menu-define asdf-vm-plugin-menu-mode-menu asdf-vm-plugin-menu-mode-map
  "Menu for `asdf-vm-plugin-menu-mode-map'."
  '("ASDF-VM Plugin"
    ["Refresh Plugin List" revert-buffer t :help "Rescan on-disk plugin repository"]
    ["Open Plugin Website" asdf-vm-plugin-browse-url t :help "Open the website of this plugin"]
    ["Execute Marked Actions" asdf-vm-plugin-menu-execute t :help "Perform all the marked actions"]
    "--"
    ["Mark for Installation" asdf-vm-plugin-menu-mark-install t :help "Mark a plugin for installation and move to the next line"]
    ["Mark for Deletion" asdf-vm-plugin-menu-mark-delete t :help "Mark a plugin for deletion and move to the next line"]
    ["Unmark" asdf-vm-plugin-menu-mark-unmark t :help "Clear any marks on a plugin and move to the next line"]
    "--"
    ["Quit" quit-window t :help "Quit plugin selection"]
    ["Customize" (customize-group 'asdf-vm-plugin-menu)]))

(define-derived-mode asdf-vm-plugin-menu-mode tabulated-list-mode "ASDF-VM Plugin Menu"
  "Major mode for browsing a list of ASDF-VM plugins.

The most useful commands here are:

  `x': Install the plugin under point if it isn't already installed,
       and delete it if it's already installed,
  `i': Mark a plugin for installation, and
  `d': mark a plugin for deletion.

Use `x' to perform the actions on the marked files.
\\<asdf-vm-plugin-menu-mode-map>
\\{asdf-vm-plugin-menu-mode-map}"
  :interactive nil
  (setq
   tabulated-list-format
   `[("Status" ,asdf-vm-plugin-menu-status-column-width asdf-vm-plugin-menu--status-predicate)
     ("Plugin" ,asdf-vm-plugin-menu-name-column-width asdf-vm-plugin-menu--name-predicate)
     ("Repository Url" ,asdf-vm-plugin-menu-url-column-width asdf-vm-plugin-menu--url-predicate)]
   tabulated-list-padding asdf-vm-plugin-menu-list-padding
   tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header)
  (setq
   revert-buffer-function #'asdf-vm-plugin-menu--refresh)
  (setf
   imenu-prev-index-position-function #'asdf-vm-plugin-menu--imenu-prev-index-position-function
   imenu-extract-index-name-function #'tabulated-list-get-id))

;;;###autoload
(defun asdf-vm-plugin-menu ()
  "Display a list of ASDF-VM Plugins.

This first refreshes the disk-cache for `asdf-vm-plugin-list'."
  (interactive)
  (let* ((buffer (get-buffer-create asdf-vm-plugin-menu-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-file-coding-system 'utf-8)
      (asdf-vm-plugin-menu-mode)
      (asdf-vm-plugin-menu--refresh))
    (pop-to-buffer-same-window buffer)))

(provide 'asdf-vm-plugin-menu)

;;; asdf-vm-plugin-menu.el ends here
