;;; asdf-vm-plugin.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Homepage: https://github.com/zellio/emacs-asdf-vm
;; Keywords: languages asdf

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

(require 'cl-lib)

(require 'asdf-vm-config)
(require 'asdf-vm-util)
(require 'asdf-vm-process)

(defgroup asdf-vm-plugin nil
  "Configuration subgroup for ASDF-VM plugins."
  :prefix "asdf-vm-plugin-"
  :group 'asdf-vm)

(defcustom asdf-vm-plugin-github-url "https://github.com/asdf-vm/asdf-plugins"
  "Source url for ASDF-VM installation."
  :group 'asdf-vm-plugin
  :type 'string)

(defcustom asdf-vm-plugin-repository-path asdf-vm--plugin-index-directory
  "Source url for ASDF-VM installation."
  :group 'asdf-vm-plugin
  :type 'string)

(defun asdf-vm-plugin--parse-index-file (path)
  "Read and extract the repository value from a plugin repository file at PATH."
  (unless (file-readable-p path)
    (signal 'asdf-vm-plugin-unreadable-repository-file (list path)))
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (search-forward "repository = ")
    (let* ((plugin-name (file-name-nondirectory path))
           (plugin-url (buffer-substring-no-properties (point) (line-end-position))))
      (cons plugin-name plugin-url))))

(defvar asdf-vm-plugin--repository-alist nil
  "Memoization variable for function `asdf-vm-plugin--repository-alist'.")

(defun asdf-vm-plugin--repository-alist ()
  "Alist representation of the on disk plugin repository.

NB. This value gets memoized on first call and may need to be manually
unset if on disk representation changes."
  (or asdf-vm-plugin--repository-alist
      (setq asdf-vm-plugin--repository-alist
            (let* ((plugins-directory (expand-file-name "plugins" asdf-vm-plugin-repository-path))
                   (plugin-repository-paths (directory-files plugins-directory t (rx line-start (not ?.)))))
              (seq-map #'asdf-vm-plugin--parse-index-file plugin-repository-paths)))))

(defun asdf-vm-plugin--plugin-completing-read
    (&optional predicate require-match initial-input hist def inherit-input-method)
  "Completing read for working with NAME of plugins.

PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
are as in `compelting-read'."
  (let* ((plugin-names (seq-map #'car (asdf-vm-plugin--repository-alist))))
    (completing-read
     "Plugin name: " plugin-names predicate require-match
     initial-input hist def inherit-input-method)))

(defun asdf-vm-plugin--git-url-read-string
    (plugin &optional initial-input history default-value inherit-input-method)
  "Completing read for working with GIT-URL of PLUGIN.

INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD as in `read-string'"
  (let* ((repository-alist (asdf-vm-plugin--repository-alist))
         (repository-url
          (alist-get plugin repository-alist "" nil #'string-equal))
         (initial-input (or initial-input repository-url)))
    (read-string
     "Plugin git url: " initial-input history
     default-value inherit-input-method)))

;;;###autoload
(defun asdf-vm-plugin-add (name git-url &optional interactive-call)
  "Add the plugin NAME from GIT-URL to ASDF-VM.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let* ((plugin (asdf-vm-plugin--plugin-completing-read nil t)))
     (list
      plugin
      (asdf-vm-plugin--git-url-read-string plugin)
      (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (asdf-vm-message "Installing plugin: %s" name))
  (asdf-vm-call
   :command '(plugin add)
   :command-arguments (list name git-url)
   :blocking interactive-call))

(defun asdf-vm-plugin--installed-plugins ()
  "List plugins installed for ASDF-VM based on disk state."
  (let* ((plugin-paths (directory-files asdf-vm--plugins-directory t (rx line-start (not ?.)))))
    (seq-map #'file-name-base plugin-paths)))


;;;###autoload
(defun asdf-vm-plugin-list (&optional interactive-call)
  "List plugins installed for ASDF-VM.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (let ((plugins (asdf-vm-plugin--installed-plugins)))
    (when interactive-call
      (asdf-vm-message "Installed plugins: %s" (string-join plugins ", ")))
    plugins))

;;;###autoload
(defun asdf-vm-plugin-list-all (&optional interactive-call)
  "List all plugins for ASDF-VM in the on disk repository.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (cl-labels ((listify-alist (cons) (list (car cons) (cdr cons))))
    (let* ((plugins (seq-map #'listify-alist (asdf-vm-plugin--repository-alist))))
      (when interactive-call
        (asdf-vm-message "Available plugins: %s" (string-join (seq-map #'car plugins) ", ")))
      plugins)))

(defun asdf-vm-plugin--installed-plugin-completing-read
    (&optional
     predicate require-match initial-input hist def inherit-input-method)
  "Completing read for installed ASDF-VM plugins.

PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
are as in `compelting-read'."
  (let* ((plugins (asdf-vm-plugin--installed-plugins)))
    (completing-read
     "Installed plugin name: " plugins predicate require-match
     initial-input hist def inherit-input-method)))

;;;###autoload
(defun asdf-vm-plugin-remove (name &optional interactive-call)
  "Remove plugin NAME from asdf.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read nil t)
    (prefix-numeric-value current-prefix-arg)))
  (when interactive-call
    (asdf-vm-message "Removing plugin: %s" name))
  (asdf-vm-call
   :command '(plugin remove)
   :command-arguments (list name)
   :blocking interactive-call))

;;;###autoload
(defun asdf-vm-plugin-update (name &optional git-ref interactive-call)
  "Update plugin NAME to optional GIT-REF or current GIT-REF.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read nil t)
    (read-string "Plugin git ref: ")
    (prefix-numeric-value current-prefix-arg)))
  (let ((command-arguments (if git-ref (list name git-ref) (list name))))
    (when interactive-call
      (asdf-vm-message "Updating plugin: %s" name))
    (asdf-vm-call
     :command '(plugin update)
     :command-arguments command-arguments
     :blocking interactive-call)))

;;;###autoload
(defun asdf-vm-plugin-update-all (&optional interactive-call)
  "Update all ASDF-VM plugins based on current git-ref values.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (when interactive-call
    (asdf-vm-message "Updating all installed plugins"))
  (asdf-vm-call
   :command '(plugin update)
   :command-arguments '("--all")
   :blocking interactive-call))

(provide 'asdf-vm-plugin)

;;; asdf-vm-plugin.el ends here
