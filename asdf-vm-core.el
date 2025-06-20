;;; asdf-vm-core.el --- ASDF-VM porcelain commands -*- lexical-binding: t -*-

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

;; Emacs bindings for ASDF-VM commands.

;; All commands, with the exception of `exec' and `env' are provided as
;; `interactive' functions named `asdf-vm-COMMAND'.

;;; Code:

(require 'asdf-vm-util)
(require 'asdf-vm-process)
(require 'asdf-vm-plugin)

;;;###autoload
(defun asdf-vm-current (&optional name interactive-call)
  "Display current version set or being used for one or all packages.

If the argument NAME is provided, only display the version for that package.

Interactively, `asdf-vm-current' will prompt for NAME unless called
with a prefix argument.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (unless current-prefix-arg (asdf-vm-plugin--installed-plugin-completing-read nil t))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((response (asdf-vm-call
                    :command 'current
                    :command-arguments (flatten-list (list name))
                    :output t))
         (response-tokens (asdf-vm--parse-skip-list response)))
    (when interactive-call
      (asdf-vm-message
       "Package version%s%s" (if name ": " "s\n") (asdf-vm--format-skip-list response-tokens)))
    (cdr response-tokens)))

(defcustom asdf-vm-help-buffer-name "*asdf-vm-help*"
  "Display buffer for `asdf-vm-help' response."
  :group 'asdf-vm
  :type 'string)

(defcustom asdf-vm-help-fill-column-width fill-column
  "Column width for `asdf-vm-help' display buffer formatting."
  :group 'asdf-vm
  :type 'integer)

;;;###autoload
(defun asdf-vm-help (name &optional version)
  "Display documentation for plugin NAME at optional version VERSION.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((plugin (asdf-vm-plugin--installed-plugin-completing-read)))
     (list
      plugin
      (when current-prefix-arg (completing-read "Plugin version: " (asdf-vm-list-all plugin))))))
  (let ((response
         (asdf-vm-call
          :command 'help
          :command-arguments (flatten-list (list name version))
          :output t)))
    (with-current-buffer (get-buffer-create asdf-vm-help-buffer-name)
      (let ((inhibit-read-only t)
            (fill-column asdf-vm-help-fill-column-width))
        (erase-buffer)
        (insert response)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line-start (line-beginning-position)) (line-end (line-end-position)))
            (when (> (- line-end line-start) fill-column)
              (fill-region-as-paragraph line-start line-end)))
          (forward-line))
        (read-only-mode +1)
        (help-mode))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun asdf-vm-install (name &optional version interactive-call)
  "Install package NAME at version VERSION.

When the value VERSION is nil, asdf will take from .tool-versions file.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((package (asdf-vm-plugin--installed-plugin-completing-read nil t)))
     (list
      package
      (unless current-prefix-arg (completing-read "Package version: " (asdf-vm-list-all package)))
      (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (if version
        (asdf-vm-message "Installing package %s at version %s" name version)
      (asdf-vm-message "Installing package %s from tool-versions" name)))
  (asdf-vm-call
   :command 'install
   :command-arguments `(,name ,@(and version (list version)))
   :blocking interactive-call))

;;;###autoload
(defun asdf-vm-latest (name &optional version-filter interactive-call)
  "Fetch latest version of package NAME, optionally filtered by VERSION-FILTER.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read nil t)
    (when current-prefix-arg (read-string "Package version: "))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((response
          (asdf-vm-call
           :command 'latest
           :command-arguments (flatten-list (list name version-filter))
           :output t))
         (version (string-trim response)))
    (when interactive-call
      (asdf-vm-message "Latest version for package %s: %s" name version))
    version))

;;;###autoload
(defun asdf-vm-list (name &optional version-filter interactive-call)
  "List installed version of package NAME optionally filtered by VERSION-FILTER.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read nil t)
    (unless current-prefix-arg (read-string "Version filter: "))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((response
          (asdf-vm-call
           :command 'list
           :command-arguments (flatten-list (list name version-filter))
           :output t))
         (versions
          (seq-map #'string-trim (string-lines response t))))
    (when interactive-call
      (asdf-vm-message "Package %s installed versions: %s" name (string-join versions ", ")))
    versions))

(defun asdf-vm--installed-package-version-completing-read
    (name &optional predicate require-match initial-input hist def inherit-input-method)
  "Completing read for working with VERSION of package NAME.

PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
are as in `compelting-read'."
  (let* ((package-versions (asdf-vm-list name))
         (version (completing-read
                   "Package version: " package-versions predicate
                   require-match initial-input hist def inherit-input-method)))
    (string-trim version (rx (one-or-more (or ?* space))))))

;;;###autoload
(defun asdf-vm-list-all (name &optional version-filter interactive-call)
  "List all versions of package NAME filtered by VERSION-FILTER.

When called interactively, one can suppress the prompt for VERSION-FILTER by
using a prefix argument.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read nil t)
    (unless current-prefix-arg (read-string "Version filter: "))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((response
          (asdf-vm-call
           :command '(list all)
           :command-arguments (flatten-list (list name version-filter))
           :output t))
         (versions
          (seq-map #'string-trim (string-lines response t))))
    (when interactive-call
      (asdf-vm-message "Package %s versions: %s" name (string-join versions ", ")))
    versions))

;;;###autoload
(defun asdf-vm-set (name version  &optional interactive-call)
  "Set NAME VERSION in a .tool-versions file.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((package (asdf-vm-plugin--installed-plugin-completing-read nil t)))
     (list
      package
      (asdf-vm--installed-package-version-completing-read package nil t)
      (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (asdf-vm-message "Setting package %s version %s" name version))
  (asdf-vm-call
   :command 'set
   :command-arguments (list name version)
   :blocking interactive-call))

;;;###autoload
(defun asdf-vm-uninstall (name version &optional interactive-call)
  "Uninstall version VERSION of package NAME.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((package (asdf-vm-plugin--installed-plugin-completing-read nil t)))
     (list
      package
      (asdf-vm--installed-package-version-completing-read package)
      (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (asdf-vm-message "Uninstalling package %s version %s" name version))
  (asdf-vm-call
   :command 'uninstall
   :command-arguments (list name version)
   :blocking interactive-call))

;;;###autoload
(defun asdf-vm-where (name &optional version interactive-call)
  "Display path for installed version VERSION of package NAME.

When called with a prefix argument, VERSION is ignored.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((package (asdf-vm-plugin--installed-plugin-completing-read nil t)))
     (list
      package
      (unless current-prefix-arg (asdf-vm--installed-package-version-completing-read package))
      (prefix-numeric-value current-prefix-arg))))
  (let* ((response
          (asdf-vm-call
           :command 'where
           :command-arguments (flatten-list (list name version))
           :output t))
         (path (string-trim response)))
    (when interactive-call
      (if version
          (asdf-vm-message "Path for %s at version %s: %s" name version path)
        (asdf-vm-message "Path for %s at tools-version: %s" name version path)))
    path))

;;;###autoload
(defun asdf-vm-which (command &optional interactive-call)
  "Display current path for COMMAND.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "sCommand: \np")
  (let* ((response
          (asdf-vm-call :command 'which :command-arguments (list command) :output t))
         (path (string-trim response)))
    (when interactive-call
      (asdf-vm-message "Path for command %s: %s" command path))
    path))

;;;###autoload
(defun asdf-vm-info (&optional interactive-call)
  "Fetch OS, Shell and ASDF debug information.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (let ((response (asdf-vm-call :command 'info :output t)))
    (when interactive-call
      (asdf-vm-message "%s" response))
    response))

;;;###autoload
(defun asdf-vm-version (&optional interactive-call)
  "Fetch the currently installed version of ASDF.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (let* ((response (asdf-vm-call :command 'version :output t))
         (version (string-trim response)))
    (when interactive-call
      (asdf-vm-message "Current version: %s" version))
    version))

;;;###autoload
(defun asdf-vm-reshim (name version &optional interactive-call)
  "Recreate shims for version VERSION of a package NAME.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (let ((package (asdf-vm-plugin--installed-plugin-completing-read nil t)))
     (list
      package
      (completing-read "Package version: " (asdf-vm-list package))
      (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (asdf-vm-message "Reshiming package %s version %s" name version))
  (asdf-vm-call
   :command 'reshim
   :command-arguments (list name version)
   :blocking interactive-call))

;;;###autoload
(defun asdf-vm-shim-versions (command &optional interactive-call)
  "List plugins and versions which provide COMMAND.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "sCommand: \np")
  (let* ((response
          (asdf-vm-call :command 'shim-versions :command-arguments (list command) :output t))
         (response-lines (string-lines response)))
    (when interactive-call
      (asdf-vm-message "Command %s provided by: %s" command (string-join response-lines ", ")))
    (seq-map #'split-string response-lines)))

(provide 'asdf-vm-core)

;;; asdf-vm-core.el ends here
