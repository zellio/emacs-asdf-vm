;;; asdf-vm-mode.el --- ASDF-VM porcelain commands -*- lexical-binding: t -*-

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

(require 'asdf-vm-core)
(require 'asdf-vm-installer)
(require 'asdf-vm-plugin)
(require 'asdf-vm-plugin-menu)
(require 'asdf-vm-tool-versions)

(defcustom asdf-vm-mode-line-format "(A)"
  "How `asdf-vm-mode' will indicate activity in the mode line."
  :group 'asdf-vm
  :type 'sexpr)

(defvar asdf-vm-core-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "e" #'asdf-vm-mode-enable)
    (define-key map "d" #'asdf-vm-mode-disable)
    (define-key map "c" #'asdf-vm-config-edit)
    (define-key map "t" #'asdf-vm-tool-versions-edit)
    (define-key map "C" #'asdf-vm-current)
    (define-key map "h" #'asdf-vm-help)
    (define-key map "i" #'asdf-vm-install)
    (define-key map "a" #'asdf-vm-latest)
    (define-key map "l" #'asdf-vm-list)
    (define-key map "L" #'asdf-vm-list-all)
    (define-key map "s" #'asdf-vm-set)
    (define-key map "u" #'asdf-vm-uninstall)
    (define-key map "w" #'asdf-vm-where)
    (define-key map "W" #'asdf-vm-which)
    (define-key map "n" #'asdf-vm-info)
    (define-key map "v" #'asdf-vm-version)
    (define-key map "r" #'asdf-vm-reshim)
    (define-key map "S" #'asdf-vm-shim-versions)
    map)
  "Command keymap for `asdf-vm-core'.")

(defvar asdf-vm-plugin-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "m" #'asdf-vm-plugin-menu)
    (define-key map "a" #'asdf-vm-plugin-add)
    (define-key map "l" #'asdf-vm-plugin-list)
    (define-key map "L" #'asdf-vm-plugin-list-all)
    (define-key map "r" #'asdf-vm-plugin-remove)
    (define-key map "u" #'asdf-vm-plugin-update)
    (define-key map "U" #'asdf-vm-plugin-update-all)
    map)
  "Command keymap for `asdf-vm-plugin'.")

(defvar asdf-vm-installer-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "i" #'asdf-vm-installer)
    (define-key map "l" #'asdf-vm-installer-list)
    (define-key map "L" #'asdf-vm-installer-list-all)
    (define-key map "d" #'asdf-vm-installer-download)
    (define-key map "I" #'asdf-vm-installer-install)
    (define-key map "a" #'asdf-vm-installer-activate)
    map)
  "Command keymap for `asdf-vm-installer'.")

(defcustom asdf-vm-mode-keymap-prefix "C-c a"
  "Keymode map prefix for `asdf-vm-mode'."
  :group 'asdf-vm
  :type 'string)

(defvar asdf-vm-mode-map
  (when asdf-vm-mode-keymap-prefix
    (let* ((map (make-sparse-keymap))
           (core-kbd (kbd asdf-vm-mode-keymap-prefix))
           (plugin-kbd (kbd (concat asdf-vm-mode-keymap-prefix "P")))
           (installer-kbd (kbd (concat asdf-vm-mode-keymap-prefix "I"))))
      (define-key map core-kbd asdf-vm-core-command-map)
      (define-key map plugin-kbd asdf-vm-plugin-command-map)
      (define-key map installer-kbd asdf-vm-installer-command-map)
      map))
  "Keymap for `asdf-vm-mode'.")

(easy-menu-define asdf-vm-mode-menu asdf-vm-mode-map
  "ASDF-VM mode menu."
  `("ASDF-VM"
    ["Enable Mode" asdf-vm-mode-enable :enable (not asdf-vm-mode) :visible (not asdf-vm-mode) :help "Unconditionally enable `asdf-vm-mode'."]
    ["Disable Mode" asdf-vm-mode-disable :enable asdf-vm-mode :visible asdf-vm-mode :help "Unconditionally disable `asdf-vm-mode'."]
    ["Edit Config" asdf-vm-config-edit :enable t :help "Customize ASDF-VM configuration file at PATH."]
    ["Edit Tools Versions" asdf-vm-tool-versions-edit :enable t :help "Customize tool-versions file at PATH."]
    "---"
    ["Current" asdf-vm-current :enable t :help "Display current version set or being used for one or all packages."]
    ["Help" asdf-vm-help :enable t :help "Display documentation for plugin NAME at optional version VERSION."]
    ["Install" asdf-vm-install :enable t :help "Install package NAME at version VERSION."]
    ["Latest" asdf-vm-latest :enable t :help "Fetch latest version of package NAME, optionally filtered by VERSION-FILTER."]
    ["List" asdf-vm-list :enable t :help "List installed version of package NAME optionally filtered by VERSION-FILTER."]
    ["List All" asdf-vm-list-all :enable t :help "List all versions of package NAME filtered by VERSION-FILTER."]
    ["Set" asdf-vm-set :enable t :help "Set NAME VERSION in a .tool-versions file."]
    ["Uninstall" asdf-vm-uninstall :enable t :help "Uninstall version VERSION of package NAME."]
    ["Where" asdf-vm-where :enable t :help "Display path for installed version VERSION of package NAME."]
    ["Which" asdf-vm-which :enable t :help "Display current path for COMMAND."]
    ["Info" asdf-vm-info :enable t :help "Fetch OS, Shell and ASDF debug information."]
    ["Version" asdf-vm-version :enable t :help "Fetch the currently installed version of ASDF."]
    ["Re-shim" asdf-vm-reshim :enable t :help "Recreate shims for version VERSION of a package NAME."]
    ["Shim Versions" asdf-vm-shim-versions :enable t :help "List plugins and versions which provide COMMAND."]
    "---"
    ("Installer"
     ["Run" asdf-vm-installer :enable t :help "Install and activate VERSION of ASDF-VM."]
     ["List" asdf-vm-installer-list :enable t :help "List installed versions of ASDF-VM."]
     ["List All" asdf-vm-installer-list-all :enable t :help "List all installable versions of ASDF-VM."]
     ["Download" asdf-vm-installer-download :enable t :help "Download source code or binary for the specified VERSION."]
     ["Install" asdf-vm-installer-install :enable t :help "Install the specified VERSION."]
     ["Activate" asdf-vm-installer-activate :enable t :help "Activate installed VERSION of ASDF-VM."])
    ("Plugin"
     ["Menu" asdf-vm-plugin-menu :enable t :help "Display a list of ASDF-VM Plugins."]
     ["Add" asdf-vm-plugin-add :enable t :help "Add the plugin NAME from GIT-URL to ASDF-VM."]
     ["List" asdf-vm-plugin-list :enable t :help "List plugins installed for ASDF-VM."]
     ["List All" asdf-vm-plugin-list-all :enable t :help "List all plugins for ASDF-VM in the on disk repository."]
     ["Remove" asdf-vm-plugin-remove :enable t :help "Remove plugin NAME from asdf."]
     ["Update" asdf-vm-plugin-update :enable t :help "Update plugin NAME to optional GIT-REF or current GIT-REF."]
     ["Update All" asdf-vm-plugin-update-all :enable t :help "Update all ASDF-VM plugins based on current git-ref values."])
    ["Customize" (customize-group 'asdf-vm-plugin-menu)]))

(defcustom asdf-vm-path-injection-behaviour 'prepend
  "Control how ASDF-VM updates the variable `exec-path'."
  :type '(choice
          (const :tag "Prepend path directories" prepend)
          (const :tag "Append path directories" append)
          (const :tag "Do not update `exec-path'" nil))
  :group 'asdf-vm)

(defun asdf-vm--inject-exec-path (&rest paths)
  "Update variable `exec-path' with PATHS."
  (pcase asdf-vm-path-injection-behaviour
    ('prepend (setq exec-path (append paths exec-path)))
    ('append (setq exec-path (append exec-path paths)))
    (_ (unless (executable-find asdf-vm-process-executable)
         (signal 'asdf-vm-no-exectuable-error nil)))))

(defun asdf-vm--clean-exec-path (&rest paths)
  "Clean PATHS from variable `exec-path'."
  (when asdf-vm-path-injection-behaviour
    (dolist (path paths)
      (setq
       exec-path (seq-remove (apply-partially #'string-equal path) exec-path)))))

(defvar asdf-vm-mode--state nil
  "State variable for `asdf-vm-mode'.")

(defun asdf-vm-mode--activate ()
  "Activate `asdf-vm-mode'."
  (add-to-list 'mode-line-misc-info asdf-vm-mode-line-format)
  (asdf-vm--inject-exec-path asdf-vm--shims-directory)
  (setenv "PATH" (string-join exec-path path-separator))
  (setq asdf-vm-mode--state (asdf-vm-config--state-inject)))

(defun asdf-vm-mode--deactivate ()
  "Deactivate `asdf-vm-mode'."
  (setq
   mode-line-misc-info (delete asdf-vm-mode-line-format mode-line-misc-info))
  (asdf-vm--clean-exec-path asdf-vm--shims-directory)
  (setenv "PATH" (string-join exec-path path-separator))
  (when asdf-vm-mode--state
    (asdf-vm-config--state-rollback asdf-vm-mode--state)
    (setq asdf-vm-mode--state nil)))

;;;###autoload
(define-minor-mode asdf-vm-mode
  "ASDF-VM porcelain minor mode.

\\{asdf-vm-mode-map}"
  :global t
  :group 'asdf-vm
  :lighter ""
  :keymap asdf-vm-mode-map
  (if asdf-vm-mode
      (asdf-vm-mode--activate)
    (asdf-vm-mode--deactivate)))

;;;###autoload
(defun asdf-vm-mode-enable ()
  "Unconditionally enable `asdf-vm-mode'."
  (asdf-vm-mode +1))

;;;###autoload
(defun asdf-vm-mode-disable ()
  "Unconditionally disable `asdf-vm-mode'."
  (asdf-vm-mode +1))

(provide 'asdf-vm-mode)

;;; asdf-vm-mode.el ends here
