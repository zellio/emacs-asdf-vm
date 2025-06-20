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

(defcustom asdf-vm-mode-line-format "(A)"
  "How `asdf-vm-mode' will indicate activity in the mode line."
  :group 'asdf-vm
  :type 'sexpr)

(defvar asdf-vm-core-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "c" #'asdf-vm-current)
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
  "Command keymap for asdf-vm-core.")

(defvar asdf-vm-plugin-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "a" #'asdf-vm-plugin-add)
    (define-key map "l" #'asdf-vm-plugin-list)
    (define-key map "L" #'asdf-vm-plugin-list-all)
    (define-key map "r" #'asdf-vm-plugin-remove)
    (define-key map "u" #'asdf-vm-plugin-update)
    (define-key map "U" #'asdf-vm-plugin-update-all)
    map)
  "Command keymap for asdf-vm-plugin.")

(defvar asdf-vm-installer-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "i" #'asdf-vm-installer)
    (define-key map "l" #'asdf-vm-installer-list)
    (define-key map "L" #'asdf-vm-installer-list-all)
    (define-key map "d" #'asdf-vm-installer-download)
    (define-key map "I" #'asdf-vm-installer-install)
    (define-key map "a" #'asdf-vm-installer-activate)
    map)
  "Command keymap for asdf-vm-installer.")

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
  "Keymap for asdf-vm-mode.")

(easy-menu-define asdf-vm-mode-menu asdf-vm-mode-map
  "ASDF-VM menu."
  `("ASDF-VM"
    ("Plugin"
     ["Add" asdf-vm-plugin-add t :help ""]
     ["List" asdf-vm-plugin-list t :help ""]
     ["List All" asdf-vm-plugin-list-all t :help ""]
     ["Remove" asdf-vm-plugin-remove t :help ""]
     ["Update" asdf-vm-plugin-update t :help ""]
     ["Update All" asdf-vm-plugin-update-all t :help ""])
    ("Installer"
     ["Installer" asdf-vm-installer-installer t :help ""]
     ["List" asdf-vm-installer-list t :help ""]
     ["List All" asdf-vm-installer-list-all t :help ""]
     ["Download" asdf-vm-installer-download t :help ""]
     ["Install" asdf-vm-installer-install t :help ""]
     ["Activate" asdf-vm-installer-activate t :help ""])
    "---"
    ["Current" asdf-vm--current t :help ""]
    ["Install" asdf-vm--install t :help ""]
    ["Latest" asdf-vm--latest t :help ""]
    ["List" asdf-vm--list t :help ""]
    ["List All" asdf-vm--list-all t :help ""]
    ["Set" asdf-vm--set t :help ""]
    ["Uninstall" asdf-vm--uninstall t :help ""]
    ["Where" asdf-vm--where t :help ""]
    ["Which" asdf-vm--which t :help ""]
    ["Info" asdf-vm--info t :help ""]
    ["Version" asdf-vm--version t :help ""]
    ["Reshim" asdf-vm--reshim t :help ""]
    ["Shim Versions" asdf-vm--shim-versions t :help ""]))

(defcustom asdf-vm-path-injection-behaviour 'prepend
  "Control how ASDF-VM updates the variable `exec-path'."
  :type '(choice
          (const :tag "Prepend path directories" prepend)
          (const :tag "Append path directories" append)
          (const :tag "Do not update `exec-path'" nil))
  :group 'asdf-vm)

(defun asdf-vm--inject-exec-path (&rest paths)
  ""
  (pcase asdf-vm-path-injection-behaviour
    ('prepend (setq exec-path (append paths exec-path)))
    ('append (setq exec-path (append exec-path paths)))))

(defun asdf-vm--clean-exec-path (&rest paths)
  ""
  (when asdf-vm-path-injection-behaviour
    (dolist (path paths)
      (setq
       exec-path (seq-remove (apply-partially #'string-equal path) exec-path)))))

(defvar asdf-vm-mode--state nil
  "State variable for `asdf-vm-mode'.")

;;;###autoload
(define-minor-mode asdf-vm-mode
  "Minor mode for asdf-vm interaction.

\\{asdf-vm-mode-map}"
  :global t
  :group 'asdf-vm
  :lighter ""
  :keymap asdf-vm-mode-map
  (if asdf-vm-mode
      (progn
        (add-to-list 'mode-line-misc-info asdf-vm-mode-line-format)
        (asdf-vm--inject-exec-path asdf-vm--shims-directory)
        (setenv "PATH" (string-join exec-path path-separator))
        (setq asdf-vm-mode--state (asdf-vm--inject-environment)))
    (setq
     mode-line-misc-info (delete asdf-vm-mode-line-format mode-line-misc-info))
    (asdf-vm--clean-exec-path asdf-vm--shims-directory)
    (setenv "PATH" (string-join exec-path path-separator))
    (when asdf-vm-mode--state
      (asdf-vm--rollback-environment asdf-vm-mode--state)
      (setq asdf-vm-mode--state nil))))

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
