;;; asdf-vm-config.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl-macs))

(defvar asdf-vm-config-file-environment-variable "ASDF_CONFIG_FILE"
  "Environment variable  for `asdf-vm-config-file'.")

(defvar asdf-vm-config-file-default (expand-file-name ".asdfrc" "~")
  "Default value for `asdf-vm-config-file'.")

(defcustom asdf-vm-config-file
  (or (getenv asdf-vm-config-file-environment-variable)
      asdf-vm-config-file-default)
  "Path to the .asdfrc configuration file.

Can be set to any location. Must be an absolute path."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-tool-versions-filename-environment-variable "ASDF_TOOL_VERSIONS_FILENAME"
  "Environment variable for `asdf-vm-tool-versions-filename'.")

(defvar asdf-vm-tool-versions-filename-default ".tool-versions"
  "Default value for `asdf-vm-tool-versions-filename'.")

(defcustom asdf-vm-tool-versions-filename
  (or (getenv asdf-vm-tool-versions-filename-environment-variable)
      asdf-vm-tool-versions-filename-default)
  "The filename of the file storing the tool names and versions.

Can be any valid filename. Typically, you should not set this value unless you
want to ignore .tool-versions files."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-dir-environment-variable "ASDF_DIR"
  "Environment variable for `asdf-vm-dir'.")

(defvar asdf-vm-dir-default nil
  "Default value for `asdf-vm-dir'.")

(defcustom asdf-vm-dir
  (or (getenv asdf-vm-dir-environment-variable)
      asdf-vm-dir-default)
  "The location of asdf core scripts.

Can be set to any location. Must be an absolute path."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-data-dir-environment-variable "ASDF_DATA_DIR"
  "Environment variable for `asdf-vm-data-dir'.")

(defvar asdf-vm-data-dir-default
  (or (let* ((default (expand-file-name ".asdf" "~")))
        (and (file-directory-p default) default))
      asdf-vm-dir)
  "Default value for `asdf-vm-data-dir'.")

(defcustom asdf-vm-data-dir
  (or (getenv asdf-vm-data-dir-environment-variable)
      asdf-vm-data-dir-default)
  "The location where asdf will install plugins, shims and tool versions.

Can be set to any location. Must be an absolute path."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-concurrency-environment-variable "ASDF_CONCURRENCY"
  "Environment variable for `asdf-vm-concurrency'.")

(defvar asdf-vm-concurrency-default "ASDF_CONCURRENCY"
  "Default variable for `asdf-vm-concurrency'.")

(defcustom asdf-vm-concurrency
  (or (getenv asdf-vm-concurrency-environment-variable)
      asdf-vm-concurrency-default)
  "Number of cores to use when compiling the source code.

If set, this value takes precedence over the asdf config concurrency value."
  :type 'string
  :group 'asdf-vm)

(defsubst asdf-vm-expand-file-name (name)
  "Expand the file NAME under `asdf-vm-dir'."
  (expand-file-name name asdf-vm-dir))

(defsubst asdf-vm-expand-data-file-name (name)
  "Expand the file NAME under `asdf-vm-data-dir'."
  (expand-file-name name asdf-vm-data-dir))

(defvar asdf-vm--downloads-directory
  (file-name-as-directory
   (asdf-vm-expand-data-file-name "downloads"))
  "")

(defvar asdf-vm--installs-directory
  (file-name-as-directory
   (asdf-vm-expand-data-file-name "installs"))
  "")

(defvar asdf-vm--plugin-index-directory
  (file-name-as-directory
   (asdf-vm-expand-data-file-name "plugin-index"))
  "")

(defvar asdf-vm--plugins-directory
  (file-name-as-directory
   (asdf-vm-expand-data-file-name "plugins"))
  "")

(defvar asdf-vm--shims-directory
  (file-name-as-directory
   (asdf-vm-expand-data-file-name "shims"))
  "")

(defconst asdf-vm--tracked-environment-symbols
  `(asdf-vm-config-file
    asdf-vm-tool-versions-filename
    asdf-vm-dir
    asdf-vm-data-dir
    asdf-vm-concurrency)
  "List of managed ASDF-VM environment variables.")

(defsubst asdf-vm--environment-variable-symbol (symbol)
  ""
  (intern (format "%s-environment-variable" (symbol-name symbol))))

(defsubst asdf-vm--default-variable-symbol (symbol)
  ""
  (intern (format "%s-default" (symbol-name symbol))))

(defun asdf-vm--lookup-tracked-symbol (symbol)
  ""
  (let* ((env-symbol (asdf-vm--environment-variable-symbol symbol))
         (env-var (symbol-value env-symbol)))
    (cons env-var (getenv env-var))))

(defsubst asdf-vm--environment-state ()
  ""
  (seq-map
   #'asdf-vm--lookup-tracked-symbol asdf-vm--tracked-environment-symbols))

(defun asdf-vm--inject-environment ()
  ""
  (let* ((current-environment (asdf-vm--environment-state)))
    (dolist (symbol asdf-vm--tracked-environment-symbols)
      (let* ((environment-symbol (asdf-vm--environment-variable-symbol symbol)))
        (setenv (symbol-value environment-symbol) (symbol-value symbol))))
    current-environment))

(defun asdf-vm--rollback-environment (environment-alist)
  ""
  (pcase-dolist (`(,variable . ,value) environment-alist)
    (setenv variable value)))

(provide 'asdf-vm-config)

;;; asdf-vm-config.el ends here
