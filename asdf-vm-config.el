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

;; ASDF-VM configuration values and management. Provided are elisp mappings
;; from the supported environment variables and glue code for swapping managed
;; environments.

;;; Code:

(require 'eieio)
(require 'eieio-custom)
(require 'asdf-vm-process)
(require 'asdf-vm-ui)

(defgroup asdf-vm-config nil
  "ASDF-VM Config configuration group."
  :prefix "asdf-vm-config-"
  :group 'asdf-vm)

(defcustom asdf-vm-config-file (or (getenv "ASDF_CONFIG_FILE")
                                   (expand-file-name ".asdfrc" "~"))
  "Path to the .asdfrc configuration file.

Can be set to any location. Must be an absolute path."
  :type 'string
  :group 'asdf-vm)

(defcustom asdf-vm-tool-versions-filename (or (getenv "ASDF_TOOL_VERSIONS_FILENAME") ".tool-versions")
  "The filename of the file storing the tool names and versions.

Must be an absolute path."
  :group 'asdf-vm-config
  :type 'string)

(defcustom asdf-vm-dir (or (getenv "ASDF_DIR")
                           (file-name-parent-directory asdf-vm-process-executable))
  "The location of ASDF-VM core scripts.

Can be set to any location. Must be an absolute path."
  :group 'asdf-vm-config
  :type 'string)

(defvar asdf-vm-data-dir-default
  (let* ((default-value (expand-file-name ".asdf" "~")))
    (if (file-directory-p default-value) default-value asdf-vm-dir))
  "Default value for `asdf-vm-data-dir'.")

(defcustom asdf-vm-data-dir (or (getenv "ASDF_DATA_DIR")
                                asdf-vm-data-dir-default)
  "The location where ASDF-VM will install plugins, shims and tool versions.

Can be set to any location. Must be an absolute path."
  :group 'asdf-vm-config
  :type 'string)

(defcustom asdf-vm-concurrency (or (getenv "ASDF_CONCURRENCY") "auto")
  "Number of cores to use when compiling the source code."
  :group 'asdf-vm-config
  :type 'string)

(defsubst asdf-vm--expand-data-dir-file-name (name)
  "Expand file NAME under `asdf-vm-data-dir'."
  (expand-file-name name asdf-vm-data-dir))

(defvar asdf-vm--downloads-directory
  (asdf-vm--expand-data-dir-file-name "download")
  "Location of ASDF-VM downloads.

Internal ASDF-VM structural path.")

(defvar asdf-vm--installs-directory
  (asdf-vm--expand-data-dir-file-name "installs")
  "Location of ASDF-VM installs.

Internal ASDF-VM structural path.")

(defvar asdf-vm--plugin-index-directory
  (asdf-vm--expand-data-dir-file-name "plugin-index")
  "Location of ASDF-VM plugin repository.

Internal ASDF-VM structural path.")

(defvar asdf-vm--plugins-directory
  (asdf-vm--expand-data-dir-file-name "plugins")
  "Location of installed ASDF-VM plugins.

Internal ASDF-VM structural path.")

(defvar asdf-vm--shims-directory
  (asdf-vm--expand-data-dir-file-name "shims")
  "Location of generated ASDF-VM shims.

Internal ASDF-VM structural path.")

(defconst asdf-vm-config--valid-file-fields
  '(always-keep-download
    concurrency
    disable-plugin-short-name-repository
    legacy-version-file
    plugin-repository-last-check-duration
    use-release-candidates)
  "Valid fields for serialization of instance variable `asdf-vm-config--file'.")

(defclass asdf-vm-config--file (asdf-vm-ui--file-backed)
  ((legacy-version-file
    :initarg :legacy-version-file
    :initform nil
    :type boolean
    :custom boolean
    :documentation "Plugins with support can read the versions files used by other version managers.")
   (use-release-candidates
    :initarg :use-release-candidates
    :initform nil
    :type boolean
    :custom boolean
    :documentation "")
   (always-keep-download
    :initarg :always-keep-download
    :initform nil
    :type boolean
    :custom boolean
    :documentation "Configure the asdf install command to keep or delete the source code or binary it downloads.")
   (plugin-repository-last-check-duration
    :initarg :plugin-repository-last-check-duration
    :initform 60
    :type integer
    :custom integer
    :documentation "Configure the duration (in minutes) between asdf plugin repository syncs.")
   (disable-plugin-short-name-repository
    :initarg :disable-plugin-short-name-repository
    :initform nil
    :type boolean
    :custom boolean
    :documentation "Disable synchronization of the asdf plugin short-name repository.")
   (concurrency
    :initarg :concurrency
    :initform "auto"
    :type string
    :custom string
    :documentation "The default number of cores to use during compilation."))
  "User ASDF-VM configuration file values.")

(defsubst asdf-vm-config--file-decode-key (key)
  "Decode CLOS KEY symbol into Emacs keyword."
  (let* ((key (string-trim key)))
    (intern (format ":%s" (string-replace "_" "-" key)))))

(defsubst asdf-vm-config--file-decode-value (value)
  "Decode configuration file VALUE into Emacs data type."
  (let* ((value (string-trim value)))
    (pcase value
      ("yes" t)
      ("no" nil)
      ((rx line-start (one-or-more digit) line-end) (string-to-number value 10))
      (_ value))))

(defun asdf-vm-config--file-decode-line (line)
  "Decode LINE into a list of KEY and VALUE."
  (let* ((idx (seq-position line ?= #'char-equal))
         (raw-key (substring-no-properties line 0 idx))
         (raw-value (substring-no-properties line (+ idx 1))))
    (list (asdf-vm-config--file-decode-key raw-key)
          (asdf-vm-config--file-decode-value raw-value))))

(defsubst asdf-vm-config--decode-line-filter (line)
  "Filter LINE by comment and empty strings."
  (let* ((line (car (string-split line ";"))))
    (unless (string-empty-p line) line)))

(cl-defmethod asdf-vm-ui--decode ((class (subclass asdf-vm-config--file)) text)
  "Decode the CLASS variable `asdf-vm-config--file' from TEXT."
  (let* ((lines (string-lines text))
         (filtered-lines
          (seq-filter #'asdf-vm-config--decode-line-filter lines))
         (tokens (seq-mapcat
                  #'asdf-vm-config--file-decode-line filtered-lines)))
    (apply #'make-instance class tokens)))

(defsubst asdf-vm-config--file-encode-key (key)
  "Encode CLOS KEY as configuration file keyword."
  (string-replace "-" "_" (symbol-name key)))

(defsubst asdf-vm-config--file-encode-value (value)
  "Encode Emacs VALUE as configuration file field value."
  (pcase value
    ((pred numberp) (number-to-string value))
    ((pred booleanp) (if value "yes" "no"))
    (_ value)))

(defsubst asdf-vm-config--file-encode-line (key value)
  "Encode KEY and VALUE as configuration file line."
  (format "%s = %s"
          (asdf-vm-config--file-encode-key key)
          (asdf-vm-config--file-encode-value value)))

(cl-defmethod asdf-vm-ui--encode
  ((obj asdf-vm-config--file))
  "Encode variable `asdf-vm-config--file' OBJ as string."
  (cl-labels ((encode-slot (slot)
                (let* ((name (eieio-slot-descriptor-name slot)))
                  (message "%s" name)
                  (when (member name asdf-vm-config--valid-file-fields)
                    (message "%s" name)
                    (asdf-vm-config--file-encode-line
                     name (slot-value obj name))))))
    (string-join
     (flatten-tree
      (seq-map #'encode-slot (eieio-class-slots 'asdf-vm-config--file)))
     "\n")))

;;;###autoload
(defun asdf-vm-config-edit (path)
  "Customize ASDF-VM configuration file at PATH."
  (interactive
   (list
    (if (or current-prefix-arg (not asdf-vm-config-file))
        (read-file-name "ASDF-VM Config: " nil nil t)
      asdf-vm-config-file)))
  (let* ((config
          (if (file-readable-p path)
              (asdf-vm-ui--read 'asdf-vm-config--file path)
            (make-instance 'asdf-vm-config--file :path path))))
    (eieio-customize-object config)))

(defconst asdf-vm-config--env-alist
  '((asdf-vm-config-file . "ASDF_CONFIG_FILE")
    (asdf-vm-tool-versions-filename . "ASDF_TOOL_VERSIONS_FILENAME")
    (asdf-vm-dir . "ASDF_DIR")
    (asdf-vm-data-dir . "ASDF_DATA_DIR")
    (asdf-vm-concurrency . "ASDF_CONCURRENCY"))
  "Mapping of internal state to environment variables.")

(defun asdf-vm-config--state-inject ()
  "Inject tracked state into environment."
  (asdf-vm-message "Injecting state")
  (cl-labels ((getenv-cell (cell)
                (pcase-let ((`(,symbol . ,name) cell))
                  (list symbol name (getenv name)))))
    (let* ((state (seq-map #'getenv-cell asdf-vm-config--env-alist)))
      (pcase-dolist (`(,symbol . ,name) asdf-vm-config--env-alist)
        (setenv name (symbol-value symbol)))
      (asdf-vm-message "state %s" state)
      state)))

(defun asdf-vm-config--state-rollback (state)
  "Rollback tracked STATE from environment."
  (asdf-vm-message "Rolling back state")
  (pcase-dolist (`(,_ ,name ,value) state)
    (setenv name value)))

(provide 'asdf-vm-config)

;;; asdf-vm-config.el ends here
