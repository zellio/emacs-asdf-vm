;;; asdf-vm-tool-versions.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

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

(require 'eieio)
(require 'eieio-custom)
(require 'asdf-vm-plugin)
(require 'asdf-vm-core)
(require 'asdf-vm-ui)

(defgroup asdf-vm-tool-versions nil
  "ASDF-VM tool-versions file configuration group."
  :prefix "asdf-vm-tool-versions-"
  :group 'asdf-vm)

(defun asdf-vm-tool-versions--file-row-tool-complete (widget)
  "WIDGET adapter for `asdf-vm-plugin--installed-plugin-completing-read'."
  (let* ((input (widget-field-value-get widget))
         (completion
          (asdf-vm-plugin--installed-plugin-completing-read nil t input)))
    (widget-field-value-set widget completion)))

(defun asdf-vm-tool-versions--file-row-versions-complete (widget)
  "WIDGET adapter for `asdf-vm--installed-package-version-completing-read'."
  (let* ((repeat (widget-get widget :parent))
         (object-slot (widget-get repeat :parent))
         (object-edit (widget-get object-slot :parent))
         (object-slot (car (widget-get object-edit :children)))
         (string (car (widget-get object-slot :children)))
         (current-tool-name (widget-field-value-get string))
         (initial-completion
          (asdf-vm--installed-package-version-completing-read
           current-tool-name nil t))
         (completion (pcase initial-completion
                       ((or "ref" "ref:")
                        (let* ((ref (read-string "Tool git ref: ")))
                          (format "ref:%s" ref)))
                       ((or "path" "path:")
                        (let* ((path (read-file-name "Tool path: " nil nil t)))
                          (format "path:%s" path)))
                       (_ initial-completion))))
    (widget-field-value-set widget completion)))

(defclass asdf-vm-tool-versions--file-row (eieio-default-superclass)
  ((tool
    :initarg :tool
    :type string
    :initform ""
    :custom (string :complete asdf-vm-tool-versions--file-row-tool-complete)
    :documentation "ASDF-VM plugin name")
   (versions
    :initarg :versions
    :initform (list "")
    :type list
    :custom (repeat
             (string
              :complete asdf-vm-tool-versions--file-row-versions-complete))
    :documentation "List of tool versions in priority order"))
  "Internal row value for `asdf-vm-tool-versions--field'.")

(cl-defmethod asdf-vm-ui--decode
  ((class (subclass asdf-vm-tool-versions--file-row)) line)
  "Decode CLASS symbol `asdf-vm-tool-versions--file-row' from LINE."
  (let* ((tokens (split-string line split-string-default-separators t)))
    (make-instance class :tool (car tokens) :versions (cdr tokens))))

(cl-defmethod asdf-vm-ui--encode
  ((obj asdf-vm-tool-versions--file-row))
  "Encode variable `asdf-vm-tool-versions--file-row' OBJ to string."
  (let* ((tool (slot-value obj 'tool))
         (versions (slot-value obj 'versions))
         (tokens (append (list tool) versions)))
    (string-join tokens " ")))

(defclass asdf-vm-tool-versions--file (asdf-vm-ui--file-backed)
  ((rows
    :initarg :rows
    :initform ()
    :type list
    :custom (editable-list
             (object :objecttype asdf-vm-tool-versions--file-row))
    :documentation "List of tool version entries."))
  "ASDF-VM tool-versions file.")

(defsubst asdf-vm-tool-versions--file-decode-line-filter (line)
  "Filter LINE for comment and empty string."
  (let* ((line (car (string-split line "#"))))
    (unless (string-empty-p line) line)))

(cl-defmethod asdf-vm-ui--decode
  ((class (subclass asdf-vm-tool-versions--file)) text)
  "Decode CLASS symbol `asdf-vm-tool-versions--file' from TEXT."
  (let* ((lines (string-lines text))
         (filtered-lines
          (seq-map #'asdf-vm-tool-versions--file-decode-line-filter lines))
         (rows
          (seq-map (apply-partially
                    #'asdf-vm-ui--decode 'asdf-vm-tool-versions--file-row)
                   filtered-lines)))
    (asdf-vm-message "%s" lines)
    (asdf-vm-message "%s" filtered-lines)
    (asdf-vm-message "%s" rows)
    (make-instance class :rows rows)))

(cl-defmethod asdf-vm-ui--encode ((obj asdf-vm-tool-versions--file))
  "Encode variable `asdf-vm-tool-versions--file' OBJ to string."
  (let* ((rows (slot-value obj 'rows))
         (lines (seq-map #'asdf-vm-ui--encode rows)))
    (string-join lines "\n")))

(defsubst asdf-vm-tool-versions--locate-dominate-file-matcher (path)
  "Return non-nil when PATH exists and is readable."
  (and path (file-readable-p path) path))

(defsubst asdf-vm-tool-versions--locate-dominating-file-sorter (a b)
  "Sort A and B by string length."
  (> (length a) (length b)))

(defun asdf-vm-tool-versions--locate-dominating-file ()
  "Locate dominating tool-versions file from current buffer directory.

This function will search first within the local directory, then within
the current project according to both `project' or `projectile', and
finally the default .tool-versions location."
  (let* ((project-path
          (cond
           ((and (featurep 'projectile)
                 (fboundp 'projectile-project-p)
                 (projectile-project-p))
            (expand-file-name ".tool-versions" (projectile-project-p)))
           ((and (featurep 'project)
                 (fboundp 'current-project)
                 (current-project))
            (expand-file-name ".tool-versions" (cdr (current-project))))))
         (dominating-path
          (when-let* ((path (and buffer-file-name
                                 (locate-dominating-file
                                  buffer-file-name ".tool-versions"))))
            (expand-file-name ".tool-versions" path)))
         (default-path (expand-file-name ".tool-versions" "~"))
         (paths
          (sort
           (list project-path dominating-path default-path)
           #'asdf-vm-tool-versions--locate-dominating-file-sorter)))
    (seq-find #'asdf-vm-tool-versions--locate-dominate-file-matcher paths)))


;;;###autoload
(defun asdf-vm-tool-versions-edit (&optional path)
  "Customize tool-versions file at PATH."
  (interactive
   (list
    (if current-prefix-arg (read-file-name "ASDF-VM Tool Versions: ")
      (asdf-vm-tool-versions--locate-dominating-file))))
  (let* ((tool-versions
          (if (file-readable-p path)
              (asdf-vm-ui--read 'asdf-vm-tool-versions--file path)
            (make-instance 'asdf-vm-tool-versions--file :path path))))
    (eieio-customize-object tool-versions)))

(provide 'asdf-vm-tool-versions)

;;; asdf-vm-tool-versions.el ends here
