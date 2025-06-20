;;; asdf-vm-ui.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

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

(require 'asdf-vm-util)

(defclass asdf-vm-ui--file-backed (eieio-default-superclass)
  ((path
    :initarg :path
    :type string
    :custom string
    :documentation "Path to the backing file."))
  "Parent class for classes within `asdf-vm'.")

(cl-defgeneric asdf-vm-ui--decode (class text)
  "A generic function to decode CLASS from TEXT.")

(cl-defgeneric asdf-vm-ui--read-from-buffer (class &optional buffer)
  "A generic function to decode CLASS from BUFFER.")

(cl-defmethod asdf-vm-ui--read-from-buffer
  ((class (subclass asdf-vm-ui--file-backed)) &optional buffer)
  "Run `asdf-vm-ui--decode' for CLASS on BUFFER contents."
  (save-excursion
    (with-current-buffer (get-buffer-create (or buffer (current-buffer)))
      (asdf-vm-ui--decode
       class (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defgeneric asdf-vm-ui--read (class path)
  "A generic function to decode CLASS from the file at PATH.")

(cl-defmethod asdf-vm-ui--read ((class (subclass asdf-vm-ui--file-backed)) path)
  "Read contents of PATH and instantiate CLASS."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((instance (asdf-vm-ui--read-from-buffer class (current-buffer))))
      (setf (slot-value instance 'path) path)
      instance)))

(cl-defgeneric asdf-vm-ui--encode (obj)
  "A generic function to encode OBJ to string.")

(cl-defgeneric asdf-vm-ui--write-to-buffer (obj &optional buffer)
  "A generic function to encode OBJ to BUFFER.")

(cl-defmethod asdf-vm-ui--write-to-buffer
  ((obj asdf-vm-ui--file-backed) &optional buffer)
  "Writes `asdf-vm-ui--encode' from OBJ to BUFFER."
  (save-excursion
    (with-current-buffer (get-buffer-create (or buffer (current-buffer)))
      (insert (asdf-vm-ui--encode obj))
      (newline))))

(cl-defgeneric asdf-vm-ui--write (obj &optional path)
  "A generic function to encode OBJ to PATH.")

(cl-defmethod asdf-vm-ui--write ((obj asdf-vm-ui--file-backed) &optional path)
  "Writes `asdf-vm-ui--encode' from OBJ to PATH."
  (with-temp-buffer
    (asdf-vm-ui--write-to-buffer obj)
    (let ((path (or path (slot-value obj 'path))))
      (write-region (point-min) (point-max) path))))

(cl-defmethod eieio-done-customizing ((obj asdf-vm-ui--file-backed))
  "Write encoded OBJ to file at PATH."
  (let* ((path (slot-value obj 'path)))
    (asdf-vm-ui--write obj path)))

(cl-defmethod eieio-custom-object-apply-reset ((_obj asdf-vm-ui--file-backed))
  "Insert an Apply and Reset button into the object editor."
  (asdf-vm-message "override")
  (widget-create
   'push-button
   :notify (lambda (&rest _)
    		 (widget-apply eieio-wo :value-get)
    		 (eieio-done-customizing eieio-co)
             (kill-buffer))
   "Save and Quit")
  (widget-insert "   ")
  (widget-create
   'push-button
   :notify (lambda (&rest _)
             (asdf-vm-message "Saving file ...")
    		 (widget-apply eieio-wo :value-get)
    		 (eieio-done-customizing eieio-co)
    		 (asdf-vm-message "File saved"))
   "Save")
  (widget-insert "   ")
  (widget-create
   'push-button
   :notify (lambda (&rest _)
    		 (kill-buffer))
   "Cancel"))

(provide 'asdf-vm-ui)

;;; asdf-vm-ui.el ends here
