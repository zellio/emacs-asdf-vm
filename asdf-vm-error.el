;;; asdf-vm-error.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

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

;; Errors for `asdf-vm'.

;;; Code:

(define-error
 'asdf-vm-error
 "[asdf-vm] Base error"
 '(error))

(define-error
 'asdf-vm-argument-missing
 "[asdf-vm] Arguments missing from function call"
 '(asdf-vm-error wrong-number-of-arguments))

(define-error
 'asdf-vm-plugin-error
 "[asdf-vm] Base asdf-vm plugin error"
 '(asdf-vm-error))

(define-error
 'asdf-vm-plugin-unreadable-repository-file
 "[asdf-vm] Cannot read repository file"
 '(asdf-vm-plugin-error))

(define-error
 'asdf-vm-plugin-menu-error
 "[asdf-vm] Base asdf-vm plugin menu error"
 '(asdf-vm-plugin-error))

(define-error
 'asdf-vm-incorrect-mode-error
 "[asdf-vm] The current buffer is not an ASDF-VM menu."
 '(asdf-vm-plugin-menu-error))

(define-error
 'asdf-vm-plugin-menu-missing-url-error
 "[asdf-vm] Plugin entry has to url"
 '(asdf-vm-plugin-menu-error))

(define-error
 'asdf-vm-process-error
 "[asdf-vm] Base process error"
 '(asdf-vm-error))

(define-error
 'asdf-vm-no-exectuable-error
 "[asdf-vm] No executable found"
 '(asdf-vm-process-error))

(define-error
 'asdf-vm-exec-error
 "[asdf-vm] Process exited with error status"
 '(asdf-vm-process-error))

(define-error
 'asdf-vm-sentinel-error
 "[asdf-vm] Base sentinel error"
 '(asdf-vm-process-error))

(define-error
 'asdf-vm-sentinel-nonsense-process-status
 "[asdf-vm] Nonsense process status in sentinel"
 '(asdf-vm-sentinel-error))

(define-error
 'asdf-vm-sentinel-missing-process
 "[asdf-vm] Nonsense process status in sentinel"
 '(asdf-vm-sentinel-error))

(define-error
 'asdf-vm-sentinel-unknown-status
 "[asdf-vm] Unhanded process status in sentinel"
 '(asdf-vm-sentinel-error))

(define-error
 'asdf-vm-installer-error
 "[asdf-vm] Base installer error"
 '(asdf-vm-error))

(define-error
 'asdf-vm-installer-unsupported-system
 "[asdf-vm] Detected system is not currently supported"
 '(asdf-vm-installer-error))

(define-error
 'asdf-vm-installer-checksum-mismatch
 "[asdf-vm] Calculated and supplied checksum mismatch"
 '(asdf-vm-installer-error))

(provide 'asdf-vm-error)

;;; asdf-vm-error.el ends here
