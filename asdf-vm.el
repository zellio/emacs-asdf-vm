;;; asdf-vm.el --- ASDF-VM porcelain -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))
;; Homepage: https://github.com/zellio/emacs-asdf-vm
;; Keywords: tools asdf-vm asdf

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

;; See README.md for general package documentation and each sub-file for more
;; specific commentary.

;;; Code:

(defgroup asdf-vm nil
  "ASDF-VM porcelain configuration group."
  :prefix "asdf-vm-"
  :group 'tools)

(require 'asdf-vm-error)
(require 'asdf-vm-util)
(require 'asdf-vm-process)
(require 'asdf-vm-ui)
(require 'asdf-vm-config)
(require 'asdf-vm-installer)
(require 'asdf-vm-plugin)
(require 'asdf-vm-plugin-menu)
(require 'asdf-vm-core)
(require 'asdf-vm-tool-versions)
(require 'asdf-vm-mode)

(provide 'asdf-vm)

;;; asdf-vm.el ends here
