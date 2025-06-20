;;; asdf-vm-installer.el --- ASDF-VM self installation -*- lexical-binding: t -*-

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

(require 'asdf-vm-error)
(require 'asdf-vm-config)
(require 'asdf-vm-util)
(require 'asdf-vm-process)

(defgroup asdf-vm-installer nil
  "ASDF-VM porcelain installer configuration group."
  :prefix "asdf-vm-installer"
  :group 'asdf-vm)

(defun asdf-vm-installer-prefix-default ()
  ""
  (if (and (featurep 'no-littering) (fboundp 'no-littering-expand-var-file-name))
      (progn
        (declare-function no-littering-expand-var-file-name "no-littering")
        (no-littering-expand-var-file-name "asdf"))
    (expand-file-name "asdf" user-emacs-directory)))

(defcustom asdf-vm-installer-prefix (asdf-vm-installer-prefix-default)
  "Installation prefix for `asdf-vm-installer'."
  :type 'string
  :group 'asdf-vm-installer)

(defcustom asdf-vm-installer-data-root-dir (expand-file-name "share" asdf-vm-installer-prefix)
  "Installation bindir for `asdf-vm-installer'."
  :type 'string
  :group 'asdf-vm-installer)

(defcustom asdf-vm-installer-src-dir (expand-file-name "share" asdf-vm-installer-prefix)
  "Installation bindir for `asdf-vm-installer'."
  :type 'string
  :group 'asdf-vm-installer)

(defcustom asdf-vm-installer-exec-prefix asdf-vm-installer-prefix
  "Installation exec prefix for `asdf-vm-installer'."
  :type 'string
  :group 'asdf-vm-installer)

(defcustom asdf-vm-installer-bin-dir (expand-file-name "bin" asdf-vm-installer-exec-prefix)
  "Installation bindir for `asdf-vm-installer'."
  :type 'string
  :group 'asdf-vm-installer)

(defcustom asdf-vm-installer-git-executable (executable-find "git")
  "Path to git executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-arguments nil
  "Optional extra arguments to be passed to git execution on every call."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "git argument")))

(defcustom asdf-vm-installer-github-url "https://github.com/asdf-vm/asdf"
  "Source url for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-url (concat asdf-vm-installer-github-url ".git")
  "Source url for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-system nil
  "Operating system for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type '(choice (const "linux") (const "darwin")))

(defun asdf-vm-installer--guess-system ()
  "Attempt to identify the current operating system."
  (or asdf-vm-installer-system
      (pcase system-configuration
        ((pred (string-match-p "linux")) "linux")
        ((pred (string-match-p "darwin")) "darwin")
        (_ (signal 'asdf-vm-installer-unsupported-system (list system-configuration))))))

(defcustom asdf-vm-installer-architecture nil
  "Hardware architecture for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type '(choice (const "amd64") (const "arm64") (const "386")))

(defun asdf-vm-installer--guess-architecture ()
  "Attempt to identify the current hardware architecture."
  (or asdf-vm-installer-architecture
      (pcase system-configuration
        ((pred (string-match-p (rx (or "aarch64" "arm64")))) "arm64")
        ((pred (string-match-p (rx (or "x86_64" "arm64" "x64")))) "amd64")
        ((pred (string-match-p "386")) "386")
        (_ (signal 'asdf-vm-installer-unsupported-system (list system-configuration))))))

(defsubst asdf-vm-installer--git-ls-remote ()
  ""
  (asdf-vm-call
   :executable asdf-vm-installer-git-executable
   :executable-arguments asdf-vm-installer-git-arguments
   :command 'ls-remote
   :command-arguments
   `("--sort=v:refname" ,asdf-vm-installer-git-url "refs/tags/v*")
   :output t))

(defsubst asdf-vm-installer--parse-ls-remote-line (line)
  ""
  (string-trim-left line (rx string-start (one-or-more (not "v")) "v")))

(defconst asdf-vm-installer--minimum-supported-version "0.16.0"
  "Minimum supported version of asdf-vm.")

(defun asdf-vm-installer--version-filter (version)
  ""
  (condition-case _
      (version<= asdf-vm-installer--minimum-supported-version version)
    (error nil)))

(defvar asdf-vm-installer--remote-version-list nil
  "Memoization var for `asdf-vm-installer-list-all'.")

(defun asdf-vm-installer-list-all (&optional interactive-call)
  "List all installable versions of ASDF-VM.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)))
  (when (null asdf-vm-installer--remote-version-list)
    (setq
     asdf-vm-installer--remote-version-list (asdf-vm-installer-list-all-internal)))
  (when interactive-call
    (asdf-vm-message
     "remote versions: %s" (string-join asdf-vm-installer--remote-version-list ", ")))
  asdf-vm-installer--remote-version-list)

(defun asdf-vm-installer-list-all-internal ()
  ""
  (let* ((ls-remote (asdf-vm-installer--git-ls-remote))
         (lines (string-lines ls-remote))
         (tags (seq-map #'asdf-vm-installer--parse-ls-remote-line lines)))
    (seq-filter #'asdf-vm-installer--version-filter tags)))

(defun asdf-vm-installer--remote-version-completing-read (&optional require-match)
  "Completing read for installed ASDF-VM version.

REQUIRE-MATCH value is as in `completing-read'."
  (let* ((versions (asdf-vm-installer-list-all)))
    (completing-read "asdf-vm version: " versions nil require-match)))

(defun asdf-vm-installer--package-name (version &optional system architecture)
  ""
  (let* ((system (or system (asdf-vm-installer--guess-system)))
         (architecture (or architecture (asdf-vm-installer--guess-architecture)))
         (components (list "asdf" (format "v%s" version) system (format "%s.tar.gz" architecture))))
    (string-join components "-")))

(defun asdf-vm-installer--package-url (version &optional system architecture)
  ""
  (let* ((components
          (list
           asdf-vm-installer-github-url
           "releases" "download"
           (format "v%s" version)
           (asdf-vm-installer--package-name version system architecture))))
    (string-join components "/")))

(defcustom asdf-vm-installer-md5sum-executable (executable-find "md5sum")
  "Path to md5sum executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-md5sum-arguments nil
  "Optional extra arguments to be passed to md5sum execution on every call."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "md5sum argument")))

(defsubst asdf-vm-installer--md5sum (path)
  ""
  (asdf-vm-call
   :executable asdf-vm-installer-md5sum-executable
   :executable-arguments asdf-vm-installer-md5sum-arguments
   :command-arguments (list path)
   :output t))

(defun asdf-vm-installer--valid-checksum-p (path &optional checksum-path)
  ""
  (let* ((checksum-path (or checksum-path (format "%s.md5" path)))
         (md5sum (asdf-vm-installer--md5sum path))
         (calculated-checksum (car (split-string md5sum)))
         (provided-checksum
          (with-temp-buffer
            (insert-file-contents-literally checksum-path)
            (string-trim (buffer-substring-no-properties (point-min)(point-max))))))
    (string= calculated-checksum provided-checksum)))

(defun asdf-vm-installer-download (version &optional interactive-call)
  "Download source code or binary for the specified VERSION.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-installer--remote-version-completing-read)
    (prefix-numeric-value current-prefix-arg)))
  (let* ((version-src-dir (expand-file-name version asdf-vm-installer-src-dir))
         (package-name (asdf-vm-installer--package-name version))
         (package-url (asdf-vm-installer--package-url version))
         (package-path (expand-file-name package-name version-src-dir))
         (checksum-name (format "%s.md5" package-name))
         (checksum-url (format "%s.md5" package-url))
         (checksum-path (expand-file-name checksum-name version-src-dir)))
    (unless (file-directory-p version-src-dir)
      (make-directory version-src-dir t))
    (url-copy-file package-url package-path t)
    (url-copy-file checksum-url checksum-path t)
    (unless (asdf-vm-installer--valid-checksum-p package-path checksum-path)
      (when (file-readable-p package-path)
        (delete-file package-path))
      (when (file-readable-p checksum-path)
        (delete-file checksum-path))
      (signal 'asdf-vm-installer-checksum-mismatch nil)))
  (when interactive-call
    (asdf-vm-message "version %s downloaded" version)))

(defcustom asdf-vm-installer-tar-executable (executable-find "tar")
  "Path to tar executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-tar-arguments nil
  "Optional extra arguments to be passed to tar execution on every call."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "tar argument")))

(defsubst asdf-vm-installer--tar-extract (download-path installation-path &optional blocking)
  ""
  (asdf-vm-call
   :executable asdf-vm-installer-tar-executable
   :executable-arguments asdf-vm-installer-tar-arguments
   :command-arguments
   (list
    "--extract"
    (format "--file=%s" download-path)
    (format "--directory=%s" installation-path))
   :blocking blocking))

(defun asdf-vm-installer-install (version &optional keep-downloads interactive-call)
  "Install the specified VERSION.

When KEEP-DOWNLOADS is set, do not delete the downloaded source files,
this can be set interactively by setting a prefix value.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-installer--remote-version-completing-read)
    current-prefix-arg
    (prefix-numeric-value current-prefix-arg)))
  (let* ((version-src-dir (expand-file-name version asdf-vm-installer-src-dir))
         (package-name (asdf-vm-installer--package-name version))
         (package-path (expand-file-name package-name version-src-dir)))
    (unless (file-readable-p package-path)
      (asdf-vm-installer-download version interactive-call))
    (unless keep-downloads
      (add-hook 'kill-emacs-hook
                (lambda ()
                  (when (file-readable-p package-path)
                    (delete-file package-path))
                  (when-let* ((checkusm-path (format "%s.md5" package-path))
                              (file-readable-p checkusm-path))
                    (delete-file checkusm-path)))))
    (asdf-vm-installer--tar-extract package-path version-src-dir interactive-call))
  (when interactive-call
    (asdf-vm-message "version %s installed" version)))

(defun asdf-vm-installer-list (&optional interactive-call)
  "List all insalled versions of ASDF-VM.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)))
  (let ((versions (asdf-vm-installer-list-internal)))
    (when interactive-call
      (asdf-vm-message
       "installed versions: %s" (string-join versions ", ")))
    versions))

(defun asdf-vm-installer-list-internal ()
  ""
  (let* ((directories (directory-files asdf-vm-installer-src-dir t))
         (maybe-versions (seq-map #'file-name-nondirectory directories)))
    (seq-filter #'asdf-vm-installer--version-filter maybe-versions)))

(defun asdf-vm-installer--local-version-completing-read (&optional require-match)
  "Completing read for installed ASDF-VM version.

REQUIRE-MATCH value is as in `completing-read'."
  (let* ((versions (asdf-vm-installer-list-internal)))
    (completing-read "asdf-vm version: " versions nil require-match)))

(defun asdf-vm-installer-activate (version &optional interactive-call)
  "Activate installed VERSION of ASDF-VM.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-installer--local-version-completing-read)
    (prefix-numeric-value current-prefix-arg)))
  (let* ((version-src-dir (expand-file-name version asdf-vm-installer-src-dir)))
    (unless (file-directory-p asdf-vm-installer-bin-dir)
      (make-directory asdf-vm-installer-bin-dir t))
    (make-symbolic-link
     (expand-file-name "asdf" version-src-dir)
     (expand-file-name "asdf" asdf-vm-installer-bin-dir)
     t))
  (when interactive-call
    (asdf-vm-message "version %s activated" version)))

(defun asdf-vm-installer--installed-p (version)
  "Check if VERSION of ASDF-VM is installed."
  (let* ((version-src-dir (expand-file-name version asdf-vm-installer-src-dir)))
    (and (file-directory-p version-src-dir)
         (file-executable-p (expand-file-name "asdf" version-src-dir)))))

;;;###autoload
(defun asdf-vm-installer (version &optional keep-downloads interactive-call)
  "Install and activate VERSION of ASDF-VM.

When KEEP-DOWNLOADS is set, do not delete the downloaded source files,
this can be set interactively by setting a prefix value.

Sets `asdf-vm-process-executable' to the installed VERSION.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-installer--remote-version-completing-read)
    current-prefix-arg
    (prefix-numeric-value current-prefix-arg)))
  (unless (asdf-vm-installer--installed-p version)
    (asdf-vm-installer-install version keep-downloads interactive-call))
  (asdf-vm-installer-activate version interactive-call)
  (setq
   asdf-vm-process-executable
   (expand-file-name "asdf" asdf-vm-installer-bin-dir)))

(provide 'asdf-vm-installer)

;;; asdf-vm-installer.el ends here
