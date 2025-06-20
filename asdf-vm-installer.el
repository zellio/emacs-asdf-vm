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

;; Manage installation of ASDF-VM.
;;
;; The main entry-point is `asdf-vm-installer'. This method will install and
;; activate the requested version of ASDF-VM.
;;
;; Installation location is controlled by the ./configure like variables:
;;   - `asdf-vm-installer-prefix'
;;   - `asdf-vm-installer-exec-prefix'
;;   - `asdf-vm-installer-bin-dir'
;;   - `asdf-vm-installer-data-dir'
;;   - `asdf-vm-installer-src-dir'

;;; Code:

(require 'asdf-vm-error)
(require 'asdf-vm-config)
(require 'asdf-vm-util)
(require 'asdf-vm-process)

(defgroup asdf-vm-installer nil
  "ASDF-VM porcelain installer configuration group."
  :prefix "asdf-vm-installer"
  :group 'asdf-vm)

;; Paths

(defun asdf-vm-installer-prefix-default ()
  "Default function for generating `asdf-vm-installer-prefix'.

Returns the path to a directory named `asdf' under the variable
`no-littering-var-directory' if `no-littering' is installed, or under
the variable `user-emacs-directory' if `no-littering' is not installed."
  (if (and (featurep 'no-littering) (fboundp 'no-littering-expand-var-file-name))
      (progn
        (declare-function no-littering-expand-var-file-name "no-littering")
        (no-littering-expand-var-file-name "asdf"))
    (expand-file-name "asdf" user-emacs-directory)))

(defcustom asdf-vm-installer-prefix-default-function
  #'asdf-vm-installer-prefix-default
  "Function to generate `asdf-vm-installer-prefix'."
  :group 'asdf-vm-installer
  :type 'function)

(defcustom asdf-vm-installer-prefix
  (funcall asdf-vm-installer-prefix-default-function)
  "Installation PREFIX for `asdf-vm-installer'."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-exec-prefix asdf-vm-installer-prefix
  "Installation EXEC-PREFIX for `asdf-vm-installer'."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-bin-dir
  (expand-file-name "bin" asdf-vm-installer-exec-prefix)
  "Installation BINDIR for `asdf-vm-installer'."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-data-dir
  (expand-file-name "share" asdf-vm-installer-exec-prefix)
  "Read-only architecture-independent data."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-src-dir (expand-file-name "src" asdf-vm-installer-data-dir)
  "Source file storage directory for `asdf-vm-installer'."
  :group 'asdf-vm-installer
  :type 'string)

;; Executables

(defcustom asdf-vm-installer-git-executable (executable-find "git")
  "Path to git executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-arguments nil
  "Optional arguments passed to git on every execution."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "git argument")))

(defcustom asdf-vm-installer-md5sum-executable (executable-find "md5sum")
  "Path to md5sum executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-md5sum-arguments nil
  "Optional arguments passed to md5sum on every execution."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "md5sum argument")))

(defcustom asdf-vm-installer-tar-executable (executable-find "tar")
  "Path to tar executable used in ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-tar-arguments nil
  "Optional arguments passed to tar on every execution."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "tar argument")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom asdf-vm-installer-github-url "https://github.com/asdf-vm/asdf"
  "Source url for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-repo-url
  (format "%s.git" asdf-vm-installer-github-url)
  "Git repository url for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-system nil
  "Operating system for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type '(choice (const "linux") (const "darwin")))

(defcustom asdf-vm-installer-architecture nil
  "Hardware architecture for ASDF-VM installation."
  :group 'asdf-vm-installer
  :type '(choice (const "amd64") (const "arm64") (const "386")))

(defun asdf-vm-installer--guess-system ()
  "Attempt to identify the current operating system."
  (or asdf-vm-installer-system
      (pcase system-configuration
        ((pred (string-match-p "linux")) "linux")
        ((pred (string-match-p "darwin")) "darwin")
        (_ (signal 'asdf-vm-installer-unsupported-system (list system-configuration))))))

(defun asdf-vm-installer--guess-architecture ()
  "Attempt to identify the current hardware architecture."
  (or asdf-vm-installer-architecture
      (pcase system-configuration
        ((pred (string-match-p (rx (or "aarch64" "arm64")))) "arm64")
        ((pred (string-match-p (rx (or "x86_64" "arm64" "x64")))) "amd64")
        ((pred (string-match-p "386")) "386")
        (_ (signal 'asdf-vm-installer-unsupported-system (list system-configuration))))))

(defsubst asdf-vm-installer--git-ls-remote ()
  "Sub-process call to git ls-remote for version tag scraping."
  (asdf-vm-call
   :executable asdf-vm-installer-git-executable
   :executable-arguments asdf-vm-installer-git-arguments
   :command 'ls-remote
   :command-arguments
   `("--sort=v:refname" ,asdf-vm-installer-git-repo-url "refs/tags/v*")
   :output t))

(defsubst asdf-vm-installer--parse-ls-remote-line (line)
  "Strip hash and ref type from beginning of LINE."
  (string-trim-left line (rx string-start (one-or-more (not "v")) "v")))

(defconst asdf-vm-installer--minimum-supported-version "0.16.0"
  "Minimum supported version of asdf-vm.")

(defun asdf-vm-installer--version-filter (version)
  "Filter for git tag values.

Requires:
 - VERSION to be a valid version according to `version-list'
 - VERSION to be >= `asdf-vm-installer--minimum-supported-version'."
  (condition-case _
      (version<= asdf-vm-installer--minimum-supported-version version)
    (error nil)))

(defvar asdf-vm-installer--remote-version-list nil
  "Memoization var for `asdf-vm-installer-list-all'.")

;;;###autoload
(defun asdf-vm-installer-list-all (&optional interactive-call)
  "List all installable versions of ASDF-VM.

Memoized version of `asdf-vm-installer-list-all-internal'. See that
function for specifics.

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
  "Scrape and filter git tags from `asdf-vm-installer-git-repo-url'.

Values are filtered by `asdf-vm-installer--version-filter'."
  (let* ((ls-remote (asdf-vm-installer--git-ls-remote))
         (lines (string-lines ls-remote))
         (tags (seq-map #'asdf-vm-installer--parse-ls-remote-line lines)))
    (seq-filter #'asdf-vm-installer--version-filter tags)))

(defun asdf-vm-installer--remote-version-completing-read
    (&optional predicate require-match initial-input hist def inherit-input-method)
  "Completing read for installed ASDF-VM version.

PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
are as in `compelting-read'."
  (let* ((versions (asdf-vm-installer-list-all)))
    (completing-read
     "asdf-vm version: " versions predicate require-match
     initial-input hist def inherit-input-method)))

(defun asdf-vm-installer--package-name (version &optional system architecture)
  "Format ASDF-VM package name for VERSION.

If SYSTEM is nil use the value returned by `asdf-vm-installer--guess-system'.

If ARCHITECTURE is nil use the value returned by
`asdf-vm-installer--guess-architecture'."
  (let* ((system (or system (asdf-vm-installer--guess-system)))
         (architecture (or architecture (asdf-vm-installer--guess-architecture)))
         (components (list "asdf" (format "v%s" version) system (format "%s.tar.gz" architecture))))
    (string-join components "-")))

(defun asdf-vm-installer--package-url (version &optional system architecture)
  "Github url to download release VERSION of ASDF-VM.

If SYSTEM is nil use the value returned by `asdf-vm-installer--guess-system'.

If ARCHITECTURE is nil use the value returned by
`asdf-vm-installer--guess-architecture'."
  (let* ((components
          (list
           asdf-vm-installer-github-url
           "releases" "download"
           (format "v%s" version)
           (asdf-vm-installer--package-name version system architecture))))
    (string-join components "/")))


(defsubst asdf-vm-installer--md5sum (path)
  "Sub-process call to md5sum for checksum verification of PATH."
  (asdf-vm-call
   :executable asdf-vm-installer-md5sum-executable
   :executable-arguments asdf-vm-installer-md5sum-arguments
   :command-arguments (list path)
   :output t))

(defun asdf-vm-installer--valid-checksum-p (path &optional checksum-path)
  "Check if md5sum values of PATH matches value in CHECKSUM-PATH.

When CHECKSUM-PATH is nil it defaults to PATH with .md5 appended."
  (let* ((checksum-path (or checksum-path (format "%s.md5" path)))
         (md5sum (asdf-vm-installer--md5sum path))
         (calculated-checksum (car (split-string md5sum)))
         (provided-checksum
          (with-temp-buffer
            (insert-file-contents-literally checksum-path)
            (string-trim (buffer-substring-no-properties (point-min)(point-max))))))
    (string= calculated-checksum provided-checksum)))

;;;###autoload
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


(defsubst asdf-vm-installer--tar-extract (download-path installation-path &optional blocking)
  "Sub-process call to tar for extracting DOWNLOAD-PATH.

Extracts files to INSTALLATION-PATH.

BLOCKING is a pass-through for `asdf-vm-call' argument plist."
  (asdf-vm-call
   :executable asdf-vm-installer-tar-executable
   :executable-arguments asdf-vm-installer-tar-arguments
   :command-arguments
   (list
    "--extract"
    (format "--file=%s" download-path)
    (format "--directory=%s" installation-path))
   :blocking blocking))

;;;###autoload
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

;;;###autoload
(defun asdf-vm-installer-list (&optional interactive-call)
  "List installed versions of ASDF-VM.

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
  "List installed versions of ASDF-VM."
  (let* ((directories (directory-files asdf-vm-installer-src-dir t))
         (maybe-versions (seq-map #'file-name-nondirectory directories)))
    (seq-filter #'asdf-vm-installer--version-filter maybe-versions)))

(defun asdf-vm-installer--local-version-completing-read
    (&optional predicate require-match initial-input hist def inherit-input-method)
  "Completing read for installed ASDF-VM version.

PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
are as in `compelting-read'."
  (let* ((versions (asdf-vm-installer-list-internal)))
    (completing-read
     "asdf-vm version: " versions predicate require-match
     initial-input hist def inherit-input-method)))

;;;###autoload
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
