;;; asdf-vm-process.el --- ASDF VM porcelain for Emacs -*- lexical-binding: t -*-

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

(require 'asdf-vm-util)

(defgroup asdf-vm-process nil
  "ASDF-VM async inferior process configuration group."
  :prefix "asdf-vm-process-"
  :group 'asdf-vm
  :group 'processes)

(defcustom asdf-vm-process-executable "asdf"
  "Path to ASDF-VM command line tool."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-executable-arguments nil
  "ASDF-VM command line tool execution arguments.

These values will be passed to every invocation of asdf before any
command or command arguments."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-buffer-name "*asdf-vm*"
  "Host buffer name for `asdf-vm-process' queue."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-stderr-buffer-name "*asdf-vm-stderr*"
  "Host buffer name for ASDF-VM process stderr."
  :type 'string
  :group 'asdf-vm-process)

(defvar asdf-vm-process--call-queue nil
  "List of waiting async ASDF-VM process calls.

These should be ultimately processed by the mutually recursive
`asdf-vm-process--sentinel' and `asdf-vm-call' functions.")

(defun asdf-vm-process--sentinel (process event)
  "Sentinel function used by the `asdf-vm-process--make-process' function.

Reads the state of PROCESS and when execution has completed will check
`asdf-vm-process--call-queue' for async work which is waiting to be
called.

The EVENT value is only ever used for error reporting."
  (let* ((status (process-status process)) (signal-data (list status event)))
    (pcase status
      ((or 'run 'stop))
      ((or 'signal 'open 'closed 'connect 'failed 'listen)
       (signal 'asdf-vm-sentinel-nonsense-process-status signal-data))
      ((pred null)
       (signal 'asdf-vm-sentinel-missing-process signal-data))
      ('exit
       (if asdf-vm-process--call-queue
           (progn
             (asdf-vm-message "Execution complete, processing call queue")
             (let ((plist (pop asdf-vm-process--call-queue)))
               (apply #'asdf-vm-call plist)))
         (asdf-vm-message "Execution complete")))
      (_
       (signal 'asdf-vm-sentinel-unknown-status signal-data)))))

(defsubst asdf-vm-process--format-name (name name-prefix command)
  "Format human readable process name from NAME, NAME-PREFIX, and COMMAND.

Internal function extracted to clean up `asdf-vm--make-process'."
  (cond (name name)
        (command (format "%s[%s]" name-prefix command))
        (t name-prefix)))

(defun asdf-vm-process--make-process (&rest plist)
  "Wrapper function for `make-process'.

PLIST is defined as in `asdf-vm-call' with the exception that
`asdf-vm-process--make-process' only understands :executable,
:executable-arguments, :command, :command-arguments, :directory,
:buffer-name, :name-prefix, and :name.

Given how much munging occurs in `asdf-vm-call' this function should
never be called directly."
  (let* ((executable (plist-get plist :executable))
         (executable-arguments (plist-get plist :executable-arguments))
         (command (plist-get plist :command))
         (command-arguments (plist-get plist :command-arguments))
         (directory (plist-get plist :directory))
         (buffer (get-buffer-create (plist-get plist :buffer-name)))
         (name-prefix (or (plist-get plist :name-prefix) (file-name-base executable)))
         (name (asdf-vm-process--format-name (plist-get plist :name) name-prefix command))
         (command-list
          (and command (seq-map #'symbol-name (if (atom command) (list command) command)))))
    (with-current-buffer buffer
      (let* ((default-directory directory))
        (make-process
         :name name
         :buffer buffer
         :sentinel #'asdf-vm-process--sentinel
         :command `(,executable
                    ,@executable-arguments
                    ,@command-list
                    ,@command-arguments)
         :stderr asdf-vm-process-stderr-buffer-name)))))

(defun asdf-vm-process--buffer-process-running-p (buffer-name)
  "Check if there is a running process connected to BUFFER-NAME."
  (and-let* ((buffer (get-buffer buffer-name))
             (process (get-buffer-process buffer)))
    (eq 'run (process-status process))))

(defconst asdf-vm-process--make-process-keys
  '(:name :name-prefix
          :executable :executable-arguments
          :command :command-arguments
          :directory :buffer-name)
  "List of plist keywords understood by `asdf-vm-process--make-process'.")

(defun asdf-vm--make-process-defaults ()
  "Default values for an `asdf-vm--make-process' argument list."
  (let ((buffer-parent-directory
         (and-let* ((buffer-file (buffer-file-name (current-buffer))))
           (file-name-parent-directory buffer-file))))
    (list
     :executable asdf-vm-process-executable
     :executable-arguments asdf-vm-process-executable-arguments
     :directory (or buffer-parent-directory default-directory))))

(defun asdf-vm--call-args (plist buffer-name)
  "Build `asdf-vm-call' arguments from PLIST and BUFFER-NAME."
  (let* ((call-args (plist-put
                     (asdf-vm--make-process-defaults)
                     :buffer-name buffer-name)))
    (dolist (key asdf-vm-process--make-process-keys)
      (when (plist-member plist key)
        (setq call-args (plist-put call-args key (plist-get plist key)))))
    call-args))

(defun asdf-vm--async-call (&rest plist)
  "Asynchronous process handler for `asdf-vm-call'.

See `asdf-vm-call'. for description of PLIST."
  (let* ((call-args (asdf-vm--call-args plist asdf-vm-process-buffer-name)))
    (if (asdf-vm-process--buffer-process-running-p asdf-vm-process-buffer-name)
        (setq asdf-vm-process--call-queue
              (append asdf-vm-process--call-queue (list call-args)))
      (apply #'asdf-vm-process--make-process call-args))))

(defun asdf-vm--sync-call (&rest plist)
  "Synchronous process handler for `asdf-vm-call'.

See `asdf-vm-call'. for description of PLIST."
  (let* ((call-args (asdf-vm--call-args plist asdf-vm-process-buffer-name))
         (executable (plist-get call-args :executable))
         (executable-arguments (plist-get call-args :executable-arguments))
         (command (plist-get call-args :command))
         (command-arguments (plist-get call-args :command-arguments))
         (default-directory (plist-get call-args :directory))
         (command-list
          (and command (seq-map #'symbol-name (if (atom command) (list command) command)))))
    (with-temp-buffer
      (apply
       #'call-process
       executable
       nil (current-buffer) nil
       `(,@executable-arguments
         ,@command-list
         ,@command-arguments))
      (when (plist-get plist :output)
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun asdf-vm-call (&rest plist)
  "Start or enqueue an ASDF-VM sub-process.

Returns the process or result of execution.

This is similar to `make-process' though the arguments carry special
meaning and defaults.

The keyword values for the PLIST are as follows:

:name NAME -- NAME is name for the process. When not provided a name
will be constructed from :name-prefix and :command.

:name-prefix NAME-PREFIX -- NAME-PREFIX is used in the construction of
NAME. It defaults to the `file-name-base' of :executable.

:executable EXECUTABLE -- EXECUTABLE is the path to the executable
program to be run in the sub-process. The values defaults to
`asdf-vm-process-executable'.

:executable-arguments EXECUTABLE-ARGUMENTS -- EXECUTABLE-ARGUMENTS is a
list of strings to be passed directly after :executable and before
:command in sub-process execution. This value defaults to
`asdf-vm-process-executable-arguments'.

:command COMMAND -- COMMAND a symbol or list of symbols representing the
sub-command of EXECUTABLE and is the first argument passed after
:executable-arguments.

:command-arguments COMMAND-ARGUMENTS -- COMMAND-ARGUMENTS is a list of
strings which is the argument values passed after :command during
sub-process execution.

:directory DIRECTORY -- DIRECTORY is the directory path in which the
sub-process is spawned. This value will default either to the parent of
the current buffer or, if there is not one, to `default-directory'.

:buffer-name BUFFER-NAME -- BUFFER-NAME is the name of the buffer which
hosts the spawned sub-process. This value defaults based on the kind of
execution occurring. For asynchronous called the value is
`asdf-vm-process-buffer-name' and for synchronous calls it is
`asdf-vm-process-output-buffer-name'.

:output OUTPUT -- OUTPUT is a boolean flag indicating that the
`asdf-vm-call' process should both block on execution and return the
string result from sub-process execution.

:blocking BLOCKING -- BLOCKING is a boolean flag indicate that the
`asdf-vm-call' should be made synchronously.

:success-codes SUCCESS-CODES -- SUCCESS-CODES is a list of integer
values between 1 and 255 which indicate sub-process execution success.
This list always has 0 pushed to the front.

When neither :blocking, nor :output is set to true, execution will be
enqueued for asynchronous execution. This execution starts immediately if
the queue is empty, otherwise it is kicked off immediately after the
completion of the previous asdf sub-process by `asdf-vm-process--sentinel'"
  (let* ((handler
          (if (or (plist-member plist :output) (plist-member plist :blocking))
              #'asdf-vm--sync-call #'asdf-vm--async-call)))
    (apply handler plist)))

(provide 'asdf-vm-process)

;;; asdf-vm-process.el ends here
