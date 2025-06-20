;;; asdf-vm-util.el --- ASDF-VM porcelain utility functions -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(defsubst asdf-vm-message (format-string &rest args)
  "Call `message' with \"[asdf-vm] \" prepended to FORMAT-STRING.

ARGS is the same as the function `message'."
  (apply #'message (concat "[asdf-vm] " format-string) args))

(defun asdf-vm--parse-skip-list-line (line &optional token-count keep-predicate skip-predicate)
  "Split LINE based on predicate funtions.

The value TOKEN-COUNT represents the maximum number of tokens to return.
NB. There may be fewer tokens returned.

The function KEEP-PREDICATE should return a true value for any character
you want to keep in a token.

The function SKIP-PREDICATE should return a true value for any character
you want to exclude from any token.

Roughly, this function will pull the longest run of values which match
KEEP-PREDICATE. Then drop those, and the longest run of characters that
match SKIP-PREDICATE after that. This process will continue until either
LINE is empyt, or TOKEN-COUNT tokens have been created.

NB. The final TOKEN may contain characters which match SKIP-PREDICATE as
it is just the remaining value of LINE."
  (let* ((blank-characters '(?  ?\C-i ?\C-j ?\C-m))
         (token-count (or token-count 3))
         (keep-predicate
          (or keep-predicate
              (lambda (character)
                (not (memq character blank-characters)))))
         (skip-predicate
          (or skip-predicate
              (lambda (character)
                (memq character blank-characters))))
         (skip-forward
          (lambda (line)
            (seq-drop-while skip-predicate (seq-drop-while keep-predicate line))))
         token tokens)
    (if (<= token-count 1)
        (list line)
      (while (not (string-empty-p line))
        (setq
         token (seq-take-while keep-predicate line)
         line (funcall skip-forward line))
        (push token tokens)
        (when (length= tokens (- token-count 1))
          (when (not (string-empty-p line))
            (push line tokens))
          (setq line "")))
      (reverse tokens))))

(defun asdf-vm--parse-skip-list (string &optional token-count keep-predicate skip-predicate)
  "Map `asdf-vm--parse-skip-list-line' over the lines of STRING.

The values TOKEN-COUNT, KEEP-PREDICATE, SKIP-PREDICATE are the same as
in the function `asdf-vm--parse-skip-list-line'."
  (seq-map
   (lambda (line)
     (asdf-vm--parse-skip-list-line line token-count keep-predicate skip-predicate))
   (string-lines string)))

(defun asdf-vm--format-skip-list (tokens-list &optional spacer-width)
  "Formats TOKEN-LIST into regular columns, padded by SPACER-WIDTH.

NB. This function assumes that TOKENS-LIST is a list of lists of equal
length, which contain only strings."
  (if (length< (car tokens-list) 2)
      (string-join (flatten-list tokens-list) "\n")
    (let* ((spacer-width (or spacer-width 2))
           (idx-list (number-sequence 0 (- (length (car tokens-list)) 2)))
           column-widths)
      (dolist (idx idx-list)
        (push
         (seq-max (seq-map (lambda (tokens) (length (nth idx tokens))) tokens-list))
         column-widths))
      (setq column-widths (reverse column-widths))
      (string-join
       (seq-map
        (lambda (tokens)
          (string-join
           (append
            (seq-map
             (lambda (idx)
               (string-pad (nth idx tokens) (+ spacer-width (nth idx column-widths))))
             idx-list)
            (last tokens))))
        tokens-list)
       "\n"))))

(provide 'asdf-vm-util)

;;; asdf-vm-util.el ends here
