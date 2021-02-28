;;; -*- lexical-binding: t -*-
;;; dired-explore.el --- MS Explorer-like incremental search for dired

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://zk-phi.gitub.io/
;; Version: 1.0.0

;;; Commentary:

;; Require this script
;;
;;   (require 'dired-explore)
;;
;; and bind keys.
;;
;;   (dolist (key '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
;;                  "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "1" "2"
;;                  "3" "4" "5" "6" "7" "8" "9" "0" "-"))
;;     (define-key dired-mode-map key 'dired-explore))
;;
;; Then in a dired buffer, you can move the cursor to the filename
;; "foobar" by typing "foob" for example.
;;
;;   -rw------- zk_phi 1000 2000-01-01 bar
;;   -rw------- zk_phi 1000 2000-01-01 foo
;;   -rw------- zk_phi 1000 2000-01-01 foobar
;;   -rw------- zk_phi 1000 2000-01-01 qux
;;
;; Since most key bindings are overwritten by `dired-explore', you may
;; need to bind some other keys to perform copy, delete, ... etc.
;;
;;   (define-key dired-mode-map (kbd "C-c c") 'dired-do-copy)
;;   (define-key dired-mode-map (kbd "C-c d") 'dired-do-delete)
;;   ...
;;
;; Otherwise, you can also use upcase keys if you don't want to
;; overwrite the default keybindings.
;;
;;   ;; Shift + alphanum
;;   (dolist (key '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
;;                  "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "!" "@"
;;                  "#" "$" "%" "^" "&" "*" "(" ")" "_"))
;;     (define-key dired-mode-map key 'dired-explore))
;;
;; dired-explore is by default prepared for QWERTY (US) layout. If you
;; use another layout, setup the variable `dired-explore-key-table'.

;;; Change Log:

;; 1.0.0 first release

;;; Code:

(defconst dired-explore-version "1.0.0")

(defgroup dired-explore nil
  "MS Explorer-like incremental search for dired"
  :group 'emacs)

(defcustom dired-explore-key-table
  '((?a  . "[Aa]") (?b  . "[Bb]") (?c . "[Cc]") (?d . "[Cd]") (?e . "[Ce]")
    (?f  . "[Ff]") (?g  . "[Gg]") (?h . "[Hh]") (?i . "[Ii]") (?j . "[Jj]")
    (?k  . "[Jk]") (?l  . "[Jl]") (?m . "[Mm]") (?n . "[Nn]") (?o . "[Oo]")
    (?p  . "[Pp]") (?q  . "[Qq]") (?r . "[Qr]") (?s . "[Qs]") (?t . "[Tt]")
    (?u  . "[Uu]") (?v  . "[Vv]") (?w . "[Ww]") (?x . "[Xx]") (?y . "[Xy]")
    (?z  . "[Xz]") (?A  . "[Aa]") (?B . "[Bb]") (?C . "[Cc]") (?D . "[Cd]")
    (?E  . "[Ce]") (?F  . "[Ff]") (?G . "[Gg]") (?H . "[Hh]") (?I . "[Ii]")
    (?J  . "[Jj]") (?K  . "[Jk]") (?L . "[Jl]") (?M . "[Mm]") (?N . "[Nn]")
    (?O  . "[Oo]") (?P  . "[Pp]") (?Q . "[Qq]") (?R . "[Qr]") (?S . "[Qs]")
    (?T  . "[Tt]") (?U  . "[Uu]") (?V . "[Vv]") (?W . "[Ww]") (?X . "[Xx]")
    (?Y  . "[Xy]") (?Z  . "[Xz]") (?! . "[1!]") (?@ . "[2@]") (?# . "[3#]")
    (?$  . "[4$]") (?%  . "[5%]") (?^ . "[6^]") (?& . "[7&]") (?* . "[8*]")
    (?\( . "[9(]") (?\) . "[0)]") (?1 . "[1!]") (?2 . "[2@]") (?3 . "[3#]")
    (?4  . "[4$]") (?5  . "[5%]") (?6 . "[6^]") (?7 . "[7&]") (?8 . "[8*]")
    (?9  . "[9(]") (?0  . "[0)]"))
  "Alist of KEY vs REGEXP. For example, (?! . \"[1!]\")
  represents that the key \"!\" searches either \"1\" or \"!\"."
  :type '(alist :key-type character :value-type string)
  :group 'dired-explore)

(defcustom dired-explore-default-key "[^0-9a-z]"
  "REGEXP for keys that are not specified in
  \"dired-explore-key-table\"."
  :type 'string
  :group 'dired-explore)

(defun dired-explore--beginning-of-filename-p (&optional point)
  (let ((change
         (save-excursion
           (beginning-of-line)
           (next-single-property-change (point) 'dired-filename nil
                                        (line-end-position)))))
    (= change (or point (point)))))

(defun dired-explore--search-filename-forward (regex)
  (let ((prev-pos (point))
        (case-fold-search t))
    (condition-case nil
        (progn
          (while (and (search-forward-regexp regex)
                      (not (dired-explore--beginning-of-filename-p (match-beginning 0)))))
          t)
      ;; not found
      (error (progn (goto-char prev-pos) nil)))))

(defvar dired-explore--key "")
(defun dired-explore ()
  "MS Explorer-like incremental search command."
  (interactive)
  (setq dired-explore--key
        (concat dired-explore--key
                (or (cdr (assoc last-command-event dired-explore-key-table))
                    dired-explore-default-key)))
  ;; jump back to the beginning of subdir
  (dired-next-subdir 0)
  (dired-next-line 2)
  ;; search forward
  (while (not (or (dired-explore--search-filename-forward dired-explore--key)
                  (string= "" (setq dired-explore--key
                                    (substring dired-explore--key 1))))))
  (when (string= "" dired-explore--key)
    (error "not found")))

(provide 'dired-explore)

;;; dired-explore.el ends here
