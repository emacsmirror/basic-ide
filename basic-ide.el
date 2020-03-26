;;; basic-ide.el --- BASIC IDE c64                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 20 Feb 2020
;; Version: 0.0.1
;; Keywords: languages, basic
;; URL: https://gitlab.com/sasanidas/emacs-c64-basic-ide
;; Package-Requires: ((emacs "25")(helm "3.6.0")  (basic-mode "0.4.2") (cl-lib "0.6")(company "1.21.4") (flycheck "31") )
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
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

;; This is a minor mode for Emacs, that provide some IDE features for the basic-mode
;; , it has flycheck,company and helm support, it also provide an easy way to integrate Simon’s BASIC.
;; Provide 3 interpreters cbmbasic,Bas 2.5 and of course VICE.
;; For the moment, it’s focus on the c64 support.

;;; Code:
(require 'cl-lib)
(require 'company)
(require 'helm)
(require 'flycheck)


;; ----------------------------------------------------------------------------
;; BASIC minor mode:
;; ----------------------------------------------------------------------------
(defgroup basic-ide nil
  "Drupal configuration."
  :prefix "basic-ide-"
  :group 'languages)

(define-minor-mode basic-ide-mode
  "Basic ide minor mode for editing an managing c64 basic files it uses flycheck, helm and company integration to provide IDE features."
  :lighter " Basic IDE"
  :group 'basic-ide

  ;; ----------------------------------------------------------------------------
  ;; Customization:
  ;; ----------------------------------------------------------------------------

;;;###autoload
  (defcustom basic-ide-completion-backend '( "as" "call" "def" "defbol" "defdbl" "defint" "defsng" "defstr"
				    "dim" "do" "else" "elseif" "end" "endif" "error" "exit" "fn"
				    "for" "gosub" "goto" "if" "loop" "next" "on" "step" "repeat"
				    "return" "sub" "then" "to" "until" "wend" "while" "and" "cls" "data" "input" "let" "mat" "mod" "not" "or"
				    "peek" "poke" "print" "read" "restore" "troff" "tron" "xor" "abs" "asc" "atn" "cdbl" "cint" "chr$" "command$" "cos" "exp"
				    "fix" "instr" "int" "lcase$" "len" "left$" "log" "log10" "mid$"
				    "pi" "right$" "rnd" "sgn" "sin" "sqr" "str$" "tab" "tan"
				    "ucase$" "usr" "val" "false" "true")
    "Basic IDE keywords for completion."
    :group 'basic-ide)

;;;###autoload
  (defcustom basic-ide-cbmbasic-executable "cbmbasic"
    "Basic IDE cmbasic executable location"
    :type 'string
    :group 'basic-ide)

;;;###autoload
  (defcustom basic-ide-x64-executable "/usr/bin/x64"
    "Basic IDE x64 VICE executable "
    :type 'string
    :group 'basic-ide)

;;;###autoload
  (defcustom basic-ide-x64-kernal "/usr/lib/vice/C64/kernal"
    "Basic IDE x64 VICE kernal file "
    :type 'string
    :group 'basic-ide)

;;;###autoload
  (defcustom basic-ide-vice-simon-disk ""
    "Basic IDE simon's basic disk location"
    :type 'string
    :group 'basic-ide)


  ;; ----------------------------------------------------------------------------
  ;; Flycheck checker:
  ;; ----------------------------------------------------------------------------

  (flycheck-define-checker basic
    "A syntax checker for the Bas 2.5 interpreter http://www.moria.de/~michael/bas/"
    :command ("bas" (eval (buffer-file-name)) )
    :error-patterns
    ((error line-start  "Error: " (message) "in line " line " at:" line-end))
    :modes basic-mode)

  (add-to-list 'flycheck-checkers 'basic)
  (flycheck-mode)


  ;; ----------------------------------------------------------------------------
  ;; Company mode:
  ;; ----------------------------------------------------------------------------

  (defun basic-ide-company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'basic-ide-completion-backend))
      (prefix (and (eq major-mode 'basic-mode)
                   (company-grab-symbol)))
      (candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        basic-ide-completion-backend))))
  (setq company-backends '(basic-ide-company-backend))
  (company-mode)

  ;; ----------------------------------------------------------------------------
  ;; Common functions:
  ;; ----------------------------------------------------------------------------
 

  (defun basic-ide-eval-region ()
    "Evaluate the current selected region and output the resulto to a custom buffer."
    (interactive)
    (basic-ide-local-execute t "cbmbasic-region-output"))

  (defun basic-ide-interactive-execute ()
    "Evaluate the current buffer and open an interactive eshell buffer with cbmbasic"
    (interactive)
    (setq file--name (buffer-file-name))
    (eshell)
    (with-current-buffer "*eshell*"
      (eshell-kill-input)
      (end-of-buffer)
      (insert (concat basic-ide-cbmbasic-executable " " file--name))
      (eshell-send-input)
      (end-of-buffer)
      (yank)))

  (defun basic-ide-local-execute (&optional use-region output-buffer-name)
    "Execute basic code locally wih cbmbasic https://github.com/mist64/cbmbasic"
    (interactive)
    (setq local-buffer-name (if output-buffer-name (format output-buffer-name) (format "cbmbasic-output")))
    ;; (setq selected-region basic-ide-selected-region)
    (setq command-output
	  (shell-command-to-string
	   (concat basic-ide-cbmbasic-executable " "
		   (if use-region
		       (progn (write-region
			       (progn
				 (if (use-region-p)
				     (setq pos1 (region-beginning) pos2 (region-end))
				   (progn
				     ((setq )etq bds (bounds-of-thing-at-point 'symbol))
				     (setq (point)os1 (car bds) pos2 (cdr bds))))
				 (format "%s" (filter-buffer-substring pos1 pos2)))
			       nil "/tmp/basic_ide_region")
			      (format "/tmp/basic_ide_region"))
		     (buffer-file-name) ))))
    (with-current-buffer
	(get-buffer-create
         local-buffer-name)
      (erase-buffer)
      (insert command-output)))

  ;; ----------------------------------------------------------------------------
  ;; Vice functions:
  ;; ----------------------------------------------------------------------------

  (defun basic-ide-vice-start-session ()
    "Start a vice session and open the emulator http://vice-emu.sourceforge.net/ "
    (interactive)
    (async-shell-command (concat  basic-ide-x64-executable " -remotemonitor -kernal " basic-ide-x64-kernal " ") nil nil))

  (defun basic-ide-vice-execute ()
    "Basic IDE execute current buffer in vice emulator, it needs to be active with start-vice-session."
    (interactive)
    (shell-command-to-string (concat "petcat -wsimon -o /tmp/f.prg " (buffer-file-name)))
    (shell-command-to-string  "echo 'cl' | netcat -N  localhost 6510 ")
    (shell-command-to-string (concat "echo" " 'l \"/tmp/f.prg\" 0' | netcat -N localhost 6510")))
  (defun basic-ide-vice-load-simon-basic ()
    "Basic IDE enables simon's basic commands to be executed inside VICE emulator"
    (interactive)
    (shell-command-to-string (concat  "echo " "'attach \"" basic-ide-vice-simon-disk "\" 8' | netcat -N localhost 6510 "))
    (shell-command-to-string   "echo 'load \"*\" 8' | netcat -N localhost 6510 "))
  (defun basic-ide-vice-reset ()
    "Basic IDE restart the current VICE session"
    (interactive)
    (shell-command-to-string  "echo 'reset 0' | netcat -N localhost 6510 ")))

(provide 'basic-ide)
;;; basic-ide.el ends here

