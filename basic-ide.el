;;; basic-ide.el --- c64 BASIC IDE

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 20 Feb 2020
;; Version: 0.0.1
;; Keywords: languages, basic
;; URL: https://github.com/emacs-php/poly-php
;; Package-Requires: ((emacs "25")(helm "3.6.0")  (basic-mode "0.4.2") (cl-lib "0.6")(company "1.21.4") (flycheck) )
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

;; This package integrate useful tools to manage and edit c64 basic projects

;;; Code:
(require 'cl-lib)
(require 'company)
(require 'helm)
(require 'flycheck)

(defgroup basic-ide nil
  "Drupal configuration."
  :prefix "basic-ide-"
  :group 'languages)

(define-minor-mode basic-ide-mode
  "Basic ide minor mode for editing an managing c64 basic files it uses flycheck, helm and company integration to provide IDE features."
  :lighter " Basic IDE"
  :group 'basic-ide

;;;###autoload
  (defcustom basic-ide-completion-backend (split-string "END    FOR     NEXT    DATA    INPUT#  INPUT   DIM     READ    LET     GOTO    RUN     IF      RESTORE GOSUB   RETURN  REM     STOP    ON      WAIT    LOAD    SAVE    VERIFY  DEF     POKE    PRINT#  PRINT   CONT    LIST    CLR     CMD     SYS     OPEN    CLOSE   GET     NEW     TAB(    TO      FN      SPC(    THEN    NOT     STEP    +       -       *       /       ^       AND     OR      >       =       <SGN    INT     ABS     USR     FRE     POS     SQR     RND     LOG     EXP     COS     SIN     TAN     ATN     PEEK    LEN     STR$    VAL     ASC     CHR$    LEFT$   RIGHT$  MID$    GO      ~" split-string-default-separators t)
    "Basic IDE keywords for completion."
    :group 'basic-ide)

;;;###autoload
  (defcustom basic-ide-cbmbasic-executable "/home/fermin/Programming/emfibasic/cbmbasic/cbmbasic"
    "Basic IDE cmbasic executable location"
    :group 'basic-ide)

  (defcustom basic-ide-x64-executable "/usr/bin/x64"
    "Basic IDE x64 VICE executable "
    :group 'basic-ide
    )

;;;###autoload
  (defcustom basic-ide-x64-kernal "/usr/lib/vice/C64/kernal"
    "Basic IDE x64 VICE kernal file "
    :group 'basic-ide
    )

;;;###autoload
  (defcustom basic-ide-vice-simon-disk "/home/fermin/Programming/emfibasic/simon.d64"
    "Basic IDE simon's basic disk location"
    :group 'basic-ide
    )

  (flycheck-define-checker basic
    "A syntax checker for the Bas 2.5 interpreter http://www.moria.de/~michael/bas/"
    :command ("bas" (eval (buffer-file-name)) )
    :error-patterns
    ((error line-start  "Error: " (message) "in line " line " at:" line-end))
    :modes basic-mode)

  (add-to-list 'flycheck-checkers 'basic)
  (flycheck-mode)


  (defun company-basic-ide-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'basic-ide-completion-backend))
      (prefix (and (eq major-mode 'basic-mode)
                   (company-grab-symbol)))
      (candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        basic-ide-completion-backend))))
  (setq company-backends '(company-basic-ide-backend))
  (company-mode)

  (defun basic-ide-eval-region ()
    "Evaluate the current selected region and output the resulto to a custom buffer."
    (interactive)
    (basic-ide-local-execute t "cbmbasic-region-output")
    )

  (defun basic-ide-interactive-execute ()
    "Evaluate the current buffer and open an interactive eshell buffer"
    (interactive)
    (setq file--name (buffer-file-name))
    (eshell)
    (with-current-buffer "*eshell*"
      (eshell-kill-input)
      (end-of-buffer)
      (insert (concat basic-ide-cbmbasic-executable " " file--name))
      (eshell-send-input)
      (end-of-buffer)
      (yank))
    )

  (defun basic-ide-local-execute (&optional use-region output-buffer-name)
    "Executa basic code locally wih cbmbasic https://github.com/mist64/cbmbasic"
    (interactive)
    (setq local-buffer-name (if output-buffer-name (format output-buffer-name) (format "cbmbasic-output")))
    ;; (setq selected-region basic-ide-selected-region)
    (setq command-output (shell-command-to-string (concat basic-ide-cbmbasic-executable " "
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
      (insert command-output)
      ))

  (defun basic-ide-vice-start-session ()
    "Start a vice session and open the emulator http://vice-emu.sourceforge.net/ "
    (interactive)
    (async-shell-command (concat  basic-ide-x64-executable " -remotemonitor -kernal " basic-ide-x64-kernal " ") nil nil)
    )

  (defun basic-ide-vice-execute ()
    "Basic IDE execute current buffer in vice emulator, it needs to be active with start-vice-session."
    (interactive)
    (shell-command-to-string (concat "petcat -wsimon -o /tmp/f.prg " (buffer-file-name)))
    (shell-command-to-string  "echo 'cl' | netcat -N  localhost 6510 ")
    (shell-command-to-string (concat "echo" " 'l \"/tmp/f.prg\" 0' | netcat -N localhost 6510"))
    )
  (defun basic-ide-vice-simon-basic ()
    "Basic IDE enables simon's basic commands to be executed inside VICE emulator"
    (interactive)
    (shell-command-to-string (concat  "echo " "'attach \"" basic-ide-vice-simon-disk "\" 8' | netcat -N localhost 6510 "))
    (shell-command-to-string   "echo 'load \"*\" 8' | netcat -N localhost 6510 ")
    )

  ;;   (defvar yasnippet-snippets-dir
  ;;   (expand-file-name
  ;;    "snippets"
  ;;    (file-name-directory
  ;;     ;; Copied from ‘f-this-file’ from f.el.
  ;;     (cond
  ;;      (load-in-progress load-file-name)
  ;;      ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
  ;;       byte-compile-current-file)
  ;;      (:else (buffer-file-name))))))

  ;; ;;;###autoload
  ;; (defun yasnippet-snippets-initialize ()
  ;;   "Load the `yasnippet-snippets' snippets directory."
  ;;   ;; NOTE: we add the symbol `yasnippet-snippets-dir' rather than its
  ;;   ;; value, so that yasnippet will automatically find the directory
  ;;   ;; after this package is updated (i.e., moves directory).
  ;;   (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
  ;;   (yas-load-directory yasnippet-snippets-dir t))


  ;; (defclass my-helm-class (helm-source-sync)
  ;;   ((candidates :initform 'basic-backend)))

  ;; (helm :sources (helm-make-source "test" 'my-helm-class)
  ;;       :buffer "*helm test*")
  )

;;;###autoload
(provide 'basic-ide)
;;; basic-ide.el ends here
