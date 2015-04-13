;;; fcitx-emacs.el --- Make fcitx better in Emacs

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; #+TITLE: fcitx-emacs
;; Make [[https://github.com/fcitx/fcitx/][fcitx]] better in Emacs.

;; This package provides a set of functions to make fcitx work better in Emacs.

;; * Setup
;;   : (add-to-list 'load-path "/path/to/fcitx-emacs.el")
;;   : (require 'fcitx-emacs)

;;   Recommended though optional:
;;   : M-x fcitx-emacs-default-setup

;;   Calling =fcitx-emacs-default-setup= will enable all the features that this
;;   package provides and use default settings. See the following sections for
;;   details.

;;   Note that for every feature, there are both =*-turn-on= and =*-turn-off=
;;   functions defined, which can enable and disable the corresponding feature,
;;   respectively.

;; * Disable Fcitx by Prefix Keys
;;   If you've enabled fcitx, then you can't easily change your buffer by "C-x b"
;;   because the second key "b" will be blocked by fcitx(and you need to press
;;   "enter" in order to send "b" to emacs). This package provides a way
;;   to define the prefix keys after which you can temporarily disable fcitx.
  
;;   For example, you want to temporarily disable fcitx after you press "C-x" so
;;   that you can directly type "b" after "C-x" and after you press "b", fcitx will
;;   be activated again so you can still type Chinese buffer name. To define "C-x"
;;   to be the prefix key that can temporarily disable fcitx:
;;   : (fcitx-emacs-prefix-keys-add "C-x")

;;   Usually, defining "C-x" and "C-c" to be such prefix keys is enough for most
;;   users. You can simply use following command:
;;   : (fcitx-emacs-prefix-keys-setup)
;;   to add "C-x" and "C-c".

;;   After defining prefix keys, you need to call 
;;   : (fcitx-emacs-prefix-keys-turn-on)
;;   to enable this feature.

;;   Of course, you can use
;;   : (fcitx-emacs-prefix-keys-turn-off)
;;   to disable this feature.

;;   Note if you use =M-x fcitx-emacs-default-setup=, then it already does all the
;;   above things, i.e. adding "C-x" and "C-c" to be prefix keys and enabling this
;;   feature, for you.

;; * Evil Support
;;   To disable fcitx when you exiting "insert mode" and enable fcitx after
;;   entering "insert mode" if originally you enable it in "insert mode":
;;   : (fcitx-emacs-evil-turn-on)

;;   It currently only works for "entering" and "exiting" the insert state for a
;;   certain buffer and it didn't have a separate fcitx state for each
;;   buffer(although it behaves like this under certain conditions).

;;   Similarly, =M-x fcitx-emacs-default-setup= enables this feature.

;; * =M-x=, =M-!=, =M-&= and =M-:= Support
;;   Usually you don't want to type Chinese when you use =M-x=, =M-!=
;;   (=shell-command=), =M-&= (=async-shell-command=) or =M-:= (=eval-expression=).
;;   You can use:
;;   : (fcitx-emacs-M-x-turn-on)
;;   : (fcitx-emacs-shell-command-turn-on)
;;   : (fcitx-emacs-eval-expression-turn-on)
;;   to disable fcitx temporarily in these commands.

;;   =M-x= should work with the original =M-x= (=execute-extended-command=), =smex=
;;   and =helm-M-x=.

;;   Again, =M-x fcitx-emacs-default-setup= enables all these three features.

;; * TODO TODO
;;   - Better Evil support
;;   - Add =key-chord= support

;;   For more features, pull requests are always welcome!

;;; Code:

(defvar fcitx-emacs-prefix-keys-polling-time 0.1
  "Time interval to execute prefix keys polling function.")

(defvar fcitx-emacs--prefix-keys-sequence nil
  "Prefix keys that can trigger disabling fcitx.")

(defvar fcitx-emacs--prefix-keys-timer nil
  "Timer for prefix keys polling function.")

(defun fcitx-emacs--check-fcitx ()
  (unless (executable-find "fcitx-remote")
    (error "`fcitx-remote' is not avaiable. Please check your
 fcitx installtion.")))

(defun fcitx-emacs--fcitx-activate ()
  (call-process-shell-command "fcitx-remote -o"))

(defun fcitx-emacs--fcitx-deactivate ()
  (call-process-shell-command "fcitx-remote -c"))

(defun fcitx-emacs--fcitx-is-active ()
  (char-equal
   (aref (shell-command-to-string "fcitx-remote") 0) ?2))

(defmacro fcitx-emacs--defun-maybe (prefix)
  (let ((var-symbol (intern
                     (concat "fcitx-emacs--"
                             prefix
                             "-disabled-by-elisp")))
        (deactivate-symbol (intern
                            (concat "fcitx-emacs--"
                                    prefix
                                    "-maybe-deactivate")))
        (activate-symbol (intern
                          (concat "fcitx-emacs--"
                                  prefix
                                  "-maybe-activate"))))
    `(progn
       (defvar ,var-symbol nil)
       (defun ,deactivate-symbol ()
         (when (fcitx-emacs--fcitx-is-active)
           (fcitx-emacs--fcitx-deactivate)
           (setq ,var-symbol t)))
       (defun ,activate-symbol ()
         (when ,var-symbol
           (fcitx-emacs--fcitx-activate)
           (setq ,var-symbol))))))

;; ------------------- ;;
;; prefix keys support ;;
;; ------------------- ;;
(fcitx-emacs--defun-maybe "prefix-keys")

(defun fcitx-emacs--prefix-keys-polling-function ()
  "Polling function executed every `fcitx-emacs-prefix-keys-polling-time'."
  (let ((key-seq (this-single-command-keys)))
    (cond
     ((member key-seq fcitx-emacs--prefix-keys-sequence)
      (fcitx-emacs--prefix-keys-maybe-deactivate))
     ((equal (this-command-keys-vector) [])
      (fcitx-emacs--prefix-keys-maybe-activate)))))

;;;###autoload
(defun fcitx-emacs-prefix-keys-add (prefix-key)
  (interactive)
  (push (vconcat (read-kbd-macro prefix-key))
        fcitx-emacs--prefix-keys-sequence))

;;;###autoload
(defun fcitx-emacs-prefix-keys-turn-on ()
  "Turn on `fcixt-disable-prefix-keys'."
  (interactive)
  (unless fcitx-emacs--prefix-keys-timer
    (setq fcitx-emacs--prefix-keys-timer
          (run-at-time t fcitx-emacs-prefix-keys-polling-time
                       #'fcitx-emacs--prefix-keys-polling-function))))

;;;###autoload
(defun fcitx-emacs-prefix-keys-turn-off ()
  "Turn off `fcixt-disable-prefix-keys'."
  (interactive)
  (when fcitx-emacs--prefix-keys-timer
    (cancel-timer fcitx-emacs--prefix-keys-timer)
    (setq fcitx-emacs--prefix-keys-timer)))

;;;###autoload
(defun fcitx-emacs-prefix-keys-setup ()
  (interactive)
  (fcitx-emacs-prefix-keys-add "C-x")
  (fcitx-emacs-prefix-keys-add "C-c"))

;; ------------ ;;
;; evil support ;;
;; ------------ ;;
(fcitx-emacs--defun-maybe "evil-insert")
(make-variable-buffer-local 'fcitx-emacs--evil-insert-disabled-by-elisp)

;;;###autoload
(defun fcitx-emacs-evil-turn-on ()
  (interactive)
  (eval-after-load "evil"
    '(progn
       (add-hook 'evil-insert-state-exit-hook
                 #'fcitx-emacs--evil-insert-maybe-deactivate)
       (add-hook 'evil-insert-state-entry-hook
                 #'fcitx-emacs--evil-insert-maybe-activate))))

;;;###autoload
(defun fcitx-emacs-evil-turn-off ()
  (interactive)
  (eval-after-load "evil"
    '(progn
       (remove-hook 'evil-insert-state-exit-hook
                    #'fcitx-emacs--evil-insert-maybe-deactivate)
       (remove-hook 'evil-insert-state-entry-hook
                    #'fcitx-emacs--evil-insert-maybe-activate))))

;; ----------------------------- ;;
;; M-x, M-!, M-& and M-: support ;;
;; ----------------------------- ;;
(fcitx-emacs--defun-maybe "minibuffer")

(defun fcitx-emacs--minibuffer (orig-fun &rest args)
  (fcitx-emacs--minibuffer-maybe-deactivate)
  (unwind-protect
      (apply orig-fun args)
    (fcitx-emacs--minibuffer-maybe-activate)))

(defmacro fcitx-emacs-minibuffer-on-off (func-name command)
  (let ((turn-on-func-name (intern
                            (concat "fcitx-emacs-"
                                    func-name
                                    "-turn-on")))
        (turn-off-func-name (intern
                             (concat "fcitx-emacs-"
                                     func-name
                                     "-turn-off"))))
    `(progn
       (defun ,turn-on-func-name ()
         (interactive)
         (advice-add ,command :around #'fcitx-emacs--minibuffer))
       (autoload 'fcitx-emacs-M-x-turn-on "fcitx-emacs")
       (defun ,turn-off-func-name ()
         (interactive)
         (advice-remove ,command #'fcitx-emacs--minibuffer))
       (autoload ',turn-off-func-name "fcitx-emacs"))))

(let ((M-x-cmd (key-binding (kbd "M-x"))))
  (cond
   ((eq M-x-cmd 'execute-extended-command)
    (fcitx-emacs-minibuffer-on-off "M-x" 'read-extended-command))
   ((or (eq M-x-cmd 'smex)
        (eq M-x-cmd 'helm-M-x))
    (fcitx-emacs-minibuffer-on-off "M-x" 'smex))
   (t
    (error "I don't know your `M-x' binding command.
 Only support original M-x, `smex' and `helm-M-x'"))))
(fcitx-emacs-minibuffer-on-off "shell-command" 'read-shell-command)
(fcitx-emacs-minibuffer-on-off "eval-expression" 'read--expression)

;;;###autoload
(defun fcitx-emacs-default-setup ()
  "Default setup for `fcitx-emacs'."
  (interactive)
  (fcitx-emacs--check-fcitx)
  ;; enable prefix keys related
  (fcitx-emacs-prefix-keys-setup)
  (fcitx-emacs-prefix-keys-turn-on)
  ;; enable minibuffer related
  (fcitx-emacs-M-x-turn-on)
  (fcitx-emacs-shell-command-turn-on)
  (fcitx-emacs-eval-expression-turn-on)
  ;; enable evil related
  (fcitx-emacs-evil-turn-on))

(provide 'fcitx-emacs)
;;; fcitx-emacs.el ends here
