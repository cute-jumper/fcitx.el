;;; fcitx.el --- Make fcitx better in Emacs

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions
;; URL: https://github.com/cute-jumper/fcitx.el

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

;; Chinese version of README: https://github.com/cute-jumper/fcitx.el/blob/master/README-zh.org

;;                              _____________

;;                                 FCITX.EL

;;                               Junpeng Qiu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Setup
;; 2 Disable Fcitx by Prefix Keys
;; 3 Evil Support
;; 4 `M-x', `M-!', `M-&' and `M-:' Support
;; 5 Aggressive setup
;; 6 Extra Functions
;; .. 6.1 I-search Support
;; .. 6.2 Character & Key Input Support
;; 7 D-Bus Version
;; 8 TODO TODO


;; Make [fcitx] better in Emacs.

;; [中文版]

;; This package provides a set of functions to make fcitx work better in
;; Emacs.


;; [fcitx] https://github.com/fcitx/fcitx/

;; [中文版] ./README-zh.org


;; 1 Setup
;; =======

;;   ,----
;;   | (add-to-list 'load-path "/path/to/fcitx.el")
;;   | (require 'fcitx)
;;   `----

;;   Recommended though optional:
;;   ,----
;;   | M-x fcitx-default-setup
;;   `----

;;   Alternatively, you can use a more aggressive setup:
;;   ,----
;;   | M-x fcitx-aggressive-setup
;;   `----

;;   The differences between these two setups will be illustrated later.

;;   Calling `fcitx-default-setup' will enable all the features that this
;;   package provides and use default settings. See the following sections
;;   for details.

;;   Note that for every feature, there are both `*-turn-on' and
;;   `*-turn-off' functions defined, which can enable and disable the
;;   corresponding feature, respectively.


;; 2 Disable Fcitx by Prefix Keys
;; ==============================

;;   If you've enabled fcitx, then you can't easily change your buffer by
;;   "C-x b" because the second key "b" will be blocked by fcitx(and you
;;   need to press "enter" in order to send "b" to emacs). This package
;;   provides a way to define the prefix keys after which you can
;;   temporarily disable fcitx.

;;   For example, you want to temporarily disable fcitx after you press
;;   "C-x" so that you can directly type "b" after "C-x" and after you
;;   press "b", fcitx will be activated again so you can still type Chinese
;;   buffer name. To define "C-x" to be the prefix key that can temporarily
;;   disable fcitx:
;;   ,----
;;   | (fcitx-prefix-keys-add "C-x")
;;   `----

;;   Usually, defining "C-x" and "C-c" to be such prefix keys is enough for
;;   most users. You can simply use following command:
;;   ,----
;;   | (fcitx-prefix-keys-setup)
;;   `----
;;   to add "C-x" and "C-c".

;;   After defining prefix keys, you need to call
;;   ,----
;;   | (fcitx-prefix-keys-turn-on)
;;   `----
;;   to enable this feature.

;;   Of course, you can use
;;   ,----
;;   | (fcitx-prefix-keys-turn-off)
;;   `----
;;   to disable this feature.

;;   Note if you use `M-x fcitx-default-setup', then it already does all
;;   the above things, i.e. adding "C-x" and "C-c" to be prefix keys and
;;   enabling this feature, for you.


;; 3 Evil Support
;; ==============

;;   To disable fcitx when you exiting "insert mode" and enable fcitx after
;;   entering "insert mode" if originally you enable it in "insert mode":
;;   ,----
;;   | (fcitx-evil-turn-on)
;;   `----

;;   The evil states in which fcitx should be enabled are defined in the
;;   variable `fcitx-active-evil-states'. The default value is `(insert
;;   emacs)', which means it will enable fcitx if necessary when entering
;;   `evil-insert-state' or `evil-emacs-state'. For Spacemacs users who use
;;   its hybrid mode, you may also want to add hybrid mode to the list:
;;   ,----
;;   | (setq fcitx-active-evil-states '(insert emacs hybrid))
;;   `----

;;   It will also disable fcitx if you use `switch-to-buffer' or
;;   `other-window' to switch to a buffer which is not in insert state or
;;   Emacs state. For example, if you're currently in insert mode in buffer
;;   `A' and you've enabled fcitx, then you call `switch-to-buffer' to
;;   switch to another buffer `B', which is currently, say, in normal mode,
;;   then fcitx will be disabled in buffer `B'.

;;   Note that currently the Evil support is not perfect. If you come
;;   across any bugs, consider filing an issue or creating a pull request.

;;   Similarly, `M-x fcitx-default-setup' enables this feature.


;; 4 `M-x', `M-!', `M-&' and `M-:' Support
;; =======================================

;;   Usually you don't want to type Chinese when you use `M-x', `M-!'
;;   (`shell-command'), `M-&' (`async-shell-command') or `M-:'
;;   (`eval-expression'). You can use:
;;   ,----
;;   | (fcitx-M-x-turn-on)
;;   | (fcitx-shell-command-turn-on)
;;   | (fcitx-eval-expression-turn-on)
;;   `----
;;   to disable fcitx temporarily in these commands.

;;   `M-x' should work with the original `M-x'
;;   (`execute-extended-command'), `smex' and `helm-M-x'.

;;   Again, `M-x fcitx-default-setup' enables all these three features.

;;   Note: If you rebind `M-x' to `smex' or `helm-M-x', then you should
;;   call `fcitx-default-setup' or `fcitx-M-x-turn-on' *after* the
;;   rebinding.


;; 5 Aggressive setup
;; ==================

;;   For me, I personally don't need to type Chinese in minibuffer, so I
;;   would like to temporarily disable fcitx in minibuffer, no matter in
;;   what kind of command. If you are the same as me, then you could choose
;;   this setup.

;;   Basically, `fcitx-aggressive-setup' would setup prefix keys feature
;;   and Evil support as `fcitx-default-setup' does, but it would not turn
;;   on `M-x', `M-!', `M-&' and `M-:' support. Instead, it will call
;;   `fcitx-aggressive-minibuffer-turn-on' to temporarily disable fcitx in
;;   all commands that use minibuffer as a source of input, including, but
;;   not limited to, `M-x', `M-!', `M-&' and `M-:'. That is why this is
;;   called "aggressive-setup". For example, if you press "C-x b" to switch
;;   buffer, or press "C-x C-f" to find file, fcitx will be disabled when
;;   you are in the minibuffer. I prefer this setup because I don't use
;;   Chinese in my filename or buffer name.


;; 6 Extra Functions
;; =================

;;   These functions are not enabled in either `fcitx-default-setup' or
;;   `fcitx-aggressive-setup'. You need to enable them manually if you want
;;   to use them.


;; 6.1 I-search Support
;; ~~~~~~~~~~~~~~~~~~~~

;;   Usually when you use fcitx, you also want to I-search in Chinese, so
;;   this feature is not enabled by eith `fcitx-default-setup' or
;;   `fcitx-aggressive-setup'. If you do want to disable fcitx when using
;;   I-search, enable this feature explicitly by
;;   ,----
;;   | (fcitx-isearch-turn-on)
;;   `----


;; 6.2 Character & Key Input Support
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   If you're using `ace-pinyin', you need to input a letter after calling
;;   `ace-pinyin'. `fcitx' can be automatically disabled by turning on the
;;   following feature:
;;   ,----
;;   | (fcitx-read-funcs-turn-on)
;;   `----

;;   However, the current implementation of this feature has some problems.
;;   See [issue #12] and [issue #14].

;;   So when to enable this feature?
;;   1. If you're on *Linux* and set `fcitx-use-dbus' to be `t', it is
;;      totally fine if you enable this feature. Although this feature of
;;      `fcitx.el' will not work as expected in all cases(see [issue #12]),
;;      it does *no harm* for you to enable this.
;;   2. If you're on OS X and use Evil, the problem described in [issue
;;      #14] may happen. You can stiil enable this feature, using the
;;      following code:
;;      ,----
;;      | (fcitx-read-funcs-turn-on)
;;      | (fcitx-read-key-sequence-turn-off)
;;      `----


;;   [issue #12] https://github.com/cute-jumper/fcitx.el/issues/12

;;   [issue #14] https://github.com/cute-jumper/fcitx.el/issues/14


;; 7 D-Bus Version
;; ===============

;;   For Linux users, you can set `fcitx-use-dbus' to be `t' to speed up a
;;   little:
;;   ,----
;;   | (setq fcitx-use-dbus t)
;;   `----

;;   For OSX users who use [fcitx-remote-for-osx], don't set this variable.


;;   [fcitx-remote-for-osx]
;;   https://github.com/CodeFalling/fcitx-remote-for-osx


;; 8 TODO TODO
;; ===========

;;   - Better Evil support

;;   For more features, pull requests are always welcome!

;;; Code:

(require 'dbus)

;; To get rid of byte compilation warnings
(eval-when-compile
  ;; evil-related
  (defvar evil-mode)
  (defvar evil-previous-state)
  (defvar evil-next-state)
  ;; fcitx-related
  (declare-function fcitx--original-M-x-turn-on "fcitx")
  (declare-function fcitx--smex-M-x-turn-on "fcitx")
  (declare-function fcitx--helm-M-x-turn-on "fcitx")
  (declare-function fcitx--original-M-x-turn-off "fcitx")
  (declare-function fcitx--smex-M-x-turn-off "fcitx")
  (declare-function fcitx--helm-M-x-turn-off "fcitx")
  (declare-function fcitx-shell-command-turn-on "fcitx")
  (declare-function fcitx-eval-expression-turn-on "fcitx")
  (declare-function fcitx-read-char-turn-on "fcitx")
  (declare-function fcitx-read-char-turn-off "fcitx")
  (declare-function fcitx-read-key-turn-on "fcitx")
  (declare-function fcitx-read-key-turn-off "fcitx")
  (declare-function fcitx-read-key-sequence-turn-on "fcitx")
  (declare-function fcitx-read-key-sequence-turn-off "fcitx")
  (declare-function fcitx-read-key-sequence-vector-turn-on "fcitx")
  (declare-function fcitx-read-key-sequence-vector-turn-off "fcitx"))

(defvar fcitx-prefix-keys-polling-time 0.1
  "Time interval to execute prefix keys polling function.")

(defvar fcitx-use-dbus nil
  "Whether we should use D-Bus version or not.
Default value is nil.")

(defvar fcitx-active-evil-states '(insert emacs)
  "Fcitx should be activated in those evil states.")

(defvar fcitx--prefix-keys-sequence nil
  "Prefix keys that can trigger disabling fcitx.")

(defvar fcitx--prefix-keys-timer nil
  "Timer for prefix keys polling function.")

(defvar fcitx--aggressive-p nil
  "Whether we should disable fcitx whenever we're in the minibuffer.")

(defun fcitx--check-status ()
  (not
   (if (executable-find "fcitx-remote")
       (let ((output (with-temp-buffer
                       (call-process "fcitx-remote" nil t)
                       (buffer-string))))
         (and (char-equal (aref output 0) ?N)
              (display-warning "fcitx.el" "`fcitx' is not running. \
Re-run the setup function after `fcitx' is started.")))
     (display-warning "fcitx.el" "`fcitx-remote' is not avaiable. Please check your\
 fcitx installtion."))))

(defmacro fcitx--defun-dbus-or-proc (func-suffix)
  (let ((func-name (intern (format "fcitx--%S" func-suffix)))
        (dbus-fn (intern (format "fcitx--%S-dbus" func-suffix)))
        (proc-fn (intern (format "fcitx--%S-proc" func-suffix))))
    `(defun ,func-name ()
       (if fcitx-use-dbus (,dbus-fn)
         (,proc-fn)))))

(fcitx--defun-dbus-or-proc activate)
(fcitx--defun-dbus-or-proc deactivate)
(fcitx--defun-dbus-or-proc active-p)

(defun fcitx--activate-proc ()
  (call-process "fcitx-remote" nil nil nil "-o"))

(defun fcitx--deactivate-proc ()
  (call-process "fcitx-remote" nil nil nil "-c"))

(defun fcitx--active-p-proc ()
  (let ((output (with-temp-buffer
                  (call-process "fcitx-remote" nil t)
                  (buffer-string))))

    (char-equal
     (aref output 0) ?2)))

(defun fcitx--activate-dbus ()
  (dbus-call-method :session
                    "org.fcitx.Fcitx"
                    "/inputmethod"
                    "org.fcitx.Fcitx.InputMethod"
                    "ActivateIM"))

(defun fcitx--deactivate-dbus ()
  (dbus-call-method :session
                    "org.fcitx.Fcitx"
                    "/inputmethod"
                    "org.fcitx.Fcitx.InputMethod"
                    "InactivateIM"))

(defun fcitx--active-p-dbus ()
  (= (dbus-call-method :session
                       "org.fcitx.Fcitx"
                       "/inputmethod"
                       "org.fcitx.Fcitx.InputMethod"
                       "GetCurrentState")
     2))

(defmacro fcitx--defun-maybe (prefix)
  (let ((var-symbol (intern
                     (concat "fcitx--"
                             prefix
                             "-disabled-by-elisp")))
        (deactivate-symbol (intern
                            (concat "fcitx--"
                                    prefix
                                    "-maybe-deactivate")))
        (activate-symbol (intern
                          (concat "fcitx--"
                                  prefix
                                  "-maybe-activate"))))
    `(progn
       (defvar ,var-symbol nil)
       (defun ,deactivate-symbol ()
         (when (fcitx--active-p)
           (fcitx--deactivate)
           (setq ,var-symbol t)))
       (defun ,activate-symbol ()
         (when ,var-symbol
           (fcitx--activate)
           (setq ,var-symbol nil))))))

;; ------------------- ;;
;; prefix keys support ;;
;; ------------------- ;;
(fcitx--defun-maybe "prefix-keys")

(defsubst fcitx--evil-adviced-commands-p (command)
  (and (boundp 'evil-mode)
       evil-mode
       (or (equal command 'switch-to-buffer)
           (equal command 'other-window))))

(defun fcitx--prefix-keys-polling-function ()
  "Polling function executed every `fcitx-prefix-keys-polling-time'."
  (let ((key-seq (this-single-command-keys)))
    (cond
     ((member key-seq fcitx--prefix-keys-sequence)
      (fcitx--prefix-keys-maybe-deactivate))
     ((and (equal (this-command-keys-vector) [])
           (not (fcitx--evil-adviced-commands-p last-command))
           (not (and fcitx--aggressive-p
                     (window-minibuffer-p))))
      (fcitx--prefix-keys-maybe-activate)))))

;;;###autoload
(defun fcitx-prefix-keys-add (&rest prefix-keys)
  (interactive)
  (dolist (key prefix-keys)
    (add-to-list 'fcitx--prefix-keys-sequence
                 (vconcat (read-kbd-macro key)))))

;;;###autoload
(defun fcitx-prefix-keys-turn-on ()
  "Turn on `fcixt-disable-prefix-keys'."
  (interactive)
  (unless fcitx--prefix-keys-timer
    (setq fcitx--prefix-keys-timer
          (run-at-time t fcitx-prefix-keys-polling-time
                       #'fcitx--prefix-keys-polling-function))))

;;;###autoload
(defun fcitx-prefix-keys-turn-off ()
  "Turn off `fcixt-disable-prefix-keys'."
  (interactive)
  (when fcitx--prefix-keys-timer
    (cancel-timer fcitx--prefix-keys-timer)
    (setq fcitx--prefix-keys-timer nil)))

;;;###autoload
(defun fcitx-prefix-keys-setup ()
  (interactive)
  (fcitx-prefix-keys-add "C-x" "C-c"))

;; ------------ ;;
;; evil support ;;
;; ------------ ;;
(fcitx--defun-maybe "evil-insert")
(make-variable-buffer-local 'fcitx--evil-insert-disabled-by-elisp)

(defvar fcitx--evil-saved-active-p nil
  "Remember the fcitx state for each buffer.")
(make-variable-buffer-local 'fcitx--evil-saved-active-p)

(defun fcitx--evil-should-disable-fcitx-p ()
  (let (active-state-p)
    (dolist (state fcitx-active-evil-states)
      (let ((state-p-func (intern (format "evil-%s-state-p" state))))
        (and (fboundp state-p-func)
             (funcall state-p-func)
             (setq active-state-p t))))
    (not active-state-p)))

;;FIX: cooperate with prefix keys and remove redundant code
(defun fcitx--evil-switch-buffer-before ()
  (when (and evil-mode
             (not (window-minibuffer-p)))
    ;; save state. Should we set `fcitx--prefix-keys-disabled-by-elisp' too?
    (setq fcitx--evil-saved-active-p
          (or (fcitx--active-p)
              fcitx--prefix-keys-disabled-by-elisp))
    (setq fcitx--prefix-keys-disabled-by-elisp nil)))

(defun fcitx--evil-switch-buffer-after ()
  (when (and evil-mode
             (not (window-minibuffer-p)))
    (cond
     ((fcitx--evil-should-disable-fcitx-p)
      (fcitx--deactivate))
     (fcitx--evil-saved-active-p
      (fcitx--activate)))
    (setq fcitx--evil-saved-active-p nil)))

(defun fcitx--evil-switch-buffer (orig-func &rest args)
  ;; before switch
  (fcitx--evil-switch-buffer-before)
  ;; switch buffer
  (prog1 (apply orig-func args)
    ;; after switch
    (fcitx--evil-switch-buffer-after)))

(unless (fboundp 'advice-add)
  (defadvice switch-to-buffer (around fcitx--evil-switch-buffer-1)
    ;; before switch
    (fcitx--evil-switch-buffer-before)
    ;; switch buffer
    ad-do-it
    ;; after switch
    (fcitx--evil-switch-buffer-after))
  (defadvice other-window (around fcitx--evil-switch-buffer-2)
    ;; before switch
    (fcitx--evil-switch-buffer-before)
    ;; switch buffer
    ad-do-it
    ;; after switch
    (fcitx--evil-switch-buffer-after)))

(defun fcitx--active-evil-states-exit ()
  (unless (member evil-next-state fcitx-active-evil-states)
    (fcitx--evil-insert-maybe-deactivate)))

(defun fcitx--active-evil-states-entry ()
  (unless (member evil-previous-state fcitx-active-evil-states)
    (fcitx--evil-insert-maybe-activate)))

(defun fcitx--evil-modify-hooks (add-p)
  (dolist (state fcitx-active-evil-states)
    (let ((exit-hook (intern (format "evil-%s-state-exit-hook" state)))
          (entry-hook (intern (format "evil-%s-state-entry-hook" state))))
      (when (and (boundp exit-hook)
                 (boundp entry-hook))
        (funcall (if add-p #'add-hook #'remove-hook) exit-hook
                 #'fcitx--active-evil-states-exit)
        (funcall (if add-p #'add-hook #'remove-hook) entry-hook
                 #'fcitx--active-evil-states-entry)))))

;;;###autoload
(defun fcitx-evil-turn-on ()
  (interactive)
  (eval-after-load "evil"
    '(progn
       (fcitx--evil-modify-hooks t)
       (if (fboundp 'advice-add)
           (progn
             (advice-add 'switch-to-buffer :around
                         #'fcitx--evil-switch-buffer)
             (advice-add 'other-window :around
                         #'fcitx--evil-switch-buffer))
         (ad-enable-advice 'switch-to-buffer 'around 'fcitx--evil-switch-buffer-1)
         (ad-activate 'switch-to-buffer)
         (ad-enable-advice 'other-window 'around 'fcitx--evil-switch-buffer-2)
         (ad-activate 'other-window)))))

;;;###autoload
(defun fcitx-evil-turn-off ()
  (interactive)
  (eval-after-load "evil"
    '(progn
       (fcitx--evil-modify-hooks nil)
       (if (fboundp 'advice-add)
           (progn
             (advice-remove 'switch-to-buffer
                            #'fcitx--evil-switch-buffer)
             (advice-remove 'other-window
                            #'fcitx--evil-switch-buffer))
         (ad-disable-advice 'switch-to-buffer 'around 'fcitx--evil-switch-buffer-1)
         (ad-activate 'switch-to-buffer)
         (ad-disable-advice 'other-window 'around 'fcitx--evil-switch-buffer-2)
         (ad-activate 'other-window)))))

;; ----------------------------- ;;
;; M-x, M-!, M-& and M-: support ;;
;; ----------------------------- ;;
(fcitx--defun-maybe "minibuffer")

(defun fcitx--minibuffer (orig-fun &rest args)
  (fcitx--minibuffer-maybe-deactivate)
  (unwind-protect
      (apply orig-fun args)
    (fcitx--minibuffer-maybe-activate)))

(defmacro fcitx-defun-minibuffer-on-off (func-name command)
  (let ((turn-on-func-name (intern
                            (concat "fcitx-"
                                    func-name
                                    "-turn-on")))
        (turn-off-func-name (intern
                             (concat "fcitx-"
                                     func-name
                                     "-turn-off"))))
    `(progn
       (if (fboundp 'advice-add)
           (progn
             (defun ,turn-on-func-name ()
               (interactive)
               (advice-add ,command :around #'fcitx--minibuffer))
             (defun ,turn-off-func-name ()
               (interactive)
               (advice-remove ,command #'fcitx--minibuffer)))
         (defadvice ,(cadr command) (around fcitx--minibuffer-1)
           (fcitx--minibuffer-maybe-deactivate)
           (unwind-protect
               ad-do-it
             (fcitx--minibuffer-maybe-activate)))
         (defun ,turn-on-func-name ()
           (interactive)
           (ad-enable-advice ,command 'around 'fcitx--minibuffer-1)
           (ad-activate ,command))
         (defun ,turn-off-func-name ()
           (interactive)
           (ad-disable-advice ,command 'around 'fcitx--minibuffer-1)
           (ad-activate ,command))))))

(defvar fcitx--M-x-binding-command nil
  "The command that `M-x' is bound to.")

(fcitx-defun-minibuffer-on-off "-original-M-x" 'read-extended-command)
(fcitx-defun-minibuffer-on-off "-smex-M-x" 'smex)
(fcitx-defun-minibuffer-on-off "-helm-M-x" 'helm-M-x-read-extended-command)

;;;###autoload
(defun fcitx-M-x-turn-on ()
  (interactive)
  (setq fcitx--M-x-binding-command (key-binding (kbd "M-x")))
  (let ((M-x-cmd fcitx--M-x-binding-command))
    (cond
     ((eq M-x-cmd 'execute-extended-command)
      (fcitx--original-M-x-turn-on))
     ((eq M-x-cmd 'smex)
      (fcitx--smex-M-x-turn-on))
     ((eq M-x-cmd 'helm-M-x)
      (fcitx--helm-M-x-turn-on))
     (t
      (error "I don't know your `M-x' binding command.
 Only support original M-x, `smex' and `helm-M-x'")))))

;;;###autoload
(defun fcitx-M-x-turn-off ()
  (interactive)
  (let ((M-x-cmd fcitx--M-x-binding-command))
    (cond
     ((eq M-x-cmd 'execute-extended-command)
      (fcitx--original-M-x-turn-off))
     ((eq M-x-cmd 'smex)
      (fcitx--smex-M-x-turn-off))
     ((eq M-x-cmd 'helm-M-x)
      (fcitx--helm-M-x-turn-off))
     (t
      (error "I don't know your `M-x' binding command.
 Only support original M-x, `smex' and `helm-M-x'")))))

;;;###autoload (autoload 'fcitx-shell-command-turn-on "fcitx" "Enable `shell-command' support" t)
;;;###autoload (autoload 'fcitx-shell-command-turn-off "fcitx" "Disable `shell-command' support" t)
(fcitx-defun-minibuffer-on-off "shell-command" 'read-shell-command)

;;;###autoload (autoload 'fcitx-eval-expression-turn-on "fcitx" "Enable `shell-command' support" t)
;;;###autoload (autoload 'fcitx-eval-expression-turn-off "fcitx" "Disable `eval-expression' support" t)
(fcitx-defun-minibuffer-on-off "eval-expression" 'read--expression)

;; ---------------- ;;
;; `read-*' support ;;
;; ---------------- ;;
(fcitx-defun-minibuffer-on-off "read-char" 'read-char)
(fcitx-defun-minibuffer-on-off "read-key" 'read-key)
(fcitx-defun-minibuffer-on-off "read-key-sequence" 'read-key-sequence)
(fcitx-defun-minibuffer-on-off "read-key-sequence-vector" 'read-key-sequence-vector)

;;;###autoload
(defun fcitx-read-funcs-turn-on ()
  (interactive)
  (fcitx-read-char-turn-on)
  (fcitx-read-key-turn-on)
  (fcitx-read-key-sequence-turn-on)
  (fcitx-read-key-sequence-vector-turn-on))

;;;###autoload
(defun fcitx-read-funcs-turn-off ()
  (interactive)
  (fcitx-read-char-turn-off)
  (fcitx-read-key-turn-off)
  (fcitx-read-key-sequence-turn-off)
  (fcitx-read-key-sequence-vector-turn-off))

;; ------------------------------ ;;
;; aggressive minibuffer strategy ;;
;; ------------------------------ ;;
(defvar fcitx--aggressive-minibuffer-disabled-by-elisp nil)

(defun fcitx--aggressive-minibuffer-maybe-deactivate ()
  (if (fcitx--active-p)
      (progn
        (fcitx--deactivate)
        (setq fcitx--aggressive-minibuffer-disabled-by-elisp t))
    (when fcitx--prefix-keys-disabled-by-elisp
      (setq fcitx--prefix-keys-disabled-by-elisp nil
            fcitx--aggressive-minibuffer-disabled-by-elisp t))))

(defun fcitx--aggressive-minibuffer-maybe-activate ()
  (when fcitx--aggressive-minibuffer-disabled-by-elisp
    (fcitx--activate)
    (setq fcitx--aggressive-minibuffer-disabled-by-elisp nil)))

;;;###autoload
(defun fcitx-aggressive-minibuffer-turn-on ()
  (interactive)
  (setq fcitx--aggressive-p t)
  (add-hook 'minibuffer-setup-hook
            #'fcitx--aggressive-minibuffer-maybe-deactivate)
  (add-hook 'minibuffer-exit-hook
            #'fcitx--aggressive-minibuffer-maybe-activate))

;;;###autoload
(defun fcitx-aggressive-minibuffer-turn-off ()
  (interactive)
  (setq fcitx--aggressive-p nil)
  (remove-hook 'minibuffer-setup-hook
               #'fcitx--aggressive-minibuffer-maybe-deactivate)
  (remove-hook 'minibuffer-exit-hook
               #'fcitx--aggressive-minibuffer-maybe-activate))

;; ------- ;;
;; isearch ;;
;; ------- ;;
(fcitx--defun-maybe "isearch")

;;;###autoload
(defun fcitx-isearch-turn-on ()
  (interactive)
  (add-hook 'isearch-mode-hook #'fcitx--isearch-maybe-deactivate)
  (add-hook 'isearch-mode-end-hook #'fcitx--isearch-maybe-activate))

;;;###autoload
(defun fcitx-isearch-turn-off ()
  (interactive)
  (remove-hook 'isearch-mode-hook #'fcitx--isearch-maybe-deactivate)
  (remove-hook 'isearch-mode-end-hook #'fcitx--isearch-maybe-activate))

;;;###autoload
(defun fcitx-default-setup ()
  "Default setup for `fcitx'."
  (interactive)
  (when (fcitx--check-status)
    ;; enable prefix-keys feature
    (fcitx-prefix-keys-setup)
    (fcitx-prefix-keys-turn-on)
    ;; enable minibuffer-related features
    (fcitx-M-x-turn-on)
    (fcitx-shell-command-turn-on)
    (fcitx-eval-expression-turn-on)
    ;; don't enable read-* functions support. See #14
    ;; (fcitx-read-funcs-turn-on)
    ;; enable evil-related features
    (fcitx-evil-turn-on)))

;;;###autoload
(defun fcitx-aggressive-setup ()
  "Aggressive setup for `fcitx'."
  (interactive)
  (when (fcitx--check-status)
    ;; enable prefix-keys feature
    (fcitx-prefix-keys-setup)
    (fcitx-prefix-keys-turn-on)
    ;; don't enable read-* functions support. See #14
    ;; (fcitx-read-funcs-turn-on)
    ;; enable evil-related features
    (fcitx-evil-turn-on)
    ;; disable fcitx in minibuffer
    (fcitx-aggressive-minibuffer-turn-on)))

(provide 'fcitx)
;;; fcitx.el ends here
