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

;;                              _____________

;;                                 FCITX.EL

;;                               Junpeng Qiu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Setup
;; 2 Example Settings
;; 3 Features
;; .. 3.1 The Feature List
;; .. 3.2 Features Enabled in Both Setup Commands
;; ..... 3.2.1 Disable Fcitx by Prefix Keys
;; ..... 3.2.2 Evil Support
;; ..... 3.2.3 Character & Key Input Support
;; ..... 3.2.4 `org-speed-command' Support
;; .. 3.3 Features Enabled *ONLY* in `fcitx-default-setup' Command
;; ..... 3.3.1 `M-x', `M-!', `M-&' and `M-:' Support
;; .. 3.4 Features Enabled *ONLY* in `fcitx-aggressive-setup' Command
;; ..... 3.4.1 Disable Fcitx in Minibuffer
;; .. 3.5 Extra Functions That are not Enabled in Both Commands
;; ..... 3.5.1 I-search Support
;; 4 Using D-Bus Interface
;; 5 TODO TODO


;; [[file:http://melpa.org/packages/fcitx-badge.svg]]
;; [[file:http://stable.melpa.org/packages/fcitx-badge.svg]]

;; Better [fcitx] integration for Emacs.

;; [中文版]

;; This package provides a set of functions to make fcitx work better in
;; Emacs.


;; [[file:http://melpa.org/packages/fcitx-badge.svg]]
;; http://melpa.org/#/fcitx

;; [[file:http://stable.melpa.org/packages/fcitx-badge.svg]]
;; http://stable.melpa.org/#/fcitx

;; [fcitx] https://github.com/fcitx/fcitx/

;; [中文版] ./README-zh.org


;; 1 Setup
;; =======

;;   Recommendation: install this package from [melpa].

;;   Or, if you like to manually install this package:
;;   ,----
;;   | (add-to-list 'load-path "/path/to/fcitx.el")
;;   | (require 'fcitx)
;;   `----

;;   You can choose between two different setup commands:
;;   ,----
;;   | M-x fcitx-default-setup
;;   `----
;;   or
;;   ,----
;;   | M-x fcitx-aggressive-setup
;;   `----

;;   The differences between these two setups will be explained later.


;; [melpa] http://melpa.org


;; 2 Example Settings
;; ==================

;;   All the examples below use `fcitx-aggressive-setup'.

;;   For Emacs users on Linux:
;;   ,----
;;   | (fcitx-aggressive-setup)
;;   | (setq fcitx-use-dbus t)
;;   `----

;;   For Emacs users on OS X:
;;   ,----
;;   | (fcitx-aggressive-setup)
;;   `----

;;   For Spacemacs users:
;;   ,----
;;   | ;; Make sure the following comes before `(fcitx-aggressive-setup)'
;;   | (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
;;   | (fcitx-aggressive-setup)
;;   | (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
;;   | ;; (setq fcitx-use-dbus t) ; uncomment if you're using Linux
;;   `----


;; 3 Features
;; ==========

;;   This package comes with a bunch of features to provide better `fcitx'
;;   integration for Emacs. For every feature, you can enable or disable it
;;   using the corresponding `*-turn-on' or `*-turn-off' command.

;;   To simplify the configuration, we provide two different setup
;;   commands, `fcitx-default-setup' and `fcitx-aggressive-setup'. They
;;   will enable different lists of features. You can choose the setup
;;   command that fits your need best. For users who want a better control,
;;   you can define and use your own setup command by enabling the features
;;   you want using the `*-turn-on' commands.


;; 3.1 The Feature List
;; ~~~~~~~~~~~~~~~~~~~~

;;   *X* indicates that the corresponding feature is enabled.

;;    Feature                      fcitx-default-setup  fcitx-aggressive-setup
;;   --------------------------------------------------------------------------
;;    Prefix-key                   X                    X
;;    Evil                         X                    X
;;    Character & key input        X                    X
;;    M-x,M-!,M-& and M-:          X
;;    Disable fcitx in minibuffer                       X
;;    org-speed-command support    X                    X
;;    Isearch


;; 3.2 Features Enabled in Both Setup Commands
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   The following features are enabled in both `fcitx-default-setup' and
;;   `fcitx-aggressive-setup'. You don't have to do anything if you're
;;   satisfied with the default settings.


;; 3.2.1 Disable Fcitx by Prefix Keys
;; ----------------------------------

;;   - *Why this feature*

;;     If you've enabled fcitx, then you can't easily change your buffer by
;;     `C-x b' because the second key, `b', will be blocked by fcitx(and
;;     you need to press `enter' in order to send `b' to emacs). This
;;     feature allows you to temporarily disable fcitx after pressing some
;;     prefix keys you've defined.

;;   - *What do the pre-defined setup comamnds do*

;;     Both setup comamnds define `C-x' and `C-c' to be such prefix keys,
;;     which means fcitx will be disabled after `C-x' or `C-c' is pressed.
;;     This setting should be enough for most users.

;;   - *For Spacemacs users*

;;     If you're a Spacemacs user who uses it in the Emacs way(or hybrid
;;     mode), it is possible that you want `M-m' to be the prefix key too.
;;     You can use the following command to add `M-m':
;;     ,----
;;     | (fcitx-prefix-keys-add "M-m")
;;     `----

;;   - *For users who want more customizations*

;;     You can define the prefix keys as you want:
;;     ,----
;;     | (fcitx-prefix-keys-add "C-x" "C-c" "C-h" "M-s" "M-o")
;;     `----

;;     After defining prefix keys, you need to call
;;     ,----
;;     | (fcitx-prefix-keys-turn-on)
;;     `----
;;     to enable this feature.

;;     Of course, you can use
;;     ,----
;;     | (fcitx-prefix-keys-turn-off)
;;     `----
;;     to disable this feature.


;; 3.2.2 Evil Support
;; ------------------

;;   - *Why this feature*

;;     This feature allows you to disable fcitx when you exit the "insert
;;     mode" and to reenable fcitx after enter "insert mode". Similar to
;;     [fcitx.vim].

;;     In addition, it will also disable fcitx if you use
;;     `switch-to-buffer' or `other-window' to switch to a buffer which is
;;     not in "insert mode". For example, if you're currently in "insert
;;     mode" in buffer `A' and you've enabled fcitx, then you call
;;     `switch-to-buffer' to switch to another buffer `B', which is
;;     currently, say, in normal mode, then fcitx will be disabled in
;;     buffer `B'.

;;   - *What do the pre-defined setup comamnds do*

;;     Both setup commands enable this feature. By default, `fcitx.el'
;;     consider both `evil-insert-state' and `evil-emacs-state' as "insert
;;     mode". Any transition from `evil-insert-state' or `evil-emacs-state'
;;     to any other evil state will disable fcitx if necessary.

;;   - *How to customize it*

;;     The evil states in which fcitx should be enabled are defined in the
;;     variable `fcitx-active-evil-states'. The default value is `(insert
;;     emacs)', which means fcitx will be enabled if necessary when
;;     entering `evil-insert-state' or `evil-emacs-state'. For Spacemacs
;;     users who use its hybrid mode, you may also want to add hybrid mode
;;     to the list:
;;     ,----
;;     | (setq fcitx-active-evil-states '(insert emacs hybrid))
;;     `----

;;   - *Bugs*

;;     Note that currently the Evil support is not perfect. If you come
;;     across any bugs, consider filing an issue or creating a pull
;;     request.


;; [fcitx.vim] https://github.com/vim-scripts/fcitx.vim


;; 3.2.3 Character & Key Input Support
;; -----------------------------------

;;   - *Why this feature*
;;     - Case 1: If you're using `ace-pinyin', you need to input a letter
;;       after calling `ace-pinyin'.
;;     - Case 2: You're using `C-h k' to see the binding for a key
;;       sequence.
;;     In both cases, fcitx will block your input. This feature can make
;;     `fcitx' automatically disabled when you're required to input a key
;;     sequence or a character.

;;   - *What do the pre-defined setup comamnds do*

;;     Both commands call `(fcitx-read-funcs-turn-on)' to enable this
;;     feature.

;;   - *What if I don't want it*

;;     Use `(fcitx-read-funcs-turn-off)' to disable it.


;; 3.2.4 `org-speed-command' Support
;; ---------------------------------

;;   - *Why this feature*

;;     This feature allows fcitx to be disabled when the cursor is at the
;;     beginning of an org heading so that you can use speed keys such as
;;     `n' and `p'.

;;   - *What do the pre-defined setup comamnds do*

;;     Both commands call `(fcitx-org-speed-command-turn-on)' to enable
;;     this feature.

;;   - *What if I don't want it*

;;     Use `(fcitx-org-speed-command-turn-off)' to disable it.


;; 3.3 Features Enabled *ONLY* in `fcitx-default-setup' Command
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; 3.3.1 `M-x', `M-!', `M-&' and `M-:' Support
;; -------------------------------------------

;;   - *Why these features*

;;     Usually you don't want to type Chinese when you use `M-x', `M-!'
;;     (`shell-command'), `M-&' (`async-shell-command') or `M-:'
;;     (`eval-expression'). You can automatically disable fcitx when you're
;;     using these commands.

;;   - *What does fcitx-default-setup do*

;;     It enables these features by calling the following commands:
;;     ,----
;;     | (fcitx-M-x-turn-on)
;;     | (fcitx-shell-command-turn-on)
;;     | (fcitx-eval-expression-turn-on)
;;     `----

;;     Your `M-x' binding should be one of `execute-extended-command' (the
;;     default `M-x' command), `smex' , `helm-M-x' and `counsel-M-x'.

;;     *WARNING*: If you rebind `M-x' to `smex', `helm-M-x', or
;;     `counsel-M-x', then you should call `fcitx-default-setup' or
;;     `fcitx-M-x-turn-on' *after* the key rebinding.

;;   - *How to customize it*

;;     You can enable some of the above three features by calling their
;;     corresponding `*-turn-on' commands, but remember if you rebind your
;;     `M-x', you should call `(fcitx-M-x-turn-on)' after the key
;;     rebinding.


;; 3.4 Features Enabled *ONLY* in `fcitx-aggressive-setup' Command
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; 3.4.1 Disable Fcitx in Minibuffer
;; ---------------------------------

;;   - *Why this features*

;;     For me, I personally don't need to type Chinese in minibuffer, so I
;;     would like to temporarily disable fcitx in minibuffer, no matter in
;;     what kind of command. If you are the same as me, then you could
;;     choose this setup.

;;   - *What does fcitx-aggressive-setup do*

;;     Unlike `fcitx-default-setup', it would not turn on `M-x', `M-!',
;;     `M-&' and `M-:' support. Instead, it will call
;;     `fcitx-aggressive-minibuffer-turn-on' to temporarily disable fcitx
;;     in all commands that use minibuffer as a source of input, including,
;;     but not limited to, `M-x', `M-!', `M-&' and `M-:'. That is why this
;;     is called "aggressive-setup". For example, if you press C-x b to
;;     switch buffer, or press C-x C-f to find file, fcitx will be disabled
;;     when you are in the minibuffer so that you can type English letters
;;     directly. However, if you choose `fcitx-default-setup', fcitx will
;;     not be disabled after you press C-x b or C-x C-f. I prefer this more
;;     aggressive setup because I don't use Chinese in my filename or
;;     buffer name.


;; 3.5 Extra Functions That are not Enabled in Both Commands
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These functions are not enabled in either `fcitx-default-setup' or
;;   `fcitx-aggressive-setup'. You need to enable them manually if you want
;;   to use them.


;; 3.5.1 I-search Support
;; ----------------------

;;   Usually when you use fcitx, you also want to I-search in Chinese, so
;;   this feature is not enabled by eith `fcitx-default-setup' or
;;   `fcitx-aggressive-setup'. If you do want to disable fcitx when using
;;   I-search, enable this feature explicitly by
;;   ,----
;;   | (fcitx-isearch-turn-on)
;;   `----


;; 4 Using D-Bus Interface
;; =======================

;;   For Linux users, it is recommended that you set `fcitx-use-dbus' to be
;;   `t' to speed up a little:
;;   ,----
;;   | (setq fcitx-use-dbus t)
;;   `----

;;   For OSX users who use [fcitx-remote-for-osx], don't set this variable.


;; [fcitx-remote-for-osx]
;; https://github.com/CodeFalling/fcitx-remote-for-osx


;; 5 TODO TODO
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
  (declare-function fcitx--counsel-M-x-turn-on "fcitx")
  (declare-function fcitx--original-M-x-turn-off "fcitx")
  (declare-function fcitx--smex-M-x-turn-off "fcitx")
  (declare-function fcitx--helm-M-x-turn-off "fcitx")
  (declare-function fcitx--counsel-M-x-turn-off "fcitx")
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
       (let ((output
              (let (deactivate-mark)
                (with-temp-buffer
                  (call-process "fcitx-remote" nil t)
                  (buffer-string)))))
         (and (char-equal (aref output 0) ?N)
              (display-warning "fcitx.el" "`fcitx' is not running. \
Re-run the setup function after `fcitx' is started.")))
     (display-warning "fcitx.el" "`fcitx-remote' is not avaiable. Please check your\
 fcitx installtion."))))

(defmacro fcitx--defun-system-interface (func-suffix)
  (let ((func-name (intern (format "fcitx--%S" func-suffix)))
        (dbus-fn (intern (format "fcitx--%S-dbus" func-suffix)))
        (proc-fn (intern (format "fcitx--%S-proc" func-suffix)))
        (osx-fn (intern (format "fcitx--%S-osx" func-suffix))))
    `(defun ,func-name ()
       (unless executing-kbd-macro
         (cond
          ((eq system-type 'darwin) (,osx-fn))
          (fcitx-use-dbus (,dbus-fn))
          (t (,proc-fn)))))))

(defun fcitx--activate-proc ()
  (call-process "fcitx-remote" nil nil nil "-o"))

(defun fcitx--deactivate-proc ()
  (call-process "fcitx-remote" nil nil nil "-c"))

(defun fcitx--active-p-proc ()
  (let ((output
         (let (deactivate-mark)
           (with-temp-buffer
             (call-process "fcitx-remote" nil t)
             (buffer-string)))))
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

;; OSX interface
(defun fcitx--active-p-osx ()
  (let (deactivate-mark)
    (with-temp-buffer
      (call-process-shell-command
       "defaults read ~/Library/Preferences/com.apple.HIToolbox.plist \
AppleSelectedInputSources" nil t)
      (goto-char (point-min))
      (not (re-search-forward "^.*KeyboardLayout Name.*$" nil t)))))

(defsubst fcitx--osx-toggle ()
  (do-applescript "tell application \"System Events\" to keystroke \"z\" \
using {shift down, control down}"))

(defun fcitx--activate-osx ()
  (unless (fcitx--active-p-osx)
    (fcitx--osx-toggle)))

(defun fcitx--deactivate-osx ()
  (when (fcitx--active-p-osx)
    (fcitx--osx-toggle)))

;; General interface
(fcitx--defun-system-interface activate)
(fcitx--defun-system-interface deactivate)
(fcitx--defun-system-interface active-p)

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
                     (window-minibuffer-p)))
           (not (and
                 (boundp 'which-key--paging-functions)
                 (member this-command which-key--paging-functions))))
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
  (when (and (boundp 'evil-mode) evil-mode)
    (let (active-state-p)
      (dolist (state fcitx-active-evil-states)
        (let ((state-p-func (intern (format "evil-%s-state-p" state))))
          (and (fboundp state-p-func)
               (funcall state-p-func)
               (setq active-state-p t))))
      (not active-state-p))))

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
  (and (not (member evil-next-state fcitx-active-evil-states))
       (fcitx--evil-insert-maybe-deactivate)))

(defun fcitx--active-evil-states-entry ()
  (and (not (eq evil-previous-state 'operator))
       (not (member evil-previous-state fcitx-active-evil-states))
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
  (if executing-kbd-macro
      (apply orig-fun args)
    (fcitx--minibuffer-maybe-deactivate)
    (unwind-protect
        (apply orig-fun args)
      (fcitx--minibuffer-maybe-activate))))

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
(fcitx-defun-minibuffer-on-off "-counsel-M-x" 'read-from-minibuffer)

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
     ((eq M-x-cmd 'counsel-M-x)
      (fcitx--counsel-M-x-turn-on))
     (t
      (error "Unknown `M-x' binding command.\
 Only original M-x, `smex', `helm-M-x' and `counsel-M-x' are supported.")))))

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
     ((eq M-x-cmd 'counsel-M-x)
      (fcitx--counsel-M-x-turn-off))
     (t
      (error "Unknown `M-x' binding command.\
 Only original M-x, `smex', `helm-M-x' and `counsel-M-x' are supported.")))))

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

;; ----------------- ;;
;; org-speed-command ;;
;; ----------------- ;;
(fcitx--defun-maybe "org-speed-command")

(defun fcitx--org-post-command-hook ()
  (when (bound-and-true-p org-use-speed-commands)
    (if (and (bolp) (looking-at org-outline-regexp))
        (fcitx--org-speed-command-maybe-deactivate)
      (unless (fcitx--evil-should-disable-fcitx-p)
        (fcitx--org-speed-command-maybe-activate)))))

(defun fcitx--org-mode-hook ()
  (add-hook 'post-command-hook 'fcitx--org-post-command-hook nil t))

;;;###autoload
(defun fcitx-org-speed-command-turn-on ()
  (interactive)
  (add-hook 'org-mode-hook #'fcitx--org-mode-hook))

;;;###autoload
(defun fcitx-org-speed-command-turn-off ()
  (interactive)
  (remove-hook 'org-mode-hook #'fcitx--org-mode-hook))

;; -------------- ;;
;; Setup commands ;;
;; -------------- ;;

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
    ;; enable read-* function support
    (fcitx-read-funcs-turn-on)
    ;; enable evil-related features
    (fcitx-evil-turn-on)
    ;; enable org-speed-command support
    (fcitx-org-speed-command-turn-on)))

;;;###autoload
(defun fcitx-aggressive-setup ()
  "Aggressive setup for `fcitx'."
  (interactive)
  (when (fcitx--check-status)
    ;; enable prefix-keys feature
    (fcitx-prefix-keys-setup)
    (fcitx-prefix-keys-turn-on)
    ;; enable read-* function support
    (fcitx-read-funcs-turn-on)
    ;; enable evil-related features
    (fcitx-evil-turn-on)
    ;; disable fcitx in minibuffer
    (fcitx-aggressive-minibuffer-turn-on)
    ;; enable org-speed-command support
    (fcitx-org-speed-command-turn-on)))

(provide 'fcitx)
;;; fcitx.el ends here
