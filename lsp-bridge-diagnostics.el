;;; lsp-bridge-diagnostics.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: May 27, 2022
;; Modified: May 27, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-bridge-diagnostics
;; Package-Requires: ((emacs "27"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'lsp-bridge)

(defgroup lsp-bridge-diagnostics nil
  "LSP Bridge support for diagnostics."
  :prefix "lsp-bridge-diagnostics-"
  :group 'lsp-bridge
  :tag "LSP Bridge Diagnostics")

(defcustom lsp-bridge-diagnostics-provider :native
  "The checker backend provider."
  :type
  '(choice
    (const :tag "Pick native" :native)
    (const :tag "Pick flycheck" :flycheck))
  :group 'lsp-bridge-diagnostics)

;; Flycheck integration

(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-define-error-level "ext:flycheck" (level &rest properties))
(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-valid-checker-p "ext:flycheck")
(declare-function flycheck-stop "ext:flycheck")

(defvar flycheck-mode)
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defvar-local lsp-bridge-diagnostics--flycheck-enabled nil
  "None-nil when lsp diagnostics flycheck integration has been enabled in this buffer.")

(defvar-local lsp-bridge-diagnostics-list nil)

(defvar-local lsp-bridge-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp diagnostics was activated.")

(defun lsp-bridge-flycheck-add-mode (mode)
  "Register flycheck support for MODE."
  (lsp-bridge-diagnostics-lsp-bridge-checker-if-needed)
  (unless (flycheck-checker-supports-major-mode-p 'lsp-bridge-checker mode)
    (flycheck-add-mode 'lsp-bridge-checker mode)))

(defun lsp-bridge-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (let ((errors (mapcar
                 (lambda (diagnostic)
                   (let* ((range (plist-get diagnostic :range))
                          (start (plist-get range :start))
                          (end (plist-get range :end)))
                     (flycheck-error-new
                      :buffer (current-buffer)
                      :checker checker
                      :filename (buffer-file-name)
                      :message (plist-get diagnostic :message)
                      :level (pcase (plist-get diagnostic :severity)
                               (1 'error)
                               (2 'warning)
                               (3 'info)
                               (4 'info)
                               (_ 'error))
                      :id (plist-get diagnostic :code)
                      :group (plist-get diagnostic :source)
                      :line (1+ (plist-get start :line))
                      :column (1+ (plist-get start :character))
                      :end-line (1+ (plist-get end :line))
                      :end-column (1+ (plist-get end :character))))) lsp-bridge-diagnostics-list)))
    (funcall callback 'finished errors)))

(defvar lsp-bridge-diagnostics-mode) ;; properly defined by define-minor-mode below

;;;###autoload
(defun lsp-bridge-diagnostics-lsp-bridge-checker-if-needed ()
  (unless (flycheck-valid-checker-p 'lsp-bridge-checker)
    (flycheck-define-generic-checker 'lsp-bridge-checker
    "A syntax checker using the Language Server Protocol (LSP) provided by lsp-bridge."
    :start #'lsp-bridge-diagnostics--flycheck-start
    :modes '(lsp-bridge-placehoder-mode) ;; placeholder
    :predicate (lambda () lsp-bridge-diagnostics-mode))))

(defun lsp-bridge-diagnostics-flycheck-enable(&rest _)
  "Enable flycheck integration for the current buffer."
  (require 'flycheck)
  (lsp-bridge-diagnostics-lsp-bridge-checker-if-needed)
  (and (not lsp-bridge-diagnostics--flycheck-enabled)
       (not (eq flycheck-checker 'lsp-bridge-checker))
       (setq lsp-bridge-diagnostics--flycheck-checker flycheck-checker))
  (setq-local lsp-bridge-diagnostics--flycheck-enabled t)
  (flycheck-mode 1)
  (flycheck-stop)
  (setq-local flycheck-checker 'lsp-bridge-checker)
  (lsp-bridge-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-bridge-checker)
  (setq lsp-bridge-diagnostics-timer
        (run-with-idle-timer lsp-bridge-diagnostics-fetch-idle t #'lsp-bridge-diagnostics-fetch)))

(defun lsp-bridge-diagnostics-flycheck-disable ()
  "Disable flycheck integration for the current buffer is it was enabled."
  (when lsp-bridge-diagnostics--flycheck-enabled
    (flycheck-stop)
    (when (eq flycheck-checker 'lsp-bridge-checker)
      (setq-local flycheck-checker lsp-bridge-diagnostics--flycheck-checker))
    (setq lsp-bridge-diagnostics--flycheck-checker nil)
    (setq-local lsp-bridge-diagnostics--flycheck-enabled nil)
    (when flycheck-mode
      (flycheck-mode -1))))

(defun lsp-bridge-diagnostics--flycheck-report (filepath diagnostics)
  (lsp-bridge--with-file-buffer filepath
    (setq-local lsp-bridge-diagnostics-list diagnostics)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer))))


(defun lsp-bridge-diagnostics-render-advice (orig-fun filepath diagnostics)
  (cond ((eq lsp-bridge-diagnostics-provider :flycheck)
         (lsp-bridge-diagnostics--flycheck-report filepath diagnostics))
        ((eq lsp-bridge-diagnostics-provider :native)
         (apply orig-fun filepath diagnostics))
        (t (user-error "Not support this `lsp-bridge-diagnostics-provider' %s" lsp-bridge-diagnostics-provider))))

;; (defadvice! +lsp-bridge-diagnostics-render (fipepath diagnostics)
;;   :around #'lsp-bridge-diagnostics-render
;;   (cond ((eq lsp-bridge-diagnostics-provider :flycheck)
;;          (lsp-bridge-diagnostics--flycheck-report filepath diagnostics))
;;         ((eq lsp-bridge-diagnostics-provider :native)
;;          (lsp-bridge-diagnostics--native-report filepath diagnostics))
;;         (t (user-error "Not support this `lsp-bridge-diagnostics-provider' %s" lsp-bridge-diagnostics-provider))))

;;;###autoload
(defun lsp-bridge-diagnostics--enable ()
  "Enable LSP checker support."
  (when (and (member lsp-bridge-diagnostics-provider '(:native :flycheck))
             (not (member major-mode lsp-bridge-diagnostics-disabled-modes)))
    (lsp-bridge-diagnostics-mode 1)))

;;;###autoload
(define-minor-mode lsp-bridge-diagnostics-mode
  "Toggle LSP Bridge diagnostics integration."
  :group 'lsp-bridge-diagnostics
  :global nil
  :lighter ""
  (cond
   (lsp-bridge-diagnostics-mode
    (cond
     ((eq lsp-bridge-diagnostics-provider :native)
      (lsp-bridge-diagnostics-native-enable))
     ((and (and (eq lsp-bridge-diagnostics-provider :flycheck)
                (or (functionp 'flycheck-mode)
                    (user-error "The lsp-bridge-diagnostics-provider is set to :flycheck but flycheck is not installed?")))
           (require 'flycheck nil t))
      (lsp-bridge-diagnostics-flycheck-enable))
     (t (message "flycheck is not installed."))))
   (t (cond ((eq lsp-bridge-diagnostics-provider :flycheck)
             (lsp-bridge-diagnostics-flycheck-disable))
            ((eq lsp-bridge-diagnostics-provider :native)
             (lsp-bridge-diagnostics-native-disable))))))

;;;###autoload
(add-hook 'lsp-bridge-mode-hook #'lsp-bridge-diagnostics--enable)
(provide 'lsp-bridge-diagnostics)
;;; lsp-bridge-diagnostics.el ends here
