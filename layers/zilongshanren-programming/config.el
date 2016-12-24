;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends lua-mode)
(spacemacs|defvar-company-backends markdown-mode)
;; (spacemacs|defvar-company-backends org-mode)
(spacemacs|defvar-company-backends nxml-mode)

(spacemacs|defvar-company-backends sh-mode)
(spacemacs|defvar-company-backends shell-script-mode)
(spacemacs|defvar-company-backends makefile-bsdmake-mode)
(spacemacs|defvar-company-backends conf-unix-mode)
(spacemacs|defvar-company-backends json-mode)

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

(add-hook 'term-mode-hook 'zilongshanren/ash-term-hooks)


;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))




(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt
                                                   '("xml"
                                                     "xsd"
                                                     "rng"
                                                     "xslt"
                                                     "xsl")
                                                   t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)



(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))



;; return nil to write content to file
(defun zilongshanren/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))

(add-hook 'c++-mode-hook
          '(lambda ()
             (add-hook 'write-contents-hooks
                       'zilongshanren/untabify-buffer nil t)))

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-bsdmake-mode))
       auto-mode-alist))


(defmacro zilongshanren|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "zilong/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))


(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

;; (set-auto-mode-alist '("\\.js\\'" . js2-mode))

;; (require 'alist )
;; (set-alist 'auto-mode-alist "\\.js\\'" js2-mode)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


(defvar js2-es6-mode-keywords
  '(
    ("\\<\\(process\\)\\>" 1 font-lock-type-face)
    ("\\<\\(module\\)\\>" 1 font-lock-type-face)
    ("\\<\\(then\\)\\>" 1 font-lock-type-face)
    ("\\<\\(on\\)\\>" 1 font-lock-type-face)
    ("\\<\\(catch\\)\\>" 1 font-lock-type-face)
    ("\\<\\(map\\)\\>" 1 font-lock-type-face)
    ("\\<\\(forEach\\)\\>" 1 font-lock-type-face)
    ("\\<\\(require\\)\\>" 1 font-lock-type-face)
    ("\\<\\(console\\)\\>" 1 font-lock-type-face)
    ("\\<\\(JSON\\)\\>" 1 font-lock-type-face)
    ("\\<\\(exports\\)\\>" 1 font-lock-variable-name-face)
    ("\\<\\(self\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(Map\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(Set\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(Object\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(Array\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(setInterval\\)\\>" 1 font-keyword-face)
    ("\\<\\(Array\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(yield\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(await\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(Promise\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(async\\)\\>" 1 font-lock-keyword-face)
    ))

(font-lock-add-keywords 'js2-mode js2-es6-mode-keywords)


;; (setq racket-racket-program "racket")
;; (setq racket-raco-program "raco")
;; (add-hook 'racket-mode-hook
;;           (lambda ()
;;             (define-key racket-mode-map (kbd "C-x C-j") 'racket-run)))
;; ;; (setq tab-always-indent 'complete) ;; 使用tab自动补全
