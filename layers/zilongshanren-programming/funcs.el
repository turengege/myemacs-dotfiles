;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun zilongshanren/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column         uv_
(current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))



;; "http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side"
(defun zilongshanren/annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))


;;js2-mode enhancement
(defun zilongshanren/js2-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))

(defun zilongshanren/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ;; ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("lua" . "lua")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*zilongshanren/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))



(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (eq major-mode 'json-mode)
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
             (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
             (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

(defun zilongshanren/load-yasnippet ()
  (interactive)
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
      (setq yas-snippet-dirs  my-snippet-dir)
      (yas-load-directory my-snippet-dir)
      (setq yas-wrap-around-region t)))
  (yas-minor-mode 1))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "CMakeLists.txt" (buffer-name)))
    (setq parent-dir (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory (buffer-file-name)))))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    (rename-buffer new-buffer-name t)))

(defun zilongshanren/impatient-mode-hook ()
  "my web mode hook for HTML REPL"
  (interactive)
  (impatient-mode)
  (spacemacs|hide-lighter impatient-mode)
  (httpd-start))

(defun my-js2-mode-hook ()
  (progn
    (define-key js2-mode-map "\C-ci" 'my-js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (modify-syntax-entry ?_ "w")
    (which-function-mode t)
    (pcre-mode)
    (setq imenu-create-index-function 'js2-imenu-make-index)
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/node/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":/Users/liurui/bin"))
    (setenv "PATH" (concat (getenv "PATH") "/Users/liurui/.nvm/versions/node/v7.8.0/lib/bin"))
    (setq exec-path (append exec-path '("/usr/local/node/bin")))
    (setq exec-path (append exec-path '("/Users/liurui/bin")))
    (setq exec-path (append exec-path '("/Users/liurui/.nvm/versions/node/v7.8.0/bin")))


    (setq mode-name "JS2")
    (define-key js2-mode-map   (kbd "s-.") 'company-tern)
    (spacemacs/toggle-syntax-checking-on)
    ;; (spacemacs/toggle-automatic-symbol-highlight-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function))

(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                               ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                               ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                               ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                               ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                               ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                               ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                               ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                               ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))


(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
        file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir)))
    file))

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)))


(defun my-auto-update-tags-when-save (prefix)
      (interactive "P")
      (cond
       ((not my-tags-updated-time)
        (setq my-tags-updated-time (current-time)))

       ((and (not prefix)
             (< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300))
        ;; < 300 seconds
        (message "no need to update the tags")
        )
       (t
        (setq my-tags-updated-time (current-time))
        (my-update-tags)
        (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-tags-updated-time))))))


(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "guanghui")
    (cond
     ((my-project-name-contains-substring "cocos2d-x")
      ;; C++ project don't need html tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
     ((my-project-name-contains-substring "Github/fireball")
      (message "load tags for fireball engine repo...")
      ;; html project donot need C++ tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/Github/fireball/engine/cocos2d")))))))


(defun set-auto-mode-alist (auto-mode-cel) (
                                            let ((auto-mode-set-key (car auto-mode-cel)))
                                             (setq auto-mode-alist
                                                   (mapcar
                                                    (lambda (one-auto-mode-cel)
                                                      (if (string= auto-mode-set-key (car one-auto-mode-cel))
                                                          auto-mode-cel
                                                        one-auto-mode-cel
                                                        ))
                                                    auto-mode-alist))))


(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ;; greek
          ("lambda" . ?λ)      ; 兰布达  波长（小写）；体积
          ("alpha" . 945)       ;α 角度；系数
          ("beta" . 946)        ;β 磁通系数；角度；系数
          ("gamma" . ?γ)       ; 伽马  电导系数（小写）
          ("GAMMA" . ?Γ)       ;γ 伽马  电导系数（小写）
          ("delta" . ?δ)       ; 变动；密度；屈光度
          ("DELTA" . ?Δ)       ; 变动；密度；屈光度
          ("epsilon" . ?ε)     ; 伊普西龙  对数之基数
          ("zeta" . ?ζ)        ; 截塔  系数；方位角；阻抗；相对粘度；原子序数
          ("eta" . ?η)         ;艾塔  磁滞系数；效率（小写）
          ("theta" . 952)       ;θ 温度；相位角
          ("iot" . ?ι)         ; 约塔  微小，一点儿
          ("kappa" . ?κ)       ;卡帕  介质常数
          ("mu" . ?μ)          ; 缪  磁导系数；微（千分之一）；放大因数（小写）
          ("nu" . ?ν)          ; 纽  磁阻系数
          ("xi" . ?ξ )         ;克西
          ("xil" . ?𝜉 )         ;
          ("rho" . ?ρ)         ;肉  电阻系数
          ("omicron" . ?ο)     ;奥密克戎
          ("sigma" . ?ς)   ; sigma  西格马  总和（大写），表面密度；跨导（小写）
          ("SIGMA" . ?∑)  ;
          ("tau" . ?τ)    ;tau  tau  套  时间常数
          ("phi" . ?φ)    ;phi  fai  佛爱  磁通；角
          ("pi" . ?π)     ;
          ("psi" . ?ψ)    ;psi  psai  普西  角速；介质电通量（静电力线）；角
          ("PSI" . ?Ψ)
          ("omega" . ?ω)       ; o`miga  欧米伽  欧姆（大写）；角速（小写）；角
          ("OMEGA" . ?Ω)
          ("euler" . ?ℇ)                ;欧拉

          ;; misc
          ("right" . ?✓)
          ("wrong" . ?✗)
          ("sectin" . ?§)
          ("refresh" . ?↺)

          ;; logical
          ("and" . ?∧)
          ("or" . ?∨)
          ("xor" . ?⊻)                  ;异或
          ("nor" . ?⊽)                  ;或非
          ("not" . ?¬)

          ;; game
          ("spade" . ?♠)
          ("club" . ?♣)
          ("heart" . ?♥)
          ("diamond" . ?♦)
          ("SPADE" . ?♤)
          ("CLUB" . ?♧)
          ("HEART" . ?♡)
          ("DIAMOND" . ?♢)
          ("king" . ?♚)
          ("queen" . ?♛)
          ("rook" . ?♜)
          ("bishop" . ?♝)
          ("knight" . ?♞)
          ("pawn" . ?♟)
          ("KING" . ?♔)
          ("QUEEN" . ?♕)
          ("ROOK" . ?♖)
          ("BISHOP" . ?♗)
          ("KNIGHT" . ?♘)
          ("PAWN" . ?♙)
          ;; music
          ("note" . ?♩)
          ;; math
          ("integral" , ?∫)
          ("infinite" . ?∞ )           ;
          ("root" . ?√)
          ("cube-root" . ?∛)
          ("forth-root" . ?∜)
          ("belong?" . ?∈)
          ("contain?" . ?∋)
          ("subset?" . ?⊂)
          ("superset?" . ?⊃)
          ("union" . ?∪)
          ("intersection" . ?∩)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("neq" . ?≠)
          ("almost" . ?≈)
          ("approximate" . ?⩰)
          ("===" . ?≡)
          ("real" . ?ℝ)
          ("natural" . ?ℕ)
          ("rational" . ?ℚ)
          ("imaginary" . ?ⅈ)
          ("imaginaryj" . ?ⅉ))))

(defun liurui/replace-symbol ()
    (interactive)
    (if prettify-symbols-mode
        (prettify-symbols-mode 0)
        (progn
          (my-add-pretty-lambda)
          (prettify-symbols-mode 1))))

(defun my-racket-mode-hook ()
  (progn
    (my-add-pretty-lambda)
    (prettify-symbols-mode 1)
    (evil-smartparens-mode)
    (pcre-mode)
    (lispy-mode t)
    (highlight-parentheses-mode)
    (spacemacs/toggle-automatic-symbol-highlight-on )))



(defun my-vue-mode-hook ()
  (progn
    (pcre-mode)
    ;; (tagedit-mode 1)
    (spacemacs/toggle-auto-completion-on)
    (spacemacs/toggle-automatic-symbol-highlight-on )))


(defun liurui/indent ()
  (interactive)
    (progn
    (indent-guide-mode)
    (if indent-guide-mode
        (progn
          (setq indent-guide-delay 0.4)
          (setq indent-guide-char "┆")
          (set-face-foreground 'indent-guide-face "#4e9376")))))


;; (defun my-web-mode-hook ()
;;   "Hook for `web-mode'."
;;   (set (make-local-variable 'company-backends)
;;        '(company-tern company-web-html company-yasnippet company-files)))

;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;; ;; Enable JavaScript completion between <script>...</script> etc.
;; (defadvice company-tern (before web-mode-set-up-ac-sources activate)
;;   "Set `tern-mode' based on current language before running company-tern."
;;   (message "advice")
;;   (if (equal major-mode 'web-mode)
;;       (let ((web-mode-cur-language
;;              (web-mode-language-at-pos)))
;;         (if (or (string= web-mode-cur-language "javascript")
;;                 (string= web-mode-cur-language "jsx")
;;                 )
;;             (unless tern-mode (tern-mode))
;;           (if tern-mode (tern-mode -1))))))

(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-web-html))
                           (company-mode t)))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
