;;;; Dependencise

(require 'evil)
(require 'paredit)

;;;; Part A -- Evil Paredit Code


;;; evil-paredit.el --- Paredit support for evil keybindings
;;
;; Copyright (C) 2012 Roman Gonzalez
;;
;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Keywords: paredit, evil
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;; Version: 0.0.2

;; URL: https://github.com/roman/evil-paredit

;; Package-Requires: ((evil "1.0.9") (paredit "25beta"))

;;; Code:

;;;###autoload
(define-minor-mode evil-paredit-mode
  "Minor mode for setting up Evil with paredit in a single buffer"
  :keymap '()
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)))

(defun -evil-paredit-check-region (beg end)
  (if (fboundp 'paredit-check-region-state)
      (if (and beg end)
          ;; Check that region begins and ends in a sufficiently similar
          ;; state, so that deleting it will leave the buffer balanced.
          (save-excursion
            (goto-char beg)
            (let* ((state (paredit-current-parse-state))
                   (state* (parse-partial-sexp beg end nil nil state)))
              (paredit-check-region-state state state*))))
    (paredit-check-region-for-delete beg end)))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (-evil-paredit-check-region beg end)
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-yank beg end type register)))

(evil-define-operator evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (delete-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-delete beg end
                         type register yank-handler)))

(defun evil-paredit-kill-end ()
  "Returns the position where paredit-kill would kill to"
  (when (paredit-in-char-p)             ; Move past the \ and prefix.
    (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let* ((eol (point-at-eol))
         (end-of-list-p (save-excursion
                          (paredit-forward-sexps-to-kill (point) eol))))
    (if end-of-list-p (progn (up-list) (backward-char)))
    (cond ((paredit-in-string-p)
           (if (save-excursion (paredit-skip-whitespace t (point-at-eol))
                               (eolp))
               (kill-line)
             (save-excursion
               ;; Be careful not to split an escape sequence.
               (if (paredit-in-string-escape-p)
                   (backward-char))
               (min (point-at-eol)
                    (cdr (paredit-string-start+end-points))))))
          ((paredit-in-comment-p)
           eol)
          (t (if (and (not end-of-list-p)
                      (eq (point-at-eol) eol))
                 eol
               (point))))))

(evil-define-operator evil-paredit-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-paredit-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-paredit-change-line
  (beg end type register yank-handler)
  "Change to end of line respecting parenthesis."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-change beg end type register yank-handler)))

(defun evil-paredit-change-whole-line ()
  "Change whole line."
  (interactive)
  (beginning-of-line)
  (evil-paredit-change-line nil nil)
  (indent-according-to-mode))

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
  (kbd "D") 'evil-paredit-delete-line
  (kbd "C") 'evil-paredit-change-line
  (kbd "S") 'evil-paredit-change-whole-line
  (kbd "Y") 'evil-paredit-yank-line
  (kbd "X") 'paredit-backward-delete
  (kbd "x") 'paredit-forward-delete)

(provide 'evil-paredit)

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
(add-hook 'scheme-mode-hook 'evil-paredit-mode)
(add-hook 'slime-repl-mode-hook 'evil-paredit-mode)


;;; evil-paredit.el ends here


;;;; Part B - ParEvil Code

;; EVIL / Paredit Bindings

;; Cut all remaining expressions in a group

(define-key evil-normal-state-map "gs"
  (lambda ()
    (interactive)
    (let ((starting-point (point))
	  (ending-point nil))
      (save-excursion
	(paredit-backward-up)
	(evil-jump-item)
	(setq ending-point (point)))
      (kill-region starting-point ending-point))))

;; Copy all remaining expressions in a group

(define-key evil-normal-state-map "gy"
  (lambda ()
    (interactive)
    (let ((starting-point (point))
	  (ending-point nil))
      (save-excursion
     	(paredit-backward-up)
	(evil-jump-item)
	(setq ending-point (point)))
      (kill-ring-save starting-point ending-point))))


(evil-define-state paredit
  "Paredit state."
  :tag " <P> "
  :enable (normal)
  :cursor (bar . 1))


;; Escape & Lisp States
(define-key evil-paredit-state-map [escape] (lambda () (interactive) (evil-normal-state)))
(define-key evil-paredit-state-map (kbd "SPC") (lambda () (interactive) (evil-normal-state)))

(defun evil-paredit-normal-state-local-map ()
  (define-key evil-normal-state-local-map (kbd "SPC") (lambda () (interactive) (evil-paredit-state))))

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-normal-state-local-map)
(add-hook 'lisp-mode-hook 'evil-paredit-normal-state-local-map)
(add-hook 'lisp-interaction-mode-hook 'evil-paredit-normal-state-local-map)
(add-hook 'scheme-mode-hook 'evil-paredit-normal-state-local-map)
(add-hook 'slime-repl-mode-hook 'evil-paredit-normal-state-local-map)


;; Lisp Keys

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html

;; Motion Commands

(define-key evil-paredit-state-map "f" 'paredit-forward)                 ;; C-M-f    paredit-forward
(define-key evil-paredit-state-map "b" 'paredit-backward)                ;; C-M-b    paredit-backward
(define-key evil-paredit-state-map "d" 'paredit-forward-down)            ;; C-M-d    paredit-forward-down
(define-key evil-paredit-state-map "u" 'paredit-backward-up)             ;; C-M-u    paredit-backward-up
(define-key evil-paredit-state-map "n" 'paredit-forward-up)              ;; C-M-n    paredit-forward-up
(define-key evil-paredit-state-map "gp" 'paredit-backward-down)          ;; C-M-p    paredit-backward-down
(define-key evil-paredit-state-map "a" 'beginning-of-defun)              ;; C-M-a    beginning-of-defun

;; Alternate Undo

(define-key evil-paredit-state-map "gu" 'evil-undo)           

;; Slurp / Barf

(define-key evil-paredit-state-map "(" 'paredit-backward-slurp-sexp)     ;; C-(      paredit-backward-slurp-sexp    
(define-key evil-paredit-state-map ")" 'paredit-forward-slurp-sexp)      ;; C-)      paredit-forward-slurp-sexp
(define-key evil-paredit-state-map "{" 'paredit-backward-barf-sexp)      ;; C-{      paredit-backward-barf-sexp
(define-key evil-paredit-state-map "}" 'paredit-forward-barf-sexp)       ;; C-}      paredit-forward-barf-sexp

;; Other Commands

(define-key evil-paredit-state-map "gr" 'paredit-raise-sexp)             ;; M-r      paredit-raise-sexp
(define-key evil-paredit-state-map "gl" 'paredit-splice-sexp)            ;; M-s      paredit-splice-sexp
(define-key evil-paredit-state-map "gw" 'paredit-wrap-round)             ;; M-(      Paredit Wrap Around
(define-key evil-paredit-state-map "gh" 'paredit-recenter-on-sexp) 
(define-key evil-paredit-state-map "g/" 'paredit-reindent-defun)

;; Transpose Sexps

(define-key evil-paredit-state-map "t" 'transpose-sexps)
(define-key evil-paredit-state-map "T" (lambda () (interactive) (transpose-sexps -1)))

  
;; Cut / Copying in Evil Paredit -- C-M-SPC is mark-sexp in paredit mode


;; Notes here https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Call.html
;; Credits Lampilelo on #Emacs for kill-ring-save mark / point
;; p is for prefix-arg


;; Single Keystroke Cut

(define-key evil-paredit-state-map "s"
  (lambda (arg)
    (interactive "p")
    (mark-sexp arg)
    (kill-region (mark) (point) 'region)))

;; Single Keystroke Copy

(define-key evil-paredit-state-map "y"
  (lambda (arg)
    (interactive "p")
    (mark-sexp arg)
    (kill-ring-save (mark) (point) 'region)
    (message "Copied sexps")))  

;; Fix pasting in evil-paredit-state

(define-key evil-paredit-state-map "p"
  (lambda ()
    (interactive)
    (evil-paste-before 1)
    (forward-char 1)))

