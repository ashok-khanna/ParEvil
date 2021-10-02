# ParEvil
*An experimental set of keybindings to integrate ParEdit into Evil Mode*


**Last Updated: 2 October 2021**

ParEdit[^1] is one of the oldest and most famous structural editing tools for the Lisp-family of languages, and one that I felt most comfortable with (whether it be its classic design or simply muscle memory). It is perfectly viable for use in Evil Mode, particularly with the assistance of Evil Paredit[^2] to avoid accidental breakage of parentheses parity.

However, key sequences without modifiers feel much more natural in VIM / Evil Mode, and my aim has been to find a set of keybindings that will allow for a more natural integration of ParEdit with Evil Mode. Indeed, whilst there are many other excellent packages which aim to do this (see Lispy[^3], Lispyville[^4], Smartparens[^5], Evil Cleverparens[^6], Symex[^7], and more[^8]), it wouldn't be Emacs if we didn't end up writing our own packages for commonly-solved functionalities.

Thus, herein, is my current approach to structural editing within Evil Mode. You can download and load `parevil.el` into your Emacs configuration and feel free to send through any comments & suggestions.

```lisp
;; Add the below to your .emacs to use ParEvil and then evaluate it
;; Don't forget to save your .emacs!

(load "/path-to-parevil/parevil.el)
```

## Introduction
Modifying the standard bindings of VIM would be a foolish endeavour, and we will not do so as a starting principle. Rather, we create a new mode (***Paredit State***) as follows, which we can toggle from and to Normal Mode by pressing `spacebar`.

```lisp
;; Define Paredit State

(evil-define-state paredit
  "Paredit state."
  :tag " <P> "
  :enable (normal)
  :cursor (bar . 1))
  
  
;; Manage Normal / Paredit States

(define-key evil-paredit-state-map [escape] (lambda () (interactive) (evil-normal-state)))
(define-key evil-paredit-state-map (kbd "SPC") (lambda () (interactive) (evil-normal-state)))
(define-key evil-normal-state-map (kbd "SPC") (lambda () (interactive) (evil-paredit-state)))
```



## Standard Keybindings
As a starting point, our new mode inherits all of the keybindings from Normal mode. We then introduce the following bindings specific for common ParEdit commands. Any ParEdit command not listed below can be accessed by its default keybinding.

```lisp
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
(define-key evil-paredit-state-map "gs" 'paredit-splice-sexp)            ;; M-s      paredit-splice-sexp
(define-key evil-paredit-state-map "gw" 'paredit-wrap-round)             ;; M-(      Paredit Wrap Around
(define-key evil-paredit-state-map "gh" 'paredit-recenter-on-sexp) 
(define-key evil-paredit-state-map "g/" 'paredit-reindent-defun)
```

Note that the movement command above accept numeric prefix arguments. For example, we can move 4 sexps forward with `4f`.


## Additional Features
Perhaps somewhat lesser known is that a reasonable amount of structural editing commands are actually built directly into Emacs[^10]. We add the following keybindings to the above mix for transposing sexps. An argument to `transpose-sexp` serves as a repeat count, moving the previous expression over that many following ones. A negative argument moves the previous balanced expression backwards across those before it. An argument of zero, rather than doing nothing, transposes the balanced expressions ending at or after point and the mark.

```lisp
;; Accepts numeric prefix argument

(define-key evil-paredit-state-map "t" 'transpose-sexps)

;; For convenience, a reverse transpose can be achieved with T:

(define-key evil-paredit-state-map "T" (lambda () (interactive) (transpose-sexps -1)))
```

We also modify cut / copy / paste with the following to allow for single keystroke cuts & copies and also to fix the paste behaviour due to working before/after point (in paredit state) vs. on point (in normal mode). Note that we can cut / copy multiple lines with numeric prefixes such as `3s` and `3y` to cut or copy 3 lines respectively.

```lisp
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
```


Finally, we have added the following new functions to cut / copy all expressions within a group (from the cursor onwards). These hopefully should work okay, but let me know any unintended behaviour.

```lisp
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
```


## Comparing vs. Normal Mode - What did we break?
The above keybindings override the Normal mode keybindings when in Paredit State. Whilst that is not too much an issue as normal mode is simply one `spacebar` away, it is prudent to review what we are overriding. Below is my current reasoning for each override.

Key | Paredit State | Normal State | Notes
--- | ------------- | ------------ | ----------
`f` | `paredit-forward` | `find-char`| find-char is not that commonly used vs. paredit-forward


## Reliance on Evil Paredit
Finally, it is useful to maintain reliance on Evil Paredit to provide some safety against accidentally breaking balanced parantheses whilst in Normal / Paredit State.

```lisp
(load "/path-to-evil-paredit/evil-paredit.el")

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
(add-hook 'scheme-mode-hook 'evil-paredit-mode)
(add-hook 'slime-repl-mode-hook 'evil-paredit-mode)
```

All of the above code has been bundled into `parevil.el`, and you simply need to add the below to your `.emacs` after downloading ParEvil to use it.

```lisp
;; Add the below to your .emacs to use ParEvil and then evaluate it
;; Don't forget to save your .emacs!

(load "/path-to-parevil/parevil.el)
```

[^1]: Main Website: http://mumble.net/~campbell/emacs/paredit.el. Quick Reference: http://mumble.net/~campbell/emacs/paredit.html
[^2]: GitHub Repo: https://github.com/roman/evil-paredit
[^3]: GitHub Repo: https://github.com/abo-abo/lispy
[^4]: GitHub Repo: https://github.com/noctuid/lispyville
[^5]: GitHub Repo: https://github.com/Fuco1/smartparens
[^6]: GitHub Repo: https://github.com/luxbock/evil-cleverparens
[^7]: GitHub Repo: https://github.com/countvajhula/symex.el
[^8]: Let me know if you have a package you want to list here
[^10]: Reference:  https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html
