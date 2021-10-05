# ParEvil
*An experimental set of keybindings to integrate ParEdit into Evil Mode. The general concept IMO is good (a separate 'Paredit State'), the keybindings however are personal preference and even for me in a state of flux. No perfect solution, but see what works best for you and customise to your hearts content :-)*


**Last Updated: 2 October 2021**

ParEdit[^1] is one of the oldest and most famous structural editing tools for the Lisp-family of languages, and one that I felt most comfortable with (whether it be its classic design or simply muscle memory). It is perfectly viable for use in Evil Mode, particularly with the assistance of Evil Paredit[^2] to avoid accidental breakage of parentheses parity.

However, key sequences without modifiers feel much more natural in VIM / Evil Mode, and my aim has been to find a set of keybindings that will allow for a more natural integration of ParEdit with Evil Mode. Indeed, whilst there are many other excellent packages which aim to do this (see Lispy[^3], Lispyville[^4], Smartparens[^5], Evil Cleverparens[^6], Symex[^7], and more[^8]), it wouldn't be Emacs if we didn't reinvent the wheel and end up writing our own keybinds.

Thus, herein, is my current approach to structural editing within Evil Mode. You can download and load `parevil.el` into your Emacs configuration and feel free to send through any comments & suggestions.

```lisp
;; Add the below to your .emacs to use ParEvil and then evaluate it
;; Don't forget to save your .emacs!

(load "/path-to-parevil/parevil.el)
```

## ParEvil Summary
Modifying the standard bindings of VIM would be a foolish endeavour, and we will not do so as a starting principle. Rather, we create a new mode (***Paredit State***) as follows, which we can toggle from and to Normal Mode by pressing `spacebar`. Note that we only bind `spacebar` within Normal mode for lisp-related major modes, so you are free to bind `spacebar` to something else for other major modes. As a starting point, our new mode inherits all of the keybindings from Normal mode. We then introduce the following bindings specific for common ParEdit commands. Any ParEdit command not listed below can be accessed by its default keybinding.

Key  | Function in Paredit State     | Key  | Function in Paredit State 
---- | ----------------------------- | ---- | ----------
`f`  | `paredit-forward`             | `(`  | `paredit-backward-slurp-sexp`
`b`  | `paredit-backward`            | `)`  | `paredit-forward-slurp-sexp`
`n`  | `paredit-forward-down`        | `{`  | `paredit-backward-barf-sexp`
`w`  | `paredit-backward-up`         | `}`  | `paredit-forward-barf-sexp`
`q`  | `beginning-of-defun`          | `gr` | `paredit-raise-sexp`
`gn` | `paredit-forward-up`          | `gl` | `paredit-splice-sexp` 
`gp` | `paredit-backward-down`       | `gw` | `paredit-wrap-round`
`gh` | `paredit-recenter-on-sexp`    | `gn` | `indent-region`    
`t`  | `transpose-sexps`             | `T`  | `transpose-sexps -1`
`p`  | `Custom Paste`                | 'se' | `mark-sexp` (i.e. select sexp)
`sd` | `Custom Sexp Cut`             | `sy` | `Custom Sexp Copy` 
`gs` | `Custom Multi Cut`            | `gy` | `Custom Multi Copy` 
 
Note that the some of the above commands accept numeric prefix arguments. For example, we can move 4 sexps forward with `4f`.

## Note on Uncommon Features
Perhaps somewhat lesser known is that a reasonable amount of structural editing commands are actually built directly into Emacs[^10]. One such command is `transpose-sexps` which we add with the following. The trick to transposing is to have your cursor right after an sexp that you wish to transpose with the one before or after it.

```lisp
;; Accepts numeric prefix argument

(define-key evil-paredit-state-map "t" 'transpose-sexps)

;; For convenience, a reverse transpose can be achieved with T:

(define-key evil-paredit-state-map "T" (lambda () (interactive) (transpose-sexps -1)))
```

Separately, we modify cut / copy / paste with the following to allow for single keystroke cuts & copies and also to fix the paste behaviour due to working before/after point (in paredit state) vs. on point (in normal mode). Note that we can cut / copy multiple lines with numeric prefixes such as `3s` and `3y` to cut or copy 3 lines respectively.

```lisp
(define-key evil-paredit-state-map "sd"
  (lambda (arg)
    (interactive "p")
    (mark-sexp arg)
    (kill-region (mark) (point) 'region)))

;; Single Keystroke Copy

(define-key evil-paredit-state-map "sy"
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

[^1]: Main Website: http://mumble.net/~campbell/emacs/paredit.el. Quick Reference: http://mumble.net/~campbell/emacs/paredit.html
[^2]: GitHub Repo: https://github.com/roman/evil-paredit
[^3]: GitHub Repo: https://github.com/abo-abo/lispy
[^4]: GitHub Repo: https://github.com/noctuid/lispyville
[^5]: GitHub Repo: https://github.com/Fuco1/smartparens
[^6]: GitHub Repo: https://github.com/luxbock/evil-cleverparens
[^7]: GitHub Repo: https://github.com/countvajhula/symex.el
[^8]: Let me know if you have a package you want to list here
[^10]: Reference:  https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html
