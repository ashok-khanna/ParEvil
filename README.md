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

# Introduction
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
# Keybindings
As a starting point, our new mode inherits all of the keybindings from Normal mode. We then introduce the following bindings specific for ParEdit commands.

```lisp

```


# Additional Features
Perhaps somewhat lesser known is that a reasonable amount of structural editing commands are actually built directly into Emacs[^10]. We add the following keybindings to the above mix.

```lisp



```



# Reliance on Evil Paredit
Finally, it is useful to maintain reliance on Evil Paredit to provide some safety against accidentally breaking balanced parantheses whilst in Normal / Paredit State.

```lisp
(load "/path-to-evil-paredit/evil-paredit.el")

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
(add-hook 'scheme-mode-hook 'evil-paredit-mode)
(add-hook 'slime-repl-mode-hook 'evil-paredit-mode)
```

But don't worry, Evil Paredit and all of the above code has been bundled into `parevil.el`, and you simply need to add the below to your `.emacs` after downloading ParEvil to use it.

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
