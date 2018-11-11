# What is awesome-pair.el ?
awesome-pair.el is a plugin that provides grammatical parenthesis completion.

## Installation
Clone or download this repository (path of the folder is the `<path-to-awesome-pair>` used below).

In your `~/.emacs`, add the following two lines:
```Elisp
(add-to-list 'load-path "<path-to-awesome-pair>") ; add awesome-pair to your load-path
(require 'awesome-pair)
```

## Enable and binding keys
Not all programming languages ​​are suitable for parenthesis auto-completion.
You can add awesome-pair.el to the programming language mode like below:

```Elisp
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode 1))))
```

Then binding below awesome-pair.el commands with below keystrokes:

```Elisp
(global-set-key (kbd "(") 'awesome-pair-open-round)
(global-set-key (kbd "[") 'awesome-pair-open-bracket)
(global-set-key (kbd "{") 'awesome-pair-open-curly)
(global-set-key (kbd ")") 'awesome-pair-close-round)
(global-set-key (kbd "]") 'awesome-pair-close-bracket)
(global-set-key (kbd "}") 'awesome-pair-close-curly)

(global-set-key (kbd "%") 'awesome-pair-match-paren)
(global-set-key (kbd "\"") 'awesome-pair-double-quote)

(global-set-key (kbd "M-o") 'awesome-pair-backward-delete) 
(global-set-key (kbd "C-k") 'awesome-pair-kill)

(global-set-key (kbd "M-\"") 'awesome-pair-wrap-double-quote) 
(global-set-key (kbd "M-[") 'awesome-pair-wrap-bracket)
(global-set-key (kbd "M-{") 'awesome-pair-wrap-curly)
(global-set-key (kbd "M-(") 'awesome-pair-wrap-round)
(global-set-key (kbd "M-)") 'awesome-pair-unwrap)

(global-set-key (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline) 
```
