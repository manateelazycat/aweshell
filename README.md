# What is aweshell?

I created `multi-term.el` and use it many years.

Now I'm a big fans of `eshell`.

So I wrote `aweshell.el` to extend `eshell` with these features:

1. Create and manage multiple eshell buffers.
2. Add some useful commands, such as: clear buffer, toggle sudo etc.
3. Display extra information and color like zsh, powered by `eshell-prompt-extras'
4. Add Fish-like history autosuggestions, powered by `esh-autosuggest', support histories from bash/zsh/eshell.
5. Validate and highlight command before post to eshell.
6. Change buffer name by directory change.
7. Add completions for git command.
8. Fix error `command not found' in MacOS.

# Installation

Put `aweshell.el`, `esh-autosuggest.el`, `eshell-prompt-extras.el`, `exec-path-from-shell.el` to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'aweshell)
```

Bind your favorite key to functions:

```Elisp
aweshell-new
aweshell-next
aweshell-prev
aweshell-clear-buffer
aweshell-sudo-toggle
```

# Customize

Customize variables below by:
```Elisp
M-x customize-group RET aweshell RET
```

```Elisp
aweshell-complete-selection-key
aweshell-clear-buffer-key
aweshell-sudo-toggle-key
```

Customize prompt as directed in [eshell-prompt-extras' README](https://github.com/kaihaosw/eshell-prompt-extras#themes).
