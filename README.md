<img src="./aweshell.gif">

# What is aweshell?

I created `multi-term.el` and use it many years.

Now I'm a big fans of `eshell`.

So I wrote `aweshell.el` to extend `eshell` with these features:

1. Create and manage multiple eshell buffers.
2. Add some useful commands, such as: clear buffer, toggle sudo etc.
3. Display extra information and color like zsh, powered by `eshell-prompt-extras'
4. Add Fish-like history autosuggestions.
5. Validate and highlight command before post to eshell.
6. Change buffer name by directory change.
7. Add completions for git command.
8. Fix error `command not found' in MacOS.
9. Integrate `eshell-up'.
10. Unpack archive file.
11. Open file with alias e.
12. Output "did you mean ..." helper when you typo.
13. Make cat file with syntax highlight.
14. Alert user when background process finished or aborted.
15. Complete shell command arguments like IDE feeling.
16. Dedicated shell window like IDE bottom terminal window.

# Installation

Put `aweshell.el`, `eshell-prompt-extras.el`, `exec-path-from-shell.el` to your load-path.
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
aweshell-switch-buffer
aweshell-dedicated-toggle
aweshell-dedicated-open
aweshell-dedicated-close
```

## Installing with Quelpa

If you prefer to use a package manager, you can use [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```Elisp
(use-package aweshell
  :quelpa (abc-mode :fetcher github :repo "manateelazycat/aweshell"))
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
aweshell-use-exec-path-from-shell
aweshell-dedicated-window-height
```

Customize prompt as directed in [eshell-prompt-extras' README](https://github.com/kaihaosw/eshell-prompt-extras#themes).

## Dedicated window
You can use command ```aweshell-dedicated-toggle``` to pop dedicated window at bottom of frame.

<img src="./aweshell-dedicated.gif">

## Aliases

[eshell-up](https://github.com/peterwvj/eshell-up)

In alias file:
```
alias up eshell-up $1
alias pk eshell-up-peek $1
```

Other customization of eshell-up:
```Elisp
(setq eshell-up-ignore-case nil)
(setq eshell-up-print-parent-dir t)
```

### FAQ
If you got error that random space insert, you perhaps need turn off ```aweshell-auto-suggestion-p``` with ```(setq aweshell-auto-suggestion-p nil)```, meantime auto suggestion feature will turn off.
