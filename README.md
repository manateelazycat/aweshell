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
9. Integrate `eshell-up'.
10. Unpack archive file.
11. Open file with alias e.
12. Output "did you mean ..." helper when you typo.
13. Make cat file with syntax highlight.
14. Alert user when background process finished or aborted.
15. IDE-like completion for shell commands.

# Installation

Put `aweshell.el`, `esh-autosuggest.el`, `eshell-prompt-extras.el`, `exec-path-from-shell.el` to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'aweshell)
```

Bind your favorite key to functions:

# Usage

| Commands                    | Description                                                   |
|-----------------------------|---------------------------------------------------------------|
| ```aweshell-new```          | create a new eshell buffer                                    |
| ```aweshell-next```         | switch to next aweshell buffer                                |
| ```aweshell-prev```         | switch to previous aweshell buffer                            |
| ```aweshell-clear-buffer``` | clear eshell buffer                                           |
| ```aweshell-sudo-toggle```  | toggle sudo                                                   |
| ```aweshell-toggle```       | switch back and forth between eshell buffer and normal buffer |


# Customize

## Variables

Customize variables  by:
```Elisp
M-x customize-group RET aweshell RET
```

| Variable                                | Description                                                            |
|-----------------------------------------|------------------------------------------------------------------------|
| ```aweshell-complete-selection-key```   | Key used for complete selected candidate                               |
| ```aweshell-clear-buffer-key```         | Key used to clear buffer (like <kbd>C-l</kbd> in traditional terminal) |
| ```aweshell-sudo-toggle-key```          | Key used to toggle sudo                                                |
| ```aweshell-use-exec-path-from-shell``` | Whether to use exec-path-from-shell to setup environment               |
| ```aweshell-autosuggest-frontend```     | Front end for displaying autosuggest                                   |

## Customize shell prompt

Aweshell uses eshell-prompt-extra to prettify shell prompt.
Consult [eshell-prompt-extra's README](https://github.com/kaihaosw/eshell-prompt-extras#themes) on how to customize shell prompt.

## Customize eshell-up

```Elisp
(setq eshell-up-ignore-case nil)
(setq eshell-up-print-parent-dir t)
```

Checkout [homepage of eshell-up](https://github.com/peterwvj/eshell-up) for more information.


## More on autosuggest front end

By default, company is used for fish-like auto suggestion,
if you want to use company for completion,
change ```aweshell-autosuggest-frontend``` from ```'company``` to ```'custom```.

<img src="./company-style-completion.png">

## Aliases

Suggested alias for [eshell-up](https://github.com/peterwvj/eshell-up) and other eshell commands:

Put in alias file:
```
alias up eshell-up $1
alias pk eshell-up-peek $1
alias ff find-file $1
alias ll ls -al
alias dd dired $1
alias fo find-file-other-window $1
alias gs magit-status
```
