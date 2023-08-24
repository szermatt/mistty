# MisTTY, a shell/comint alternative with a real terminal

MisTTY runs a shell interactively under Emacs 29.1 and later, just like `M-x shell` does. 

In a MisTTY buffer, just like in a normal shell buffer, the usual
native Emacs movement and editing work. What *also* work is everything
that you normally only have in a terminal, such as TAB-completion and
native shell history.

MisTTY works well with Bash and ZSH, but it is especially well suited
to running [Fish](https://fishshell.com): you get autosuggestions,
completion in full colors. With
[TIDE](https://github.com/IlanCosman/tide) you also get asynchronous
prompts. You get all that in Emacs buffer, together with the usual
Emacs commands!

## INSTALLATION

Here's one way of installing MisTTY and binding it to the key `C-c s`:

```elisp
(package-vc-install "https://github.com/szermatt/mistty")
(use-package mistty
  :bind (("C-c s" . mistty)
  
         ;; bind here the shortcuts you'd like the 
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map
         ;; all shells: go up/down in the shell history
         ("C-p" . mistty-send-key)
         ("C-r" . mistty-send-key)
         ;; bash: history-token-search-backward
         ("M-." . mistty-send-key)
         ;; fish: dir history, more history manipulation
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))
```

Binding keys is of course optional, but strongly recommended.

## USAGE

Type `M-x mistty` to launch a new shell buffer in MisTTY mode, then
use it as you would comint.

You'll quickly notice some differences. For example TAB completion
working just like in a terminal instead of relying of Emacs
completion.

Commands that takes the whole screen such as `less` or `vi` take you 
into terminal mode for the duration of that command. You can still 
access previous commands in the "scrollback" MisTTY buffer by typing 
`C-c C-j`. 

If you ever get into a situation where a command needs you to press 
keys normally sent to Emacs, such as the arrow keys, press `C-c C-q`. 
It'll send all key strokes directly to the terminal until you exit 
the mode by pressing `C-g`. To send a single key to the terminal 
you can also press `C-q <key>` instead.

If you call `M-x mistty` another time, you'll be taken back to any
existing MisTTY buffer - see its documentation. If you don't like this
behavior, use `M-x mistty-create` instead.

### Configuration

You'll very likely want to enable directory tracking, so Emacs knows
what directory you're currently visiting. If you SSH into other
machines, you'll want to also enable TRAMP paths.

If you only run bash locally inside of a MisTTY buffer, you don't have
to do anything. It'll just work. Otherwise, read on.

The best way of enabling directory tracking is to configure your shell
to send out OSC 7 sequence as part of the prompt. 

You might add the following to your `~/.bashrc` (for Bash) or
`~/.zshrc` (for ZSH):

```bash
PS1='\e]7;file://$HOSTNAME$PWD\e\\\\'$PS1
```

Or from a function called from PROMPT_COMMAND:
```bash
PROMPT_COMMAND=my_prompt

function my_prompt {
  printf "\e]7;file://%s%s\e\\\\" "$HOSTNAME" "$PWD"
  ...
```

If you prefer the fish shell, add the following to `~/.local/config/fish/config.fish`
```fish
    function osc7_send_pwd --on-event fish_prompt
      printf "\e]7;file://%s%s\e\\\\" (hostname) "$PWD"
    end
```

If you know TRAMP works and you often SSH into hosts, consider
enabling such remote paths reported by the shell `M-x
customize-option` `mistty-allow-tramp-paths`

## COMPATIBILITY

MisTTY requires Emacs 29.1 or later.
