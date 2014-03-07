# ido-at-point

Makes `completion-at-point` display completion candidates using the **ido** prompt.

![screenshot](http://i.imgur.com/MvTla9I.png)

### Installation

You can install the package from [MELPA](http://melpa.milkbox.net/) and autoload it:

    (autoload 'ido-at-point-mode "ido-at-point")

Otherwise, if installed manually:

    (require 'ido-at-point)

Then activate the mode:

    (ido-at-point-mode)

### Usage

Press `M-tab` or `C-M-i` to start completion.

### Options

Partial completion is *on* by default. If you don't want to get partial completions, set `ido-at-point-partial` to `nil`:

    (setq ido-at-point-partial nil)

Fuzzy matching is *off* by default. If you want fuzzy matching, set `ido-at-point-fuzzy` to `t`:

    (setq ido-at-point-fuzzy t)

If you want to use **helm** instead of **ido**, set `ido-at-point-use-helm` to `t`. Please note that since this is optional, you'll need to manage the helm dependency manually.

    (require 'helm-mode)
    (setq ido-at-point-use-helm t)

### Compatibility

Works only in Emacs 24 and higher. Compatible with Emacs Lisp mode, Tern, Shell mode and hopefully many others.

If you use Emacs 24.4 (the next release), checkout the branch [*next*](https://github.com/katspaugh/ido-at-point/tree/next).
