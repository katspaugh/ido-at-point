# ido-at-point

Makes `completion-at-point` display possible completions via `ido–completing-read`.

![screenshot](http://i.imgur.com/MvTla9I.png)

### Installation

You can install the package from [MELPA](http://melpa.milkbox.net/) and autoload it:

    (autoload 'ido-at-point-mode "ido-at-point")

Otherwise, if installed manually:

    (require 'ido-at-point)

Then activate the mode:

    (ido-at-point-mode)

### Usage

Press `M-tab` or `C-M-i` to start completing.

### Options

Partial completion is *on* by default. If you don't want to get partial completions, set `ido-at-point-partial` to `nil`:

    (setq ido-at-point-partial nil)

### Compatibility

Works only in Emacs 24 and higher. Compatible with asynchronous completion requests (including Tern's completion for JavaScript).
