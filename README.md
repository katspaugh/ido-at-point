# ido-at-point

Makes `completion-at-point` display possible completions via `ido–completing-read`.

![screenshot](http://i.imgur.com/MvTla9I.png)

### Installation

    (autoload 'ido-at-point-setup "ido-at-point")
    (ido-at-point-setup)

### Usage

Press `M-tab` or `C-M-i` to start completing.

### Compatibility

Works only in Emacs 24 and higher. Compatible with asynchronous completion requests (including Tern's completion for JavaScript).

