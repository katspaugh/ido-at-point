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

### Alternative completion front-ends

If you fancy **helm** instead of **ido**, re-define the `ido-at-point-read` function:

    (require 'helm-mode)
    (defun ido-at-point-read (choices common)
      (helm-comp-read "" choices
                      :initial-input common
                      :must-match t))

### Compatibility

Works only in Emacs 24 and higher. Compatible with Emacs Lisp mode, Tern, Shell mode and hopefully many others.
