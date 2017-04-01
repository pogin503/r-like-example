r-like-example
==============

### Overview

This program display examples of functions on other buffer.

This can be able to improve learning Emacs Lisp effectively. 

### Installation

Clone to your .emacs.d path.

```lisp
git clone https://github.com/pogin503/r-like-example.git 
```

Install dependency.

```
M-x package-install f
```

```lisp
(add-to-list 'load-path "~/path/to/r-like-example")

(require 'r-like-example)
(require 'elisp-examples)
(ex-set-keybindings)
```

I recommend specially to use popwin.el. 

```
M-x package-install popwin RET
```

Add below setting to your init.el

```lisp
(push '("*example*" :position right :width 45 :stick t) popwin:special-display-config)
```


Basic Usage
===========

### Learning function usage

You type `M-9 mapcar`,


###Keybindings

| keybind    | description    |
|:-----------|:---------------|
| M-9 or s-9 | Display examples |
| M-0 or s-0 | Store function examples |
| C-c 0 a    | Add current cursor position S expression |
| C-c 0 i    | Insert examples data in current buffer |
| C-c 0 p    | Add current cursor position S expression into specific key |
| C-c 0 d    | Pop specific key example |
| C-c 0 u    | Display the key data of the unsaved to the mini-buffer |

Basic Commands
==============

### Command `ex-example`

Dispaly function examples to `*example*` buffer.

### Command `ex-add-example`

Add example on cursor position. 

### Command `ex-insert-current-buffer`

Insert example data like:

```lisp
(ex-put-example 'type-of '("(type-of 1)"
"(type-of 'nil)"
"(type-of '())    ; () is nil."
"(type-of '(x)") t)
```

in current buffer.

### Command `ex-display-unstored-data`

Display unstored example keys 
