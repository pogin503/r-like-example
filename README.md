r-like-example
==============

### Overview

This program display examples of functions on other buffer.

This can be able to improve learning Emacs Lisp effectively. 

### Installation

+Clone to your .emacs.d path.

```lisp
git clone https://github.com/pogin503/r-like-example.git 
```

```
M-x package-install f
```

```lisp
(add-to-list 'load-path "~/path/to/r-like-example")

(require 'r-like-example)
(require 'elisp-example)
```

I recommend specially to use popwin.el. 

When you use emacs24, `M-x package-install popwin RET` 

```lisp
(push '("*example*" :position right :width 45 :stick t) popwin:special-display-config)
```

```lisp
(global-set-key (kbd "s-9") 'ex-example)
(global-set-key (kbd "M-9") 'ex-example)
(global-set-key (kbd "s-0") 'ex-store-key-example)
(global-set-key (kbd "M-0") 'ex-store-key-example)
(global-set-key (kbd "C-c 0 a") 'ex-add-example)
(global-set-key (kbd "C-c 0 i") 'ex-insert-current-buffer)
(global-set-key (kbd "C-c 0 p") 'ex-put-to-example)
(global-set-key (kbd "C-c 0 d") 'ex-delete-last-elem)
```

Basic Usage
===========

### Learning function usage

You type `M-9 mapcar`,



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
"(type-of '(x)"))
```

in current buffer.
