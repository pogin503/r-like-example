r-like-example
==============

### Overview

This program display example of functions on other buffer.

This can be able to improve lerning efficiency. 

### Installation

```lisp
(add-to-list 'load-path "~/path/to/r-like-example")

(require 'r-like-example)
(require 'elisp-example)
```

I recommend specially to use popwin.el. 

When you use emacs24, `M-x package-install popwin RET` 

```lisp
(add-to-list 'popwin:special-display-config '("*example*" :position right :width 45 :dedicated t))
```

```lisp
(global-set-key (kbd "M-9") 'ex-example)
(global-set-key (kbd "s-9") 'ex-example) ;; for Mac keybind
(global-set-key (kbd "M-0") 'ex-insert-current-buffer)
(global-set-key (kbd "s-0") 'ex-insert-current-buffer)
(global-set-key (kbd "C-c 0") 'ex-add-example)
```

Basic Usage
===========

### Learning function usage

You type `M-9 mapcar`,

### For Development



Basic Commands
==============

### Command `ex-example`

Dispaly function example to `*example*` buffer.

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
