# Emacs Poetry
Dead simple Python Poetry virtualenv integration and utilities.

## Philosophy

This package is not intended to replace the Poetry CLI, only
complement It. It just adds some utilities to make it easier to work
on Poetry projects in Emacs.

If you want to install dependencies, lock, etc. use the CLI. If you
want to activate/deactivate the virtualenv, jump to the
`pyproject.toml` file, etc. use this package.

## Features

![2024-02-29-115149](https://github.com/emil-vdw/emacs-poetry/assets/33003253/f2d9c3e4-39db-4be7-adba-387b36df6f2a)

- Activate/deactivate poetry environment.
- Jump to the active project's `pyproject.toml` file.
- Jump to the active project's directory.
- Integrates with `eglot` to reconnect manually/on virtualenv change
  when in an `eglot` managed buffer.

## Installation

### Using straight

```elisp
(straight-use-package '(poetry :type git :host github :repo "emil-vdw/emacs-poetry"))
```

### Straight and use-package

```elisp
(use-package
  poetry
  :straight
  (poetry :type git :host github :repo "emil-vdw/emacs-poetry")
  :bind (("C-c m" . poetry-transient)))
```
