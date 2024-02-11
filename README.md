# Emacs Poetry
A Poetry porcelain for Emacs.

## Installation

### use-package and quelpa-use-package

```elisp
(use-package poetry-mode
  :quelpa (poetry-mode :fetcher github :repo "emil-vdw/emacs-poetry")
  :bind (("C-c m" . poetry-transient)))
```

## Features

- Activate/deactivate poetry environment.
- Jump to `pyproject.toml` file.
