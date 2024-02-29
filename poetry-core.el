;;; poetry-core.el --- The core functionality of poetry-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; This library implements the core functionality of poetry-mode.

;;; Code:


;;; Internals
(defun poetry--run-command-sync (args)
  "Run a Poetry command with ARGS and return the output as a string."
  (shell-command-to-string
   (format "%s %s" poetry-command (s-join " " args))))

(provide 'poetry-core)
;;; poetry-core.el ends here
