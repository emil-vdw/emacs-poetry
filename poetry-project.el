;;; poetry-project.el --- Project related functionality for poetry-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; This library implements project related functionality for poetry-mode.

;;; Code:
(defvar poetry-active-project nil
  "The attributes of the active poetry project.")

(defun poetry-project--root (&optional working-dir)
  "Return the directory of the current poetry project.

WORKING-DIR is the directory to start searching from. If nil, defaults to the
current working directory. If no poetry project is found, return nil.

Path returned is in the form \\\"/path/to/project\\\" (without a trailing slash)."
  (when-let* ((starting-dir (or working-dir "."))
              (project-root (locate-dominating-file starting-dir "pyproject.toml"))
              (pyproject-file-contents
               (with-temp-buffer
                 (insert-file-contents-literally
                  (concat (file-name-as-directory project-root) "pyproject.toml"))
                 (buffer-string))))

    ;; Just because we found a pyproject.toml file doesn't mean
    ;; it's a poetry project.
    (when (string-match "^\\[tool\\.poetry]$" pyproject-file-contents)
      project-root)))

(defun poetry-project--name (project-root)
  "Return the name of the current poetry project.

PROJECT-ROOT is the root directory of the project."
  (let ((pyproject-file (concat project-root "/" "pyproject.toml")))
    (with-temp-buffer
      (insert-file-contents pyproject-file)
      (goto-char (point-min))
      (if (re-search-forward "^name\\s-*=\\s-*\"\\(.*?\\)\"" nil t)
          (match-string 1)
        (error "Project name not found")))))

(defun poetry-project--goto-pyproject-root (&optional project-root)
  "Open the root directory of the current project in dired."
  (interactive)
  (when-let* ((project-root
               (or project-root (poetry-project--root))))
    (dired project-root)))

(defun poetry-project--goto-pyproject-file (&optional project-root)
  "Open the pyproject.toml file in the current project.

If PROJECT-ROOT is nil, defaults to the project for the current working
directory. If no project is found, do nothing."
  (interactive)
  (when-let* ((project-root
               (or project-root (poetry-project--root)))
              (pyproject-file (concat project-root "pyproject.toml")))
    (find-file pyproject-file)))

(provide 'poetry-project)
;;; poetry-project.el ends here
