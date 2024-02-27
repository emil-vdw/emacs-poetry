;;; poetry.el --- A Poetry porcelain for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; URL: https://github.com/emil-vdw/poetry-mode
;; Keywords: Python, Tools
;; Package-Version: 0.1.0
;; Package-Requires: (
;;     (emacs "29.2")
;;     (transient "0.5.3")
;;     (dash "2.19.1"))

;;; Code:
(require 'cl-lib)

(require 'transient)
(require 'dash)

(require 'poetry-core)
(require 'poetry-project)
(require 'poetry-venv)

;;; Public API
(defcustom poetry-command "poetry"
  "The command to run poetry."
  :type 'string
  :group 'poetry)

(defcustom poetry-output-buffer "*Poetry*"
  "The buffer used to display the output of poetry commands."
  :type 'string
  :group 'poetry)

(defcustom poetry-process-name "poetry"
  "The name of the process used to run poetry commands."
  :type 'string
  :group 'poetry)

(defun poetry--transient-status-block ()
  "Return a string with the status of the current project."
  (if (not (or (poetry-project--root)
               poetry-active-project))
      ;; No project detected.
      (propertize "No project detected\n" 'face 'italic)

    (concat
     (propertize "Status:\n" 'face font-lock-keyword-face)
     ;; Active project.
     (when-let ((project-name (alist-get 'project-name poetry-active-project)))
       (format "  %s: %s\n"
               (propertize "Project" 'face font-lock-variable-name-face)
               project-name))

     ;; Active env.
     (when-let ((venv-name (alist-get 'venv-name poetry-active-project)))
       (format "  %s: %s\n"
               (propertize "Active env" 'face font-lock-variable-name-face)
               venv-name))

     ;; In some poetry project directory.
     (when-let* ((_ (not poetry-active-project))
                 (project-root (poetry-project--root))
                 (project-name (poetry-project--name project-root)))
       (format "  %s: %s\n"
               (propertize "Project detected" 'face font-lock-variable-name-face)
               project-name)))))

;;;###autoload (autoload 'poetry-transient "poetry" nil t)
(transient-define-prefix poetry-transient ()
  "Poetry menu."
  [:description poetry--transient-status-block

   [:description
    "Environment"
    ("a" "Activate" poetry-venv-activate
     :inapt-if
     (lambda () (or poetry-active-project
                    (not (poetry-project--root)))))
    
    ("d" "Deactivate" poetry-venv-deactivate
     :inapt-if-nil
     poetry-active-project)

    ("r" "Eglot reconnect" poetry-eglot-reconnect
     :inapt-if-not eglot-managed-p)]

   [:description
    "Project"
    ("f" "Jump to pyproject.toml file" poetry-project--goto-pyproject-file
     :inapt-if-not (lambda ()
                     (or poetry-active-project
                         (poetry-project--root))))

    ("p" "Open project root directory" poetry-project--goto-pyproject-root
     :inapt-if-not (lambda ()
                     (or poetry-active-project
                         (poetry-project--root))))]])

(provide 'poetry)
;;; poetry.el ends here
