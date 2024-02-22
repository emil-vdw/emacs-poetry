;;; poetry-venv.el --- The Poetry virtual environment manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; This library implements a virtual environment manager for Poetry.

;;; Code:
(defvar poetry-venv--original-env-vars
  `(("PYTHONHOME" . ,(getenv "PYTHONHOME"))
    ("VIRTUAL_ENV" . nil))
  "The base environment variables to be set when activating a virtualenv.")

(defun poetry-eglot-reconnect ()
  "Reconnect to the eglot server."
  (interactive)
  (when (eglot-managed-p)
    (eglot-reconnect (eglot--current-server-or-lose))))

(defun poetry--venv-path ()
  "Return the path to the virtualenv associated with the current project."
  (save-match-data
    (when-let* ((venv-path-pattern "^\\(\/[-\.a-zA-Z0-9_]+\\)+$")
                (venv-command-output
                 (poetry--run-command-sync
                  '("env" "info" "-p")))
                (venv-path-match
                 (string-match venv-path-pattern venv-command-output)))
      (match-string 0 venv-command-output))))

(defun poetry-venv-activate ()
  "Activate the virtualenv associated with the current project.

This function sets the environment variables for the virtualenv
and adds the virtualenv bin directory to the PATH. It also sets
`poetry-active-project' to the active project attributes."
  (interactive)

  (unless (poetry-project--root)
    (error "Not in a poetry project"))

  (when poetry-active-project
    (poetry-venv-deactivate))

  (let* ((project-dir (poetry-project--root))
         (venv-path (poetry--venv-path))
         (venv-bin-dir (concat venv-path "/bin"))
         (project-name (poetry-project--name project-dir)))

    (unless venv-path
      (error "No virtualenv found for the current project"))

    ;; Save the original environment variables.
    (setq poetry-venv--original-env-vars
          (-map (lambda (env-var-item)
                  `(,(car env-var-item) . ,(getenv (car env-var-item))))
                poetry-venv--original-env-vars))

    ;; Set the environment variables for the virtualenv.
    (setenv "VIRTUAL_ENV" venv-path)
    (setenv "PYTHONHOME" nil)
    (setenv "PATH"
            (string-join
             (cons venv-bin-dir (split-string (getenv "PATH") ":")) ":"))

    ;; Add the virtualenv bin directory to the PATH.
    (push venv-bin-dir exec-path)

    ;; Set the active project attributes.n
    (setq poetry-active-project
          `((project-name . ,project-name)
            (project-dir . ,project-dir)
            (venv-path . ,venv-path)
            (venv-name . ,(file-name-nondirectory venv-path))
            (venv-bin . ,venv-bin-dir)))

    (poetry-eglot-reconnect)))

(defun poetry-venv-deactivate ()
  "Deactivate the virtualenv associated with the current project.

This function restores the original environment variables and
removes the virtualenv bin directory from the PATH. It also sets
`poetry-active-project' to nil."
  (interactive)
  (unless poetry-active-project
    (error "No active virtualenv to deactivate"))

  ;; Restore the original environment variables.
  (-each poetry-venv--original-env-vars
    (lambda (env-var-item)
      (let ((env-var-name (car env-var-item))
            (env-var-value (cdr env-var-item)))
        (setenv env-var-name env-var-value))))

  ;; Remove the virtualenv bin directory from the PATH.
  (let* ((venv-bin-dir (alist-get 'venv-bin poetry-active-project))
         (path-dirs (split-string (getenv "PATH") ":"))
         (cleaned-path-dirs (remove venv-bin-dir path-dirs)))
    ;; Set the PATH environment variable.
    (setenv "PATH" (string-join cleaned-path-dirs ":"))
    ;; Remove the virtualenv from the exec-path.
    (setq exec-path (remove venv-bin-dir exec-path))

    (setq poetry-active-project nil))

  (poetry-eglot-reconnect))


(provide 'poetry-venv)
;;; poetry-venv.el ends here
