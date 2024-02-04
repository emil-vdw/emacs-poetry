;;; poetry.el --- A Poetry porcelain for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; URL: https://github.com/emil-vdw/poetry-mode
;; Keywords: Python, Tools
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.2") (transient "0.5.3") (dash "2.19.1"))

;;; Code:

;;; Public API
(defcustom poetry-mode-poetry-command "poetry"
  "The command to run poetry."
  :type 'string
  :group 'poetry-mode)

(defcustom poetry-mode-output-buffer "*Poetry*"
  "The buffer used to display the output of poetry commands."
  :type 'string
  :group 'poetry-mode)

(defcustom poetry-mode-process-name "poetry-mode"
  "The name of the process used to run poetry commands."
  :type 'string
  :group 'poetry-mode)

;;;###autoload
(defun poetry-mode-project-dir ()
  "Return the directory of the current poetry project."
  (when-let* ((project-root (locate-dominating-file "." "pyproject.toml"))
              (pyproject-file-contents
               (with-temp-buffer
                 (insert-file-contents-literally
                  (concat (file-name-as-directory project-root) "pyproject.toml"))
                 (buffer-string))))

    ;; Just because we found a pyproject.toml file doesn't mean
    ;; it's a poetry project.
    (when (string-match "^\\[tool\\.poetry]$" pyproject-file-contents)
      project-root)))


;;; Internals
(defvar poetry-mode--active-project nil
  "The attributes of the active poetry project.")

(defvar poetry-mode--debug-buffer "*Poetry Debug*"
  "The buffer used to display debug information.")

(defvar poetry-mode--original-env-vars
  (poetry-mode--get-env-vars)
  "The base environment variables to be set when activating a virtualenv.")

(defun poetry-mode--get-env-vars ()
  "Return the environment variables affected by the virtualenv."
  `(("PYTHONHOME" . ,(getenv "PYTHONHOME"))
    ("VIRTUAL_ENV" . nil)))

(defun poetry-mode--env-path ()
  "Return the path to the virtualenv associated with the current project."
  (when-let ((venv-path-pattern "^\\(\/[-\.a-zA-Z0-9]+\\)+$")
             (command-output
              (shell-command-to-string
               (format "%s env info -p" poetry-mode-poetry-command))))
    (save-match-data
      (when (string-match venv-path-pattern command-output)
        (match-string 0 command-output)))))


(defun poetry-mode--run-command (command-list &optional project-dir callback)
  "Run a Poetry command COMMAND-LIST asynchronously in the PROJECT-DIR directory.
If CALLBACK is non-nil, it is called with the process as an argument when the command finishes."

  (let ((default-directory (or project-dir default-directory))
        (process-buffer (generate-new-buffer poetry-mode--debug-buffer)))
    (make-process :name poetry-mode-process-name
                  :buffer process-buffer
                  :command (list shell-file-name
                                 shell-command-switch
                                 poetry-mode-poetry-command
                                 command)
                  :sentinel (lambda (process event)
                              (when (and callback (string= event "finished\n"))
                                (funcall callback process))
                              (unless (string= event "finished\n")
                                (with-current-buffer (process-buffer process)
                                  (setq-local buffer-read-only t)
                                  (goto-char (point-min))
                                  (insert (format "Process %s failed: %s" poetry-mode-process-name event))
                                  (display-buffer (current-buffer))))))))

(defun poetry-mode--poetry-command-sentinel (process event)
  "Sentinel for poetry commands."
  (with-current-buffer (process-buffer process)
    (setq-local buffer-read-only t)
    (goto-char (point-min))
    (display-buffer (current-buffer))))


(defun poetry-mode--in-project-p ()
  "Return non-nil if the current buffer is in a poetry project."
  (stringp (poetry-mode-project-dir)))

(defun poetry-mode-venv-activate ()
  "Activate the virtualenv associated with the current project."
  (interactive)

  (unless (poetry-mode--in-project-p)
    (error "Not in a poetry project"))

  (when poetry-mode--active-project
    (poetry-mode-venv-deactivate))

  (let* ((project-dir (poetry-mode-project-dir))
         (venv-path (poetry-mode--env-path))
         (venv-bin-dir (concat venv-path "/bin")))

    ;; Save the original environment variables.
    (setq poetry-mode--original-env-vars
          (-map (lambda (env-var-item)
                  `(,(car env-var-item) . ,(getenv (car env-var-item))))
                poetry-mode--original-env-vars))

    ;; Set the environment variables for the virtualenv.
    (setenv "VIRTUAL_ENV" venv-path)
    (setenv "PYTHONHOME" nil)
    (setenv "PATH"
            (string-join
             (cons venv-bin-dir (split-string (getenv "PATH") ":")) ":"))

    ;; Add the virtualenv bin directory to the PATH.
    (push venv-bin-dir exec-path)

    ;; Set the active project attributes.
    (setq poetry-mode--active-project
          `((project-dir . ,project-dir)
            (venv-path . ,venv-path)
            (venv-name . ,(file-name-nondirectory venv-path))
            (venv-bin . ,venv-bin-dir)))))


(defun poetry-mode-venv-deactivate ()
  "Deactivate the virtualenv associated with the current project."
  (interactive)
  (unless poetry-mode--active-project
    (error "No active virtualenv to deactivate"))

  ;; Restore the original environment variables.
  (-each poetry-mode--original-env-vars
    (lambda (env-var-item)
      (let ((env-var-name (car env-var-item))
            (env-var-value (cdr env-var-item)))
        (setenv env-var-name env-var-value))))

  ;; Remove the virtualenv bin directory from the PATH.
  (let* ((venv-bin-dir (alist-get 'venv-bin poetry-mode--active-project))
         (path-dirs (split-string (getenv "PATH") ":"))
         (cleaned-path-dirs (remove venv-bin-dir path-dirs)))
    ;; Set the PATH environment variable.
    (setenv "PATH" (string-join cleaned-path-dirs ":"))
    ;; Remove the virtualenv from the exec-path.
    (setq exec-path (remove venv-bin-dir exec-path))

    (setq poetry-mode--active-project nil)))


(provide 'poetry-mode)
;;; poetry-mode.el ends here
