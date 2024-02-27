;;; poetry-core.el --- The core functionality of poetry-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 -  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; This library implements the core functionality of poetry-mode.

;;; Code:


;;; Internals
;; Define the minor mode
(define-minor-mode poetry-output-mode
  "Minor mode for viewing poetry command output."
  :lighter " Poetry Out"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Define custom keybindings, if any
            ;; Example: (define-key map (kbd "<key>") '<function>)
            map)
  ;; Optional: Mode setup code, runs on mode activation
  (when poetry-output-mode
    ;; Example setup code: adjust buffer settings, apply syntax highlighting, etc.
    ))

;; Function to run a poetry command and display the output in a custom buffer
(defun run-poetry-command (command)
  "Run a poetry command and display the output in a buffer."
  (interactive "sPoetry command: ")
  (let ((output-buffer (get-buffer-create "*Poetry-Output*")))
    (with-current-buffer output-buffer
      (read-only-mode -1)  ;; Disable read-only mode to clear the buffer
      (erase-buffer)       ;; Clear the buffer
      ;; Run the poetry command asynchronously
      (make-process :name "poetry-command"
                    :buffer output-buffer
                    :command (list "poetry" (shell-quote-argument command))
                    :sentinel (lambda (process event)
                                (when (string-match-p "^finished" event)
                                  (with-current-buffer (process-buffer process)
                                    (poetry-output-mode 1)  ;; Activate poetry-output-mode
                                    (read-only-mode 1)     ;; Make the buffer read-only
                                    )))))
    ;; Display the output buffer
    (display-buffer output-buffer)))

;; Optional: Define additional functions or hooks as needed for your minor mode


(defvar poetry-mode--debug-buffer "*Poetry Debug*"
  "The buffer used to display debug information.")

(defun poetry--run-command-sync (args)
  "Run a Poetry command with ARGS and return the output as a string."
  (shell-command-to-string
   (format "%s %s" poetry-command (s-join " " args))))

(defun poetry-mode--run-command (command-list &optional project-dir callback)
  "Run a Poetry command COMMAND-LIST asynchronously in the PROJECT-DIR directory.
If CALLBACK is non-nil, it is called with the process as an argument when the command finishes."

  (let ((default-directory (or project-dir default-directory))
        (process-buffer (generate-new-buffer poetry--debug-buffer)))
    (make-process :name poetry-mode-process-name
                  :buffer process-buffer
                  :command (list shell-file-name
                                 shell-command-switch
                                 poetry-command
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

(provide 'poetry-core)
;;; poetry-core.el ends here
