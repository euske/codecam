;;  kiirogaa.el
;;
(require 'cl)

; User set variables.
(defvar kiirogaa-log-base-dir "~/keylog/"
  "Log file directory.")
(defvar kiirogaa-log-buffer-alist nil
  "Buffer object to log.")

; Internal variables.
(defvar -kiirogaa-tmp-last-point)
(defconst -kiirogaa-tmp-buffer-name " *kiirogaa*"
  "Name for temporary buffer (internal use).")

(defun kiirogaa-format-entry (c time)
  (if (aref printable-chars c)
      (format "%s %d '%s'\n" time c (char-to-string c))
    (format "%s %d '\\%03o'\n" time c c)))

(defun kiirogaa-start-logging (filename)
  (interactive (list
                (read-file-name "Log filename: " kiirogaa-log-base-dir nil nil
                                (concat (buffer-name) ".keylog"))))
  (let ((pair (cons (buffer-name) filename)))
    (setq kiirogaa-log-buffer-alist
          (cons pair kiirogaa-log-buffer-alist))
    ))

(defun kiirogaa-stop-logging ()
  (interactive)
  (let ((name (buffer-name)))
    (setq kiirogaa-log-buffer-alist
          (remove-if (lambda (pair) (string= name (car pair)))
                     kiirogaa-log-buffer-alist))
    ))

(defun kiirogaa-write-buffer-with-timestamp (filename &optional visit)
  (interactive "FWrite filename: ")
  (let ((chars (make-vector (buffer-size) nil)))
    (let ((i 0))
      (while (< i (length chars))
        (setf (aref chars i)
              (list (char-after (1+ i))
                    (get-text-property (1+ i) 'time)))
        (setq i (1+ i))))
    (save-current-buffer
      (set-buffer (get-buffer-create -kiirogaa-tmp-buffer-name))
      (erase-buffer)
      (let ((i 0))
        (while (< i (length chars))
          (let ((e (aref chars i)))
            (insert (kiirogaa-format-entry (car e) (cadr e))))
          (setq i (1+ i))))
      (if visit
          (write-file filename t)
        (write-region nil nil filename nil []))
      )
    ))

(defun kiirogaa-save-buffer-with-timestamp ()
  (interactive)
  (let* ((name (buffer-name))
         (pair (assoc name kiirogaa-log-buffer-alist)))
    (when pair
      (kiirogaa-write-buffer-with-timestamp
       (concat kiirogaa-log-base-dir
               (concat name ".timestamp")))
      )
    ))

(defun kiirogaa-pre-command-hook ()
  (setq -kiirogaa-tmp-last-point (point)))

(defun kiirogaa-post-self-insert-hook ()
  (let* ((name (buffer-name))
         (pair (assoc name kiirogaa-log-buffer-alist)))
    (when pair
      (let ((time (float-time))
            (p -kiirogaa-tmp-last-point)
            (c last-command-event))
        (put-text-property p (1+ p) 'time time)
        (save-current-buffer
          (set-buffer (get-buffer-create -kiirogaa-tmp-buffer-name))
          (erase-buffer)
          (insert (kiirogaa-format-entry c time))
          (write-region nil nil (cdr pair) t []))
        ))
    ))

(add-hook 'pre-command-hook (function kiirogaa-pre-command-hook))
(add-hook 'after-save-hook (function kiirogaa-save-buffer-with-timestamp))
(add-hook 'post-self-insert-hook (function kiirogaa-post-self-insert-hook))
