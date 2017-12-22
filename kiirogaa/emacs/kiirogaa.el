;;  kiirogaa.el
;;

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
				(concat (buffer-name (current-buffer)) ".keylog"))))
  (setq kiirogaa-log-buffer-alist
	(cons (cons (current-buffer) filename) kiirogaa-log-buffer-alist))
  )

(defun kiirogaa-stop-logging ()
  (interactive)
  (setq kiirogaa-log-buffer-alist
	(assq-delete-all (current-buffer) kiirogaa-log-buffer-alist))
  )

(defun kiirogaa-save-buffer-with-timestamp (filename)
  (interactive "FSave filename: ")
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
      (write-file filename t))
    ))

(defun kiirogaa-pre-command-hook ()
  (setq -kiirogaa-tmp-last-point (point)))

(defun kiirogaa-post-self-insert-hook ()
  (let* ((alist (assq (current-buffer) kiirogaa-log-buffer-alist)))
    (when alist
      (let ((time (float-time))
	    (p -kiirogaa-tmp-last-point)
	    (c last-command-event))
	(put-text-property p (1+ p) 'time time)
	(save-current-buffer
	  (set-buffer (get-buffer-create -kiirogaa-tmp-buffer-name))
	  (erase-buffer)
	  (insert (kiirogaa-format-entry c time))
	  (write-region nil nil (cdr alist) t []))
	))
    ))
(add-hook 'pre-command-hook (function kiirogaa-pre-command-hook))
(add-hook 'post-self-insert-hook (function kiirogaa-post-self-insert-hook))
