;;  kiirogaa.el
;;

; User set variables.
(defvar kiirogaa-log-base-dir "~/keylog/"
  "Log file directory.")
(defvar kiirogaa-log-buffer-alist nil
  "Buffer object to log.")

; Internal variables.
(defconst -kiirogaa-tmp-buffer-name " *kiirogaa*"
  "Name for temporary buffer (internal use).")

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

(defun kiirogaa-post-self-insert-hook ()
  (let* ((alist (assq (current-buffer) kiirogaa-log-buffer-alist)))
    (when alist
      (save-current-buffer
	(set-buffer (get-buffer-create -kiirogaa-tmp-buffer-name))
	(erase-buffer)
	(let ((time (float-time))
	      (c last-command-event))
	  (insert (if (aref printable-chars c)
		      (format "%s %d '%s'\n" time c (char-to-string c))
		    (format "%s %d '\\%03o'\n" time c c)))
	  (write-region nil nil (cdr alist) t []))
	))
    ))
(add-hook 'post-self-insert-hook (function kiirogaa-post-self-insert-hook))
