;;  kiirogaa.el
;;

; User set variables.
(defvar kiirogaa-log-file-name "~/kiirogaa.log"
  "Log file name.")
(defvar kiirogaa-log-buffer nil
  "Buffer object to log.")
(defcustom kiirogaa-watch-buffer-name nil
  "Buffer name to watch (regexp).")

; Internal variables.
(defconst -kiirogaa-tmp-buffer-name " *kiirogaa*"
  "Name for temporary buffer (internal use).")
(defvar -kiirogaa-last-buffer nil
  "Last logged buffer (internal use).")

(defun kiirogaa-start-logging ()
  (interactive)
  (setq kiirogaa-log-buffer (current-buffer))
  )

(defun kiirogaa-stop-logging ()
  (interactive)
  (setq kiirogaa-log-buffer nil)
  )

(defun kiirogaa-post-self-insert-hook ()
  (when (or kiirogaa-log-buffer
	    (stringp kiirogaa-watch-buffer-name))
    (let* ((buffer (current-buffer))
	   (name (buffer-name buffer)))
      (when (or (eq kiirogaa-log-buffer buffer)
		(string-match kiirogaa-watch-buffer-name name))
	(save-current-buffer
	  (set-buffer (get-buffer-create -kiirogaa-tmp-buffer-name))
	  (erase-buffer)
	  (when (not (eq -kiirogaa-last-buffer buffer))
	    (insert (format "# %s\n" name))
	    (setq -kiirogaa-last-buffer buffer))
	  (insert (format "%s %d\n" (float-time) last-command-event))
	  (write-region nil nil kiirogaa-log-file-name t [])
	  ))
      ))
  )
(add-hook 'post-self-insert-hook (function kiirogaa-post-self-insert-hook))
