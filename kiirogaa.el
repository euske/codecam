;;  kiirogaa.el
;;

; User set variables.
(defvar kiirogaa-log-file-name "~/kiirogaa.log"
  "Log file name.")
(defvar kiirogaa-watch-buffer-name nil
  "Buffer name to watch (regexp).")

; Internal variables.
(defconst -kiirogaa-tmp-buffer-name " *kiirogaa*"
  "Name for temporary buffer (internal use).")
(defvar -kiirogaa-last-buffer nil
  "Last logged buffer (internal use).")

(defun kiirogaa-post-self-insert-hook ()
  (when (stringp kiirogaa-watch-buffer-name)
    (let* ((buffer (current-buffer))
	   (name (buffer-name buffer)))
      (when (string-match kiirogaa-watch-buffer-name name)
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
