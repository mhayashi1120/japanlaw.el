
(defun japanlaw-test-full-index ()
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (cond
     ((looking-at "^(\"[+]\"")
      (japanlaw-index-open-or-close))
     ((looking-at "^(\"  -\"")
      (let ((bufs (buffer-list)))
        (save-window-excursion
          (japanlaw-index-open-or-close))
        (let ((created (cl-set-difference (buffer-list) bufs)))
          (dolist (buf created)
            (when (and (eq (buffer-local-value 'major-mode buf) 'japanlaw-mode)
                       (buffer-live-p buf))
              (kill-buffer buf)))))))
    (forward-line 1)
    (redisplay t)))
