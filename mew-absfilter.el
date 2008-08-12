;;; mew-bsfilter.el --- spam filter with bsfilter

;; Author: SAITO Takuya <reed@lily.freemail.ne.jp>
;; $Id$

;; You can use, copy, distribute, and/or modify this file for any purpose.
;; There is NO WARRANTY.

;;; Commentary:

;; http://www.h2.dion.ne.jp/~nabeken/bsfilter/

;; ** Don't "O" (mew-summary-pack) or 'refile + inc' during spam cheking **

;;; Code:

(require 'mew)

(defvar mew-bsfilter-program "bsfilter")
(defvar mew-bsfilter-arg-check '("--auto-update"))
(defvar mew-bsfilter-arg-clean '("--sub-spam" "--add-clean" "--update"))
(defvar mew-bsfilter-arg-spam '("--sub-clean" "--add-spam" "--update"))
(defvar mew-bsfilter-spam-cutoff 0.95)
(defvar mew-bsfilter-spam-folder "+spam")
(defvar mew-bsfilter-spam-action `(("^\\+spam$" ,mew-mark-review)
				   (t ,mew-bsfilter-spam-folder)))

(defvar mew-bsfilter-map nil)

(unless mew-bsfilter-map
  (setq mew-bsfilter-map (make-sparse-keymap))
  (define-key mew-bsfilter-map "c" 'mew-bsfilter-clean)
  (define-key mew-bsfilter-map "s" 'mew-bsfilter-spam)
  (define-key mew-bsfilter-map "mc" 'mew-bsfilter-mark-clean)
  (define-key mew-bsfilter-map "ms" 'mew-bsfilter-mark-spam)
  (define-key mew-bsfilter-map "b" 'mew-bsfilter-check-spam))

(define-key mew-summary-mode-map "b" mew-bsfilter-map)


;; Use buffer-local-variable in process-buffer.
;; process-{put,get} is avairable only in Emacs-21.4 or above.
(defvar mew-bsfilter-process-folder nil)
(make-variable-buffer-local 'mew-bsfilter-process-folder)

;; modeline
(defvar mew-summary-buffer-bsfilter-process nil)
(defvar mew-summary-buffer-bsfilter-process-status " bsfilter")

(defadvice mew-summary-setup-mode-line (after bsfilter-process activate)
  (let ((bsfilter (list 'mew-summary-buffer-bsfilter-process
			'mew-summary-buffer-bsfilter-process-status)))
    (unless (assq bsfilter mode-line-process)
      (setq mode-line-process (cons bsfilter mode-line-process)))))


(defun mew-bsfilter-clean (&optional wait no-msg)
  "Mark this message as clean (not spam)."
  (interactive)
  (when (mew-sumsyn-match mew-regex-sumsyn-short)
    (let* ((msg (mew-sumsyn-message-number))
	   (fld (mew-sumsyn-folder-name))
	   (path (mew-expand-folder fld msg)))
      (setq wait (if wait nil 0))
      (apply 'call-process
	     mew-bsfilter-program nil wait nil
	     (append mew-bsfilter-arg-clean (list path)))
      (mew-bsfilter-undo-action (mew-bsfilter-folder-action fld)))
    (unless no-msg
      (message "Marked as clean"))))

(defun mew-bsfilter-spam (&optional wait no-msg)
  "Mark this message as spam."
  (interactive)
  (when (mew-sumsyn-match mew-regex-sumsyn-short)
    (let* ((msg (mew-sumsyn-message-number))
	   (fld (mew-sumsyn-folder-name))
	   (path (mew-expand-folder fld msg)))
      (setq wait (if wait nil 0))
      (apply 'call-process
	     mew-bsfilter-program nil wait nil
	     (append mew-bsfilter-arg-spam (list path)))
      (mew-bsfilter-do-action (mew-bsfilter-folder-action fld)))
    (unless no-msg
      (message "Marked as spam"))))

(defmacro mew-bsfilter-each-mark-msg (mark region &rest body)
  `(let ((begend (when ,region (mew-summary-get-region))))
     (mew-decode-syntax-delete)
     (save-excursion
       (dolist (msg (mew-summary-mark-collect ,mark (car begend) (cdr begend)))
	 (goto-char (point-min))
	 (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
	   ,@body)))))

(defun mew-bsfilter-mark-clean (&optional arg)
  (interactive "P")
  (message "Marking as clean...")
  (mew-bsfilter-each-mark-msg mew-mark-review arg
			      (mew-bsfilter-clean t t))
  (message "Marking as clean...done"))

(defun mew-bsfilter-mark-spam (&optional arg)
  (interactive "P")
  (message "Marking as spam...")
  (mew-bsfilter-each-mark-msg mew-mark-review arg
			      (mew-bsfilter-spam t t))
  (message "Marking as spam...done"))

(defun mew-bsfilter-check-spam (&optional arg)
  "Check spam messages with bsfilter."
  (interactive "P")
  (mew-summary-or-thread
   (let ((region (if arg
		     (mew-summary-get-region)
		   (cons (point-min) (point-max)))))
     (mew-bsfilter-check-spam-region (mew-summary-folder-name)
				     (car region) (cdr region)))))

(defun mew-bsfilter-folder-action (folder)
  (let ((action (catch 'found
		  (dolist (act mew-bsfilter-spam-action)
		    (when (or (eq (car act) t)
			      ;; use (mew-folder-folder folder) ?
			      (string-match (car act) folder))
		      (throw 'found (cadr act)))))))
    (when (stringp action)
      (let ((prefix (mew-folder-prefix (mew-folder-folder folder))))
	(setq action (delq nil
			   (mapcar
			    (lambda (fld)
			      (let ((p (mew-folder-prefix
					(mew-folder-folder fld))))
				(when (and (not (mew-folder-nntpp p))
					   (not (mew-folder-popp p))
					   (string= prefix p))
				  fld)))
			    (mew-split action ?,))))))
    (or action 
	;;xxx default
	mew-mark-review)))

(defun mew-bsfilter-do-action (action)
  (cond
   ((listp action)
    (mew-summary-refile-body action t t))
   ((integerp action)
    (mew-mark-put-mark action t))))

(defun mew-bsfilter-undo-action (action)
  (when (mew-sumsyn-match mew-regex-sumsyn-short)
    (let ((msg (mew-sumsyn-message-number))
	  (fld (mew-sumsyn-folder-name))
	  (mark (mew-summary-get-mark)))
      (cond
       ((listp action)
	(when (and (eq mark mew-mark-refile)
		   (get-buffer fld)
		   (equal action (cdr (with-current-buffer fld
					(mew-refile-get msg)))))
	  (mew-summary-undo)))
       ((integerp action)
	(when (eq action mark)
	  (mew-summary-undo)))))))

(defun mew-bsfilter-check-spam-region (folder begin end)
  (if (not (and folder
	       (with-current-buffer folder (mew-summary-p))))
      (message "Can not spam check here")
    (let ((msglist (mew-bsfilter-collect-message-region begin end))
	  process)
      (when msglist
	(message "Spam checking...")
	(set-buffer (get-buffer-create
		     (generate-new-buffer-name " *mew bsfilter*")))
	(mew-erase-buffer)
	(setq mew-bsfilter-process-folder folder)
	(setq process (apply 'start-process "mew-bsfilter"
			     (current-buffer)
			     mew-bsfilter-program
			     (append mew-bsfilter-arg-check msglist)))
	(set-process-sentinel process 'mew-bsfilter-sentinel)
	(add-to-list 'mew-summary-buffer-bsfilter-process process)))))

(defun mew-bsfilter-sentinel (process event)
  (mew-filter
   (let* ((fld mew-bsfilter-process-folder)
	  (spam (mew-bsfilter-collect-spam-message fld)))
     (mew-bsfilter-apply-spam-action fld spam)))
  (setq mew-summary-buffer-bsfilter-process
	(delq process mew-summary-buffer-bsfilter-process))
  (kill-buffer (process-buffer process))
  (message "Spam checking...done"))

(defun mew-bsfilter-collect-message-region (begin end)
  "This function returns a list of message number."
  (mew-summary-or-thread
   (save-excursion
     (let ((regex (mew-mark-regex ? ))
	   (msglist nil))
       (goto-char begin)
       (while (re-search-forward regex end t)
	 (when (mew-sumsyn-match mew-regex-sumsyn-short)
	   (let ((msg (mew-sumsyn-message-number))
		 (fld (mew-sumsyn-folder-name)))
	     (setq msglist (cons (mew-expand-folder fld msg) msglist))))
	 (forward-line))
       (nreverse msglist)))))

(defun mew-bsfilter-collect-spam-message (folder)
  (save-excursion
    (let ((regexp (format " %s/\\([^ ]+\\) [^ ]+ \\(.+\\)$"
			  (regexp-quote (mew-expand-folder folder))))
	  spam)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(let ((msg (mew-match-string 1))
	      (score (string-to-number (mew-match-string 2))))
	  (when (> score mew-bsfilter-spam-cutoff)
	    (setq spam (cons msg spam)))))
      (nreverse spam))))

(defun mew-bsfilter-apply-spam-action (folder spam)
  (let* ((vfolder (mew-folder-to-thread folder))
	 (buf (if (and (get-buffer vfolder)
		       (mew-virtual-thread-p vfolder)
		       (mew-thread-cache-valid-p vfolder))
		  vfolder
		folder))
	 action)
    (when (get-buffer buf)
      (set-buffer buf)
      (setq action (mew-bsfilter-folder-action folder))
      (save-excursion
	(dolist (msg spam)
	  (goto-char (point-min))
	  (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
	    (mew-bsfilter-do-action action)))))))


;;; Check after `mew-summary-retrieve'.

;; biff scan inc sync exec get list jobs
(defvar mew-bsfilter-check-directive-list '(("+" inc)
					    ("$" nil)
					    ("%" inc scan)
					    ("-" scan)))

(eval-when-compile
  (defvar bnm)
  (defvar directive))
(defun mew-bsfilter-check-spam-after-retrieve ()
  "Check spam messages with bsfilter after retrieve."
  ;; bnm and directive is local variable which can be used in
  ;; mew-{local,pop,imap,nntp}-sentinel.
  (let* ((proto (mew-folder-prefix (mew-folder-folder bnm)))
	 (check (cdr (assoc proto mew-bsfilter-check-directive-list))))
    (when (memq directive check)
      (mew-bsfilter-check-spam-region bnm
				      (mew-sinfo-get-start-point)
				      (point-max)))))

(add-hook 'mew-pop-sentinel-non-biff-hook
	  'mew-bsfilter-check-spam-after-retrieve)
(add-hook 'mew-imap-sentinel-non-biff-hook
	  'mew-bsfilter-check-spam-after-retrieve)
(add-hook 'mew-nntp-sentinel-hook
	  'mew-bsfilter-check-spam-after-retrieve)
;; (add-hook 'mew-scan-sentinel-hook
;; 	  'mew-bsfilter-check-spam-after-retrieve)
(defadvice mew-local-sentinel (around bsfilter-check activate)
  (let* ((pnm (process-name process))
	 (bnm (mew-local-get-bnm pnm))
	 (directive (mew-local-get-directive pnm)))
    ad-do-it
    (mew-bsfilter-check-spam-after-retrieve)))


;;; Check after `mew-shimbun-retrieve'

;; Depend on my setting. 
;; See `mew-shimbun-retrieve-set-start-point' and
;; my `mew-shimbun-retrieve-all' which is not exec mew-kill-buffer.
(defun mew-bsfilter-check-spam-after-shimbun-retrieve ()
  "Check spam messages with bsfilter after shimbun-retrieve."
  (mew-bsfilter-check-spam-region (mew-summary-folder-name 'ext)
				  (mew-sinfo-get-start-point) (point-max)))

(add-hook 'mew-shimbun-retrieve-hook
	  'mew-bsfilter-check-spam-after-shimbun-retrieve)

(provide 'mew-bsfilter)

;;; mew-bsfilter.el ends here
