;;; magit-gl-comments.el --- Gitlab commenting extensions for magit

;; Author: Jason Dufair
;; URL: https://github.com/jkdufair/magit-gl
;; Version: 0.1.0
;; Keywords: gitlab,magit

;; Copyright (C) 2017 Jason Dufair <jase@dufair.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Commit comment utilities for Magit & Gitlab

;;; Code:

(require 'dash)

(defun magit-gl-commit-level-comments (comments)
	(--filter
	 (eq nil (cdr (assoc 'line it)))
	 (-map 'identity comments)))

(defun magit-gl-line-level-comments (comments)
	(--filter
	 (not (eq nil (cdr (assoc 'line it))))
	 (-map 'identity comments)))

(defun magit-gl-comments (revision project-id)
	"Fetch the comments for a given REVISION in PROJECT-ID."
	(with-local-quit (gitlab-list-commit-comments project-id revision)))

(defmacro magit-gl-with-project (&rest body)
	"Evaluate BODY in the context of the project implied by the revision buffer."
	`(let*
			((sha (magit-rev-parse (car magit-refresh-args)))
			 (project-name (replace-regexp-in-string "*magit-revision: " "" (buffer-name)))
			 (project-id (cdr (assoc project-name projectile-gitlab-project-cache))))
		 ,@body))

(defun magit-gl-comment-with-author-initials (comment)
	(let ((case-fold-search nil)
				(date-string (cdr (assoc 'created_at comment))))
		(string-match "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" date-string)
		(concat
		 "(" (match-string 1 date-string) ") "
		 (replace-regexp-in-string
			"[[:lower:] ]" ""
			(cdr (assoc 'name (cdr (assoc 'author comment)))))
		 ": "
		 (replace-regexp-in-string
			"\\(\\|
\\)"
			""
			(cdr (assoc 'note comment)))
		 "\n")))

(defun magit-gl-insert-commit-level-comments (rev)
	(set-window-margins (get-buffer-window) 2 0)
	(magit-gl-with-project
	 (let ((all-comments (magit-gl-comments sha project-id)))
		 ;; commit-level
		 (let
				 ((commit-comments (magit-gl-commit-level-comments all-comments)))
			 (magit-insert-section (commit-comment)
				 (magit-insert-heading "Commit-level comments:")
				 (mapc (lambda (comment) (insert (magit-gl-comment-with-author-initials comment)))
							 commit-comments))
			 (newline))

		 ;; line level
		 (remove-overlays)
		 (let ((line-comments (magit-gl-line-level-comments all-comments)))
			 (save-excursion
				 (goto-char (point-min))
				 ;; Skip down to the file sections
				 (while (not (magit-section-match 'file))
					 (magit-section-forward-sibling))
				 (let ((more-sections t))
					 (while (and more-sections (magit-section-match 'file))
						 (while (not (magit-section-match 'hunk))
							 (magit-section-forward)
							 (let* ((section (magit-current-section))
											(section-value (magit-section-value section))
											(a (cadr section-value))
											(b (car (cddr section-value)))
											(_	(string-match "^-\\([0-9]*\\),\\([0-9]*\\)$" a))
											(abeg (string-to-number (match-string 1 a)))
											(alen (string-to-number (match-string 2 a)))
											(aend (+ abeg alen))
											(_ (string-match "^+\\([0-9]*\\),\\([0-9]*\\)$" b))
											(bbeg (string-to-number (match-string 1 b)))
											(blen (string-to-number (match-string 2 b)))
											(bend (+ bbeg blen))
											(comments-in-range
											 (--filter (and (>= (cdr (assoc 'line it)) (min abeg bbeg))
																		 (<= (cdr (assoc 'line it)) (max aend bend)))
																line-comments)))
								 (mapc (lambda (comment)
												 (save-excursion
													 ;; TODO figure out the line offsets
													 (forward-line (+ 1 (- (cdr (assoc 'line comment)) abeg)))
													 (let ((bullet-ov (make-overlay (point) (point)))
																 (comments-ov (make-overlay (line-end-position) (line-end-position))))
														 (overlay-put bullet-ov 'before-string (propertize "nil" 'display '((margin left-margin) "⦿")))
														 (overlay-put comments-ov 'after-string (concat "\n"
																																						(magit-gl-comment-with-author-initials comment)))
														 )))
											 comments-in-range)))
						 (if (> (length (magit-section-siblings (magit-current-section) 'next)) 0)
								 (magit-section-forward-sibling)
							 (setq more-sections nil)))))))))

(add-hook 'magit-revision-mode-hook
					(lambda ()
						(local-set-key (kbd "C") 'magit-gl-create-commit-level-comment)))

(defun magit-gl-create-commit-level-comment ()
	"Prompt for a new comment and add to commit in revision buffer."
	(interactive)
	(let*
			((sha (magit-rev-parse (car magit-refresh-args)))
			 (project-name (replace-regexp-in-string "*magit-revision: " "" (buffer-name)))
			 (project-id (cdr (assoc project-name projectile-gitlab-project-cache))))
		(magit-gl-with-project
		 (gitlab-create-commit-comment project-id sha
																	(read-string "Your 2¢: "))))
	(magit-refresh-buffer))

(provide 'magit-gl-comments)
;;; magit-gl-comments.el ends here
