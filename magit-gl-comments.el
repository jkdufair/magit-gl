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

(defun magit-gl-insert-commit-level-comments (rev)
	(magit-gl-with-project
	 (let*
			((case-fold-search nil))
		(magit-insert-section (commit-comment)
			(magit-insert-heading "Commit-level comments:")
			(mapc (lambda (comment)
							(let ((date-string (cdr (assoc 'created_at comment))))
								(string-match "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" date-string)
								(insert "(" (match-string 1 date-string) ") "))
							(insert (replace-regexp-in-string
											 "[[:lower:] ]" ""
											 (cdr (assoc 'name (cdr (assoc 'author comment))))))
							(insert ": ")
							(insert (replace-regexp-in-string
											 "\\(\\|
\\)"
											 ""
											 (cdr (assoc 'note comment))))
							(newline))
						(magit-gl-commit-level-comments
						 (magit-gl-comments sha project-id)))
			(newline)))))

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
