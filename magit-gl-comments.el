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

(defun magit-gl-current-revision ()
  (cond ((derived-mode-p 'magit-revision-mode)
         (car magit-refresh-args))
        ((derived-mode-p 'magit-diff-mode)
         (--when-let (car magit-refresh-args)
           (and (string-match "\\.\\.\\([^.].*\\)?[ \t]*\\'" it)
                (match-string 1 it))))))

(defun magit-gl-current-hunk (section)
  (pcase (magit-diff-scope)
    ((or `hunk `region) section)
    ((or `file `files)  (car (magit-section-children section)))
    (`list (car (magit-section-children
                 (car (magit-section-children section)))))))

(defun magit-gl-diff-position (section)
  (let* ((parent-section (magit-section-parent section))
         (cpos (marker-position (magit-section-content parent-section)))
         (cstart (save-excursion (goto-char cpos) (line-number-at-pos)))
         (stop (line-number-at-pos)))
    (- stop cstart)))

(defun magit-gl-comments-test ()
	"Hey ho."
	(run-with-idle-timer
	 0.5 nil
	 (lambda ()
		 (let*
				 ((revision (magit-copy-buffer-revision))
					(project-name (replace-regexp-in-string "*magit-revision: " "" (buffer-name)))
					(project-id (cdr (assoc project-name projectile-gitlab-project-cache)))
					(comments (with-local-quit (gitlab-list-commit-comments project-id revision))))
			 (message "%s" comments)))))

(provide 'magit-gl-comments)
;;; magit-gl-comments.el ends here
