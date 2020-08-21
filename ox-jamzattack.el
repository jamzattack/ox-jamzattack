;;; ox-jamzattack.el --- ox-publish helper functions for jamzattack.xyz  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'ox-publish)
(require 'ox-html)
(require 'cl-seq)


;;; Format Spec

(defun ox-jamzattack:last-updated (file)
  "Return the time that changes were commited in FILE."
  (let* ((default-directory (file-name-directory file))
	 (filename (file-relative-name file default-directory)))
    (nth 0
	 (ignore-errors
	   (process-lines (executable-find "git")
			  "log" "-1" "--date=short" "--pretty=format:%cd"
			  filename)))))

(defun ox-jamzattack:format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel.

Differences from `org-html-format-spec':
* %A - Author's name as a mailto link
* %M - Last modified time according to git"
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info timestamp-format)
			      info))
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
	      (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
	      (split-string (plist-get info :email)  ",+ *")
	      ", "))

      (?A . ,(format "<a href=\"mailto:%s\">%s</a>"
		     (plist-get info :email)
		     (org-export-data (plist-get info :author) info)))
      (?M . ,(let ((file (plist-get info :input-file)))
	       (or (ox-jamzattack:last-updated file)
		   (format-time-string timestamp-format
				       (and file (file-attribute-modification-time
						  (file-attributes file)))))))

      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file))))))
      (?v . ,(or (plist-get info :html-validation-link) "")))))

(advice-add 'org-html-format-spec :override 'ox-jamzattack:format-spec)


;;; Sitemap

(defun ox-jamzattack:sitemap (project &optional sitemap-filename)
  "If the file \".ring.org\" exists, include it in the sitemap.

This is advice for `org-publish-sitemap', and it should not be
called on its own.  PROJECT and SITEMAP-FILENAME are both used to
determine where the sitemap is located."
  (let* ((root (expand-file-name
		(file-name-as-directory
		 (org-publish-property :base-directory project))))
	 (sitemap-filename (concat root (or sitemap-filename "sitemap.org"))))
    (with-current-buffer (find-file-noselect sitemap-filename)
      (when (file-exists-p ".ring.org")
	(goto-char (point-max))
	(insert "\n\n#+include: .ring.org")
	(save-buffer)))))

(advice-add 'org-publish-sitemap :after 'ox-jamzattack:sitemap)


;;; RSS

(use-package webfeeder
  :straight t
  :defer t)

(defun ox-jamzattack:make-rss (project &rest _ignored)
  "Create an RSS feed for PROJECT."
  (let* ((dir (expand-file-name
	       (file-name-as-directory
		(org-publish-property :publishing-directory project))))
	 (files (directory-files dir nil "\\.html\\'"))
	 (project-name (car project))
	 (url (format "https://%s.jamzattack.xyz" project-name))
	 (title (pcase project-name
		  ("music" "Music")
		  ("blog" "The Yeet Log"))))
    (when (cl-member project-name
		     '("blog"
		       "music")
		     :test #'string-equal)
      (webfeeder-build
       "rss.xml"
       dir
       url
       files
       :title title
       :builder 'webfeeder-make-rss))))

(advice-add 'org-publish :after #'ox-jamzattack:make-rss)

(provide 'ox-jamzattack)
;;; ox-jamzattack.el ends here
