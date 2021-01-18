;;; quickmarks.el --- quickmarks -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ioannis Canellos
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 




;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;;; Code:


;;
;; Quickmarks
;;


;;
;; Quickmarks is a convention created on top of org-mode that allows you to easily insert org links and images in org files (and not only) ...
;; The idea is that you create rich bookmarks in org-mode that are tagged with 'quickmark' (customizable) and use quickmark functions and snippets to easily use them as you are typing.
;; These entries may include properties for:
;; - url: QM_URL (customizable)
;; - logo: QM_LOGO (customizable)
;; - avatar: QM_AVATAR (customizable)
;; 

(defcustom qm-url "QM_URL" "The property key for the quickmark url." :group 'quickmarks)
(defcustom qm-logo "QM_LOGO" "The property key for the quickmark logo." :group 'quickmarks)
(defcustom qm-avatar "QM_AVATAR" "The property key for the quickmark avatar." :group 'quickmarks)
(defcustom qm-tag "quickmark" "The property key for the quickmark entry." :group 'quickmarks)

(defcustom qm-src-dir nil "The path to quickmark sources. If not specified detection will be attempted." :group 'quickmarks)
;; Capturing customization: Only applicable to capturing, querying doesn't require a fixed file or structure.
(defcustom qm-org-capture-template-dir "~/Documents/org/templates/" "The directory the template is going to installed." :group 'quickmarks)
(defcustom qm-org-capture-file "~/Documents/org/roam/quickmarks.org" "The file to use for capturing quickmarks." :group 'quickmarks)
(defcustom qm-org-capture-key-chord "Q" "The key chord to use for capturing quickmarks." :group 'quickmarks)
(defcustom qm-org-capture-heading "Quickmakrs" "The heading under which quickmarks will be captured." :group 'quickmarks)

(defconst qm-org-capture-template-file-name "quickmarks.orgtmpl" "The file name of the quickmarks template.")

(defun qm-all-entries ()
  "Find all quickmarks."
  (interactive)
  (let* ((entries (org-ql-query
                    :select ,(list (substring-no-properties (org-get-heading t t))
                                   (org-entry-get (point) ,qm-url)
                                   (org-entry-get (point) ,qm-logo)
                                   (org-entry-get (point) ,qm-avatar))
                    :from (org-agenda-files)
                    :where `(tags ,qm-tag))))
    entries))

(defun qm-entry-by-name (name)
  "Find a quickmark entry by name."
  (interactive)
  (let* ((entries (org-ql-query
                    :select `(list (substring-no-properties (org-get-heading t t))
                                   (org-entry-get (point) ,qm-url)
                                   (org-entry-get (point) ,qm-logo))
                    :from (org-agenda-files)
                    :where `(and (regexp ,name) (tags ,qm-tag)))))
    entries))

(defun qm-url-by-name (name)
  "Find a quickmark url entry by name."
  (nth 1 (qm-entry-by-name)))

(defun qm-logo-by-name (name)
  "Find a quickmark logo entry by name."
  (nth 2 (qm-entry-by-name)))

(defun qm-avatar-by-name (name)
  "Find a quickmark avatar entry by name."
  (nth 3 (qm-entry-by-name)))


(defun qm--link-to-qm (link)
  "Converts an org-mode LINK to a quickmark entry."
  (let* ((path (org-element-property :path link))
         (type (org-element-property :type link))
         (url (concat type ":" path))
         (entry (nth 2 link))
         (description (if entry (substring-no-properties (nth 2 link)) nil)))
    (if desc (list description (concat type path) nil nil) nil)))

(defun qm--collect-quickmarks-from-file (file) 
  "Collects all links from the SPECIIFED file and returns a list of quickmark entries."
  (with-temp-buffer
    (insert-file file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'link 'qm--link-to-qm)))


(defun qm--collect-quickmarks-from-directory (dir)
  (let ((path (file-truename dir)))
    (mapcar 'qm--collect-quickmarks-from-file (directory-files-recursively dir "\\.org$" t))))

(defun qm-create(heading url logo avatar)
  "Create a new quickmark. The quickmark will be an entry the specified HEADING.
   Optionally a property will be added for the specified URL LOGO and AVATAR. "
  (save-excursion
    (with-temp-buffer
      (insert-file qm-org-capture-file)
      (goto-char (point-max))
      (insert (concat "\n" "**" " " heading "  " ":quickmark:"))
      (message "adding url %s" url)
      (when url (org-entry-put (point) qm-url url))
      (when logo (org-entry-put (point) qm-logo logo))
      (when avatar (org-entry-put (point) qm-avatar avatar))
      (goto-char (point-max))
      (write-file qm-org-capture-file))))

;;
;; Initialization & Setup
;;

(defun qm--find-source-dir ()
  "Find the source dir of the project."
  (if qm-src-dir
      qm-src-dir
    (progn
      (setq qm-src-dir (replace-regexp-in-string (regexp-quote "straight/build/idee") "straight/repos/idee"
                                                 (file-name-directory
                                                  ;; Copied from ‘yasnippet-snippets’ that copied from ‘f-this-file’ from f.el.
                                                  (cond (load-in-progress load-file-name) ((and (boundp 'byte-compile-current-file) byte-compile-current-file) byte-compile-current-file)
                                                        (:else                            (buffer-file-name))))))
      qm-src-dir)))

(defun qm--install-template ()
  "Install the template to the template directory."
  (let* ((src (qm--find-source-dir))
         (src-template (concat (file-name-as-directory src) qm-org-capture-template-file-name))
         (target-template (concat (file-name-as-directory qm-org-capture-template-dir) qm-org-capture-template-file-name)))
    (when (not (file-exists-p src)) (make-directory src t))
    (copy-file src-template target-template t)))

(defun qm--install-snippets ()
  "Install the snippets to the emacs snippet directory."
  (let* ((src (qm--find-source-dir))
         (src-snippet-dir (file-name-as-directory (concat (file-name-as-directory src) "snippets")))
         (src-org-mode-snippet-dir (file-name-as-directory (concat src-snippet-dir "org-mode")))
         (target-snippet-dir (concat (file-name-as-directory user-emacs-directory) "snippets"))
         (target-org-mode-snippet-dir (file-name-as-directory (concat (file-name-as-directory target-snippet-dir) "org-mode"))))

    (when (not (file-exists-p target-org-mode-snippet-dir)) (make-directory target-org-mode-snippet-dir t))
    (mapcar (lambda (f) (copy-file f (concat target-org-mode-snippet-dir (file-name-nondirectory f))))
            (seq-filter (lambda (f) (not (string-suffix-p "." f))) (directory-files src-org-mode-snippet-dir t)))))

(defun qm--register-template()
  "Register the capture template for quickmarks."
  (let ((template-file (concat qm-org-capture-template-dir qm-org-capture-template-file-name)))
    (setq org-capture-templates (append org-capture-templates `((,qm-org-capture-key-chord "Quickmarks" entry (file+olp ,qm-org-capture-file "Quickmarks") (file ,template-file)))))))

(defun qm-install()
  (itneractive)
  "Install templates and snippets."
  (qm--install-snippets)
  (qm--install-template))

(defun qm-init()
  (interactive)
  "Initialize quickmarks."
  (qm--register-template))
;;; quickmarks.el ends here
