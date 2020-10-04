;;; completing-read-xref.el --- Description -*- lexical-binding: t; -*-

;; Author: jixiuf  jixiuf@jxfhome
;; Keywords:
;; URL:

;; Copyright (C) 2020, jixiuf, all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages use completing-read as the interface for selection from xref results.
;; So it should support use icomplete-mode/helm/ivy to complete.
;; This is a fork of https://github.com/alexmurray/ivy-xref

;;; Setup

;; (setq xref-show-xrefs-function 'completing-read-xref-show-defs)
;; (setq xref-show-definitions-function 'completing-read-xref-show-defs)

;;; Code:
(require 'xref)

(defgroup completing-read-xref nil
  "Select xref results using completing-read."
  :prefix "completing-read-xref-"
  :group 'completing-read
  :link '(url-link :tag "Github" "https://github.com/jixiuf/completing-read-xref"))

(defcustom completing-read-xref-use-file-path nil
  "Whether to display the file path."
  :type 'boolean
  :group 'completing-read-xref)

(defcustom completing-read-xref-remove-text-properties nil
  "Whether to display the candidates with their original faces."
  :type 'boolean
  :group 'completing-read-xref)

(defun completing-read-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `completing-read'."
  (let ((collection nil))
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let* ((line (xref-location-line location))
               (file (xref-location-group location))
               (candidate
                 (concat
                  (propertize
                   (concat
                    (if completing-read-xref-use-file-path
                        file
                      (file-name-nondirectory file))
                    (if (integerp line)
                        (format ":%d: " line)
                      ": "))
                   'face 'compilation-info)
                  (progn
                    (when completing-read-xref-remove-text-properties
                      (set-text-properties 0 (length summary) nil summary))
                    summary))))
          (push `(,candidate . ,location) collection))))
    (nreverse collection)))

;;;###autoload
(defun completing-xref-show-xrefs (fetcher alist)
  "Show the list of xrefs returned by FETCHER and ALIST via completing-read."
  ;; call the original xref--show-xref-buffer so we can be used with
  ;; dired-do-find-regexp-and-replace etc which expects to use the normal xref
  ;; results buffer but then bury it and delete the window containing it
  ;; immediately since we don't want to see it - see #2
  (let* ((xrefs (if (functionp fetcher)
                    ;; Emacs 27
                    (or (assoc-default 'fetched-xrefs alist)
                        (funcall fetcher))
                  fetcher))
         (buffer (xref--show-xref-buffer fetcher alist)))
    (quit-window)
    (let* ((orig-buf (current-buffer))
           (orig-pos (point))
           (cands (completing-read-xref-make-collection xrefs))
           (candidate (completing-read "xref: "  cands nil t ))
           done)
      (setq candidate (assoc candidate cands))
      (condition-case err
          (let* ((marker (xref-location-marker (cdr candidate)))
                 (buf (marker-buffer marker)))
            (with-current-buffer buffer
              (select-window
               ;; function signature changed in
               ;; 2a973edeacefcabb9fd8024188b7e167f0f9a9b6
               (if (version< emacs-version "26.0.90")
                   (xref--show-pos-in-buf marker buf t)
                 (xref--show-pos-in-buf marker buf)))))
        (user-error (message (error-message-string err)))))
    ;; honor the contact of xref--show-xref-buffer by returning its original
    ;; return value
    buffer))

;;;###autoload
(defun completing-read-xref-show-defs (fetcher alist)
  "Show the list of definitions returned by FETCHER and ALIST via completing-read.
Will jump to the definition if only one is found."
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (completing-xref-show-xrefs fetcher
                           (cons (cons 'fetched-xrefs xrefs)
                                 alist))))))

(provide 'completing-read-xref)

;; Local Variables:
;; coding: utf-8
;; End:

;;; completing-read-xref.el ends here.
