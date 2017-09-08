;;; berkshelf.el --- Interact with Berkshelf from Emacs

;; Copyright (c) 2016 Alexander aka CosmonauT Vynnyk <cosmonaut.ok@zoho.com>

;; Author: Tobias Svensson <tob@tobiassvensson.co.uk>
;; URL: https://github.com/restaurant-ide/berkshelf.el
;; Keywords: berkshelf ruby
;; Created: 24 Nov 2016
;; Version: 0.1.1
;; Package-Requires: ((cl-lib "0.5") (json "1.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Interact with Berkshelf from Emacs.
;;

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)

(defvar berksfile-modification-time 0)
(defvar berksfile-cookbooks-list-cache nil)

(defgroup berkshelf nil
  "Berkshelf mode."
  :group 'languages)

(defcustom berkshelf-use-bundler-when-possible t
  "Use `bundle exec` for berkshelf when it's possible"
  :type 'boolean
  :group 'berkshelf)

(defun berkshelf-bundler-p ()
  (and berkshelf-use-bundler-when-possible
       (shell-command "which bundler")
       (shell-command "which bundle")))

(defun berks-command (cmd &optional buffer-name)
  "Run cmd in an async buffer."
  (let ((default-directory (berks-locate-berksfile))
	(buffer-name (or buffer-name "*Berkshelf*"))
	(current-buffer))
    ;; remove buffer if needed
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;;
    (setq current-buffer (get-buffer-create buffer-name))
    (princ (shell-command-to-string (concat (and (berkshelf-bundler-p) "bundle exec ") "berks "  cmd)) current-buffer)
    (view-buffer current-buffer)))

(defun berkshelf-colorize-compilation-buffer ()
  "Colorize berkshelf compile buffer output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; define berkshelf compilation mode
(define-compilation-mode berkshelf-compilation-mode "Berkshelf compilation"
  "Compilation mode for Berkshelf output."
  (add-hook 'compilation-filter-hook 'berkshelf-colorize-compilation-buffer nil t))

(defun berks-compile (cmd)
  "Run cmd in an async compilation buffer."
  (let ((root-dir (berks-locate-berksfile)))
    (if root-dir
	(let ((default-directory root-dir))
	  (compile
	   ;; cmd
	   (concat (and (berkshelf-bundler-p) "bundle exec ") "berks "  cmd)
	   'berkshelf-compilation-mode))
      (error "Couldn't locate Berksfile!"))))

(defun berks-completing-read (prompt collection &optional predicate
				     require-match initial-input
				     hist def inherit-input-method)
  "Try to use ido for completing read. Fallback to default."
  (if (and (listp collection) (fboundp 'ido-completing-read))
      (ido-completing-read prompt collection predicate require-match
			   initial-input hist def inherit-input-method)
    (completing-read-default prompt collection predicate require-match
			     initial-input hist def inherit-input-method)))

(defun update-berksfile-change-time ()
  "Use for cache expiration"
  (let ((berksfile (berks-locate-berksfile)))
    (when berksfile
      (let ((mod-time (nth 6 (file-attributes berksfile))))
	(when (not (eq (+ (car mod-time) (cadr mod-time)) berksfile-modification-time))
	  (setq berksfile-modification-time (+ (car mod-time) (cadr mod-time))))))))
    
(defun make-berksfile-cookbooks-cache ()
  (when (update-berksfile-change-time)
    (let ((default-directory (berks-locate-berksfile)))
      (setq berksfile-cookbooks-list-cache
	    ;; emacs has no standard way to convert array to list.
	    ;; We use ``append`` than
	    (append (cdar
		     (json-read-from-string
		      (shell-command-to-string
		       (concat (and (berkshelf-bundler-p) "bundle exec ") "berks list -F json")))) nil)))))

(defun get-berksfile-cookbooks-list ()
  (make-berksfile-cookbooks-cache)
  berksfile-cookbooks-list-cache)

(defun get-berksfile-cookbooks-names ()
  (let ((names))
    (dolist (cb (get-berksfile-cookbooks-list))
      (dolist (cb-param cb)
	(when (eq (car cb-param) 'name)
	  (push (cdr cb-param) names))))
    names))

(defvar berks-gem-list-cache
  (make-hash-table)
  "Holds a hash table of gem lists per directory.")

(cl-defun berks-locate-berksfile (&optional (dir default-directory))
         (let ((has-berksfile (directory-files dir nil "^Berksfile$"))
               (is-root (equal dir "/")))
           (cond
            (has-berksfile dir)
            (is-root
             (print (format
                     "No Berksfile found in either %s or any parent directory!"
                     default-directory))
             nil)
            ((berks-locate-berksfile (expand-file-name ".." dir))))))

;;;###autoload
(defun berks-install ()
  "Run berks install for the current berks."
  (interactive)
  (berks-compile "install"))

;;;###autoload
(defun berks-update ()
  "Run berks update cookbook."
  (interactive)
  (berks-compile "update"))

;;;###autoload
(defun berks-update-cookbook (cookbook)
  "Run berks update cookbook."
  (interactive "sCookbook Name: ")
  (berks-compile (concat "update " cookbook)))

;;;###autoload
(defun berks-list ()
  "Run berks install for the current berks."
  (interactive)
  ;; (make-berksfile-cookbooks-cache)
  (let ((cookbooks-string))
    (dolist (cb (get-berksfile-cookbooks-list))
      (let ((cb-name) (cb-version) (cb-location))
	(dolist (cb-desc cb)
	  (cond ((eq (car cb-desc) 'name)
		 (setq cb-name (cdr cb-desc)))
		((eq (car cb-desc) 'version)
		 (setq cb-version (cdr cb-desc)))
		((eq (car cb-desc) 'location)
		 (setq cb-location (cdr cb-desc)))))
	(if cb-name
	    (setq cookbooks-string (concat cookbooks-string "Name: " cb-name)))
	(if cb-version
	    (setq cookbooks-string (concat cookbooks-string "\t\t\tVersion: " cb-version)))
	(if cb-location
	    (setq cookbooks-string (concat cookbooks-string "\tLocation: " cb-location)))
	(setq cookbooks-string (concat cookbooks-string "\n"))))
    ;; print result string
    (with-output-to-temp-buffer "*Berks List*"
      (princ cookbooks-string))))

;;;###autoload
(defun berks-contingent (cookbook)
  "Run berks contingent for cookbook."
  (interactive (list (berks-completing-read "Cookbook Name: " (get-berksfile-cookbooks-names))))
  (berks-compile (concat "contingent " cookbook)))

;;;###autoload
(defun berks-outdated ()
  "Run berks outdated for the current berks."
  (interactive)
  (berks-compile "outdated"))

;;;###autoload
(defun berks-info (cookbook)
  "Run berks info for cookbook."
  (interactive "sCookbook Name: ")
  (berks-compile (concat "info " cookbook)))

;;;###autoload
(defun berks-upload (cookbook)
  "Run berks install for the current berks."
  (interactive (list (berks-completing-read "Cookbook Name: " (get-berksfile-cookbooks-names))))
  (berks-compile (concat "upload " cookbook)))

;;;###autoload
(defun berks-verify ()
  "Run berks install for the current berks."
  (interactive)
  (berks-compile "verify"))

;;;###autoload
(defun berks-viz ()
  "Run berks install for the current berks."
  (interactive)
  (let ((file (concat ".berks_viz." (number-to-string (random 100000)))))
    (berks-command (concat "viz -o " file "; xdg-open " file "; rm -f " file))))

;;;###autoload
(defun berks-search (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-compile (concat "search " cookbook)))

(provide 'berkshelf)
;;; berkshelf.el ends here.
