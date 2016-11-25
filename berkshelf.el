;;; berkshelf.el --- Interact with Berkshelf from Emacs

;; Copyright (c) 2016 Alexander aka CosmonauT Vynnyk <cosmonaut.ok@zoho.com>

;; Author: Tobias Svensson <tob@tobiassvensson.co.uk>
;; URL: https://github.com/restaurant-ide/berkshelf.el
;; Keywords: berkshelf ruby
;; Created: 24 Nov 2016
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))

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

;;;###autoload
(defun berks-install ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks install"))

;;;###autoload
(defun berks-list ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks list"))

;;;###autoload
(defun berks-outdated ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks outdated"))

;;;###autoload
(defun berks-search (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-command (concat "berks search " cookbookk)))

;;;###autoload
(defun berks-info (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-command (concat "berks info " cookbookk)))

;;;###autoload
(defun berks-show (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-command (concat "berks show " cookbookk)))

;;;###autoload
(defun berks-update (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-command (concat "berks update " cookbookk)))

;;;###autoload
(defun berks-upload (cookbook)
  "Run berks install for the current berks."
  (interactive "sCookbook Name: ")
  (berks-command (concat "berks upload " cookbookk)))

;;;###autoload
(defun berks-vendor (path)
  "Run berks install for the current berks."
  (interactive "sPath Name: ")
  (berks-command (concat "berks vendor " path)))

;;;###autoload
(defun berks-package (path)
  "Run berks install for the current berks."
  (interactive "sDirectory Name: ")
  (berks-command (concat "berks package " path)))

;;;###autoload
(defun berks-install ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks install"))

;;;###autoload
(defun berks-verify ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks verify"))

;;;###autoload
(defun berks-viz ()
  "Run berks install for the current berks."
  (interactive)
  (let ((file (concat ".berks_viz." (number-to-string (random 100000)))))
    (berks-command (concat "berks viz -o " file "; xdg-open " file "; rm -f " file))))

(defun berks-command (cmd)
  "Run cmd in an async buffer."
  (async-shell-command cmd "*Berkshelf*"))

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

(provide 'berkshelf)
;;; berkshelf.el ends here.
