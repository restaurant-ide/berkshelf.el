;;; berkshelf.el --- Interact with Berkshelf from Emacs

;; Copyright (c) 2011 Tobias Svensson <tob@tobiassvensson.co.uk>

;; Author: Tobias Svensson <tob@tobiassvensson.co.uk>
;; URL: http://github.com/endofunky/berkshelf.el
;; Keywords: berkshelf ruby
;; Created: 31 Dec 2011
;; Version: 1.1.1
;; Package-Requires: ((inf-ruby "2.1") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Interact with Berkshelf from Emacs.
;;
;; 1) berks-open
;;
;;    Wraps 'berks open' which, if the given gem is installed and has been
;;    required correctly, will open the gem's source directory with dired.
;;
;; 2) berks-console
;;
;;    Starts an inferior ruby process in the context of the current berks
;;    using 'berks console' (requires inf-ruby to be installed).
;;
;; 3) berks-install, berks-update, berks-check
;;
;;    Runs the corresponding Berkshelf command with async-shell-command and
;;    *Berkshelf* as the target buffer. This exists so the output won't mess
;;    with the default buffer used by M-& and async-shell-command.

;;; Install

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/endofunky/berkshelf.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/berkshelf.el")
;; (require 'berkshelf)

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
(require 'inf-ruby)
;;;###autoload
;; TODO: implement apply, contingent, cookbook <NAME>, info <NAME>, init <path>, list, package, search, shelf, show, upload, vendor, verify, viz

;;;###autoload
(defun berks-install ()
  "Run berks install for the current berks."
  (interactive)
  (berks-command "berks install"))

;;;###autoload
(defun berks-update (&optional update-cmd-args)
  "Run berks update for the current berks."
  (interactive "P")
  (let ((command "berks update"))
    ;; For customization of the command with prefix arg.
    (setq command (if update-cmd-args
                      (read-string "Run: " (concat command " "))
                    command))

    (berks-command command)))

;;;###autoload
(defun berks-berksfile (&optional berksfile)
  "Set BERKS_BERKSFILE environment variable."
  (interactive
   (list
    (let ((default-p
            (let ((berksfile-dir (berks-locate-berksfile)))
              (if (not berksfile-dir)
                  "Berksfile"
                (concat berksfile-dir "Berksfile")))))
    (read-string (format "Berksfile (%s): " default-p)
                 default-p nil default-p))))
  (if berksfile
      (if (file-readable-p berksfile)
          (progn
            (setq berks-gem-list-cache (make-hash-table))
            (setenv "BERKS_BERKSFILE" berksfile)
            (message "BERKS_BERKSFILE set to: %s." berksfile))
        (message "Warning: couldn't read file \"%s\". BERKS_BERKSFILE unchanged." berksfile))
    (setenv "BERKS_BERKSFILE")))

;;;###autoload
(defun berks-outdated ()
  "List installed gems with newer versions available."
  (interactive)
  (berks-command "berks outdated"))

;;;###autoload
(defun berks-show ()
  "Shows all gems that are part of the berks, or the path to a given gem."
  (interactive)
  (berks-command "berks show"))

;;;###autoload
(defun berks-version ()
  "Prints version information."
  (interactive)
  (shell-command "berks version"))

(defun berks-command (cmd)
  "Run cmd in an async buffer."
  (async-shell-command cmd "*Berkshelf*"))

(defun berks-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-berksfile if the
Berksfile could not be found, or nil if the Gem could not be
found."
  (let ((berkshelf-stdout
         (shell-command-to-string
          (format "berks show %s" (shell-quote-argument gem-name))))
        (remote (file-remote-p default-directory)))
    (cond
     ((string-match "Could not locate Berksfile" berkshelf-stdout)
      'no-berksfile)
     ((string-match "Could not find " berkshelf-stdout)
      nil)
     (t
      (concat remote
              (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               berkshelf-stdout)
              "/")))))

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

(defun berks-list-gems-cached ()
  (let* ((berksfile-dir (berks-locate-berksfile))
         (gem-list (gethash berksfile-dir berks-gem-list-cache)))
    (if (not berksfile-dir)
        nil
      (unless gem-list
        (print (format "Don't have directory %s in cache yet, updating." berksfile-dir))
        (setq gem-list (berks-list-gems))
        (puthash berksfile-dir gem-list berks-gem-list-cache))
      gem-list)))

(defun berks-list-gems ()
  (save-excursion
    (let* ((cmd "berks list")
           (berks-out (shell-command-to-string cmd))
           (berks-lines (split-string berks-out "\n")))

      (defun parse-berks-list-line (line)
        (cond
         ((string-match "^  \\* \\([^\s]+\\).*$" line)
          (match-string 1 line))
         ((string-match "Could not \\(find\\|locate\\)" line)
          (message line) nil)
         ((string-match "Gems included by the berks:\\|^ *$" line)
          nil)
         (t
          (message "Warning: couldn't parse line from \"%s\":\n%s"
                   cmd line)
          nil)))

      (remq nil (mapcar 'parse-berks-list-line berks-lines)))))

(defun berks-list-gem-paths ()
  (save-excursion
    (let* ((cmd "berks list --paths")
           (berks-out (shell-command-to-string cmd)))
      (split-string berks-out "\n"))))

(provide 'berkshelf)
;;; berkshelf.el ends here.
