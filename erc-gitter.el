;;; erc-gitter.el --- Gitter-Interaction module for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  

;; Author:  <jleechpe@zin-archtop>
;; Keywords: tools, extensions

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

;; This module requires that the fix to bug #18936 be applied.
;; Without it erc-gitter-mode will not work.

;;; Code:

(require 'erc)
(require 'markdown-mode)

(defgroup erc-gitter nil
  "Customization for `erc-gitter'."
  :group 'erc)

(defcustom erc-gitter-is-fool 't
  "Is the gitter-bot considered a fool."
  :type 'boolean
  :group 'erc-gitter)

(defvar erc-gitter-button
  '("\\(\\w+/\\w+\\)?#\\([0-9]+\\)" 0
    (string= "irc.gitter.im" erc-session-server)
    erc-gitter-browse-issue 0)
  "Match github issue links that are sent to ERC.")

(define-erc-module gitter nil
  "Enable Gitter features in ERC."
  ((when erc-gitter-is-fool
     (erc-gitter-gitter-is-fool))
   (add-hook 'erc-send-pre-hook #'erc-gitter-send-code)
   (add-hook 'erc-send-modify-hook #'erc-gitter-display-code)
   (add-hook 'erc-insert-modify-hook #'erc-gitter-format-markdown)
   (add-hook 'erc-send-modify-hook #'erc-gitter-format-markdown)
   (add-to-list 'erc-button-alist erc-gitter-button))
  ((erc-gitter-gitter-is-no-fool)
   (remove-hook 'erc-send-pre-hook #'erc-gitter-send-code)
   (remove-hook 'erc-send-modify-hook #'erc-gitter-display-code)
   (remove-hook 'erc-insert-modify-hook #'erc-gitter-format-markdown)
   (remove-hook 'erc-send-modify-hook #'erc-gitter-format-markdown)
   (setq erc-button-alist (delete erc-gitter-button erc-button-alist))))

;;;; Multiline sending and markdown formatting

(defun erc-gitter-send-code (s)
  (when (string= "irc.gitter.im" erc-session-server)
    (setq str (replace-regexp-in-string "[\n]" "\r" s nil))))

(defun erc-gitter-display-code ()
  (when (string= "irc.gitter.im" erc-session-server)
    (goto-char (point-min))
    (while (re-search-forward "[\r]" nil t)
      (replace-match (format "\n%s" (erc-format-my-nick))))))

(defun erc-gitter-format-markdown ()
  (when (string= "irc.gitter.im" erc-session-server)
    (save-restriction
      (let* ((marker erc-insert-marker)
             (beg (point-min))
             (end (point-max))
             (str (buffer-substring beg end))
             buf)
        (with-temp-buffer
          (insert str)
          (markdown-mode)
          (font-lock-fontify-region (point-min) (point-max))
          (setq buf (buffer-substring (point-min) (point-max))))
        (goto-char beg)
        (insert buf)
        (delete-region (point) (point-max))))))

;;;; Gitter link handling

(defun erc-gitter-browse-issue (link)
  (when (string= "irc.gitter.im" erc-session-server)
    (let* ((split (split-string link "#"))
           (channel (if (string= "" (car split))
                        (substring (buffer-name (current-buffer))
                                   1)
                      (car split)))
           (issue (cadr split))
           (url "https://github.com/%s/issues/%s"))
      (browse-url (format url channel issue)))))

;;;; Gitter-bot notification handling

(defun erc-gitter-gitter-is-fool ()
  "Add the gitter-bot to the list of fools.

It will be treated as any other fool."
  (interactive)
  (add-to-list 'erc-fools "gitter!gitter@gitter.im"))

(defun erc-gitter-gitter-is-no-fool ()
  "Remove the gitter-bot from the list of fools."
  (interactive)
  (setq erc-fools (delete "gitter!gitter@gitter.im" erc-fools)))

(provide 'erc-gitter)
;;; erc-gitter.el ends here
