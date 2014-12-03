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

(defcustom erc-gitter-bot-handling 't
  "How to handle messages from gitter integration.

'buffer will send messages to a separate buffer
nil will do nothing with it.
non-nil values will set `gitter' as a fool."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Separate buffer" 'buffer)
                 (symbol :tag "Treat as Fool" 't))
  :group 'erc-gitter)

(defvar erc-gitter-button
  '("\\(\\w+/\\w+\\)?#\\([0-9]+\\)" 0
    (string= "irc.gitter.im" erc-session-server)
    erc-gitter-browse-issue 0)
  "Match github issue links that are sent to ERC.")

(defvar erc-gitter-compose nil
  "Non-nil when using erc-gitter-compose to override return.")

(define-erc-module gitter nil
  "Enable Gitter features in ERC."
  ((when erc-gitter-bot-handling
     (erc-gitter-gitter-is-fool))
   (when (eq erc-gitter-bot-handling 'buffer)
     (add-hook 'erc-text-matched-hook 'erc-gitter-bot-to-buffer))
   (add-hook 'erc-send-pre-hook #'erc-gitter-send-code)
   (add-hook 'erc-send-modify-hook #'erc-gitter-display-code)
   (add-hook 'erc-insert-modify-hook #'erc-gitter-format-markdown)
   (add-hook 'erc-send-modify-hook #'erc-gitter-format-markdown)
   (add-to-list 'erc-button-alist erc-gitter-button)
   (add-hook 'erc-join-hook #'erc-gitter-if-gitter)
   (and (ad-enable-advice 'erc-server-filter-function
                          'before
                          'erc-gitter-multiline)
        (ad-activate 'erc-server-filter-function)))
  ((erc-gitter-gitter-is-no-fool)
   (remove-hook 'erc-text-matched-hook 'erc-gitter-bot-to-buffer)
   (remove-hook 'erc-send-pre-hook #'erc-gitter-send-code)
   (remove-hook 'erc-send-modify-hook #'erc-gitter-display-code)
   (remove-hook 'erc-insert-modify-hook #'erc-gitter-format-markdown)
   (remove-hook 'erc-send-modify-hook #'erc-gitter-format-markdown)
   (setq erc-button-alist (delete erc-gitter-button erc-button-alist))
   (erc-gitter-minor-mode 0)
   (and (ad-disable-advice 'erc-server-filter-function
                           'before
                           'erc-gitter-multiline)
        (ad-activate 'erc-server-filter-function))))

;;;; Minor mode keybindings

(defvar erc-gitter-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'erc-gitter-return)
    (define-key map (kbd "S-<return>") #'erc-gitter-compose)
    (define-key map (kbd "C-<return>") #'erc-gitter-send)
    (define-key map (kbd "C-c c") #'erc-gitter-insert-code-block)
    (define-key map (kbd "C-c n") #'erc-gn-switch-to-notif)
    map)
  "Keymap for ERC-Gitter mode.")

(define-minor-mode erc-gitter-minor-mode
  "Minor mode to provide keybindings for `erc-gitter'."
  :keymap erc-gitter-minor-mode-map
  :lighter " Gitter")

(defun erc-gitter-if-gitter ()
  (if (string= "irc.gitter.im" erc-session-server)
      (erc-gitter-minor-mode 1)
    nil))

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

;;;; Multiline receiving

(defadvice erc-server-filter-function (before erc-gitter-multiline activate)
  "Include message details in messages that include `^M' linebreaks.
This allows for proper treatment of code blocks and multi-line
messages sent from ERC."
  (when (string-match-p "erc-irc.gitter.im-6667" (process-name process))
    (let ((msg (and (string-match "\\\(:.*:\\\)" string)
                    (match-string 1 string))))
      (setq string (replace-regexp-in-string ""
                                             (format "%s" msg)
                                             string)))))

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

;;;;; Gitter notification buffer

(defun erc-gitter-bot-to-buffer (match-type nickuserhost message)
  (when (and (eq match-type 'fool)
             (string= "gitter!gitter@gitter.im" nickuserhost))
    (let ((buf (erc-gn-make-buffer)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert message))
      (erc-hide-fools match-type nickuserhost message))))

(defun erc-gn-make-buffer ()
  (when (eq erc-gitter-bot-handling 'buffer)
    (let ((buf (get-buffer-create "*Gitter Notifications*")))
      (with-current-buffer buf
        (gitter-notifications-mode))
      buf)))

(defvar gitter-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" nil) ; nothing to revert
    (define-key map "q" #'bury-buffer)
    ;; (define-key map (kbd "<return>") #'erc-gn-visit)
    ;; (define-key map "p" #'erc-gn-previous)
    ;; (define-key map "n" #'erc-gn-next)
    map))

(define-derived-mode gitter-notifications-mode special-mode
  "Gitter-Notifications"
  "Major mode used in the \"*Gitter Notifications*\" buffer."
  :group 'erc-gitter)

(defun erc-gn-switch-to-notif ()
  (interactive)
  (if (eq erc-gitter-bot-handling 'buffer)
      (switch-to-buffer (erc-gn-make-buffer))
    (user-error "Gitter notifications are not being tracked.")))

;;;; Markdown and compose mode

(defun erc-gitter-return ()
  "Check for codeblock then send current line.

If the line starts with three backticks, enable
`erc-gitter-compose' and adjust keybindings."
  (interactive)
  (cond
   (erc-gitter-compose
    (erc-gitter-compose))
   ((save-excursion
      (erc-bol)
      (looking-at "```"))
    (erc-gitter-compose)
    (save-excursion
      (newline)
      (insert "```")))
   ((erc-gitter-send))))

(defun erc-gitter-send ()
  (interactive)
  (when erc-gitter-compose
    (setq erc-gitter-compose nil)
    (setq overriding-local-map nil))
  (erc-send-current-line))

(defun erc-gitter-compose ()
  (interactive)
  (setq erc-gitter-compose 't)
  (newline))

(defun erc-gitter-insert-code-block ()
  (interactive)
  (insert "```")
  (erc-gitter-compose)
  (save-excursion
    (newline)
    (insert "```")))

(provide 'erc-gitter)
;;; erc-gitter.el ends here
