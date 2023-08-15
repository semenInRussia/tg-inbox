;;; tg-inbox.el --- Sync inbox.org with Telegram chat -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((simple-httpd "1.5"))

;; This program is free software: you can redistribute it and/or modify
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

;; Sync inbox.org with Telegram chat.
;;
;; You call `tg-inbox-sync' and messages from the bot will be inserted in the
;; current buffer as `org-mode' headings with TODO label.

;;; Code:

(require 'subr-x)  ; for `thread-last'
(defvar url-http-end-of-headers)

(defvar tg-inbox-sync-time-file
  (thread-last
    ;; this file name
    (cond (load-in-progress
           load-file-name)
          (byte-compile-current-file
           byte-compile-current-file)
          (t (buffer-file-name)))
    ;; this directory
    file-name-directory
    ;; the target filename
    (expand-file-name "tg-inbox-sync-time.txt"))
  "The filename where `tg-inbox' stores the time of the last sync.")

(defvar tg-inbox-bot-token "6580506179:AAGf7VtyNWy1GGseeXBdIwa6mFpziIWsi_U"
  "Token of the bot on which you send your tasks.")

;;; `org-mode' functions

(defun tg-inbox-sync ()
  "Insert all new messages from the Telegram Bot as `org-mode' tasks.

Insert these messages to the end of the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (tg-inbox--ensure-empty-line)
    (mapc #'tg-inbox--insert-task-msg
          (tg-inbox--new-messages))))

(defun tg-inbox--insert-task-msg (msg)
  "Insert an `org-mode' heading as an inbox task with a MSG."
  (insert "* TODO " msg)
  (newline))


;;; Telegram API Internals

(defun tg-inbox--is-new-msg-p (msg)
  "Return non-nil if a given Telegram MSG is sent after the last sync time.

MSG is an alist which was parsed from JSON returned with the Telegram API method
getUpdates"
  ;; (message
  ;;  (message_id . 1)
  ;;  (from
  ;;   (id . 664167507)
  ;;   (is_bot . :false)
  ;;   (first_name . "Semen")
  ;;   (last_name . "Khramtsov")
  ;;   (username . "semenInRussia")
  ;;   (language_code . "en"))
  ;;  (chat
  ;;   (id . 664167507)
  ;;   (first_name . "Semen")
  ;;   (last_name . "Khramtsov")
  ;;   (username . "semenInRussia")
  ;;   (type . "private"))
  ;;  (date . 1692105224)
  ;;  (text . "/start")
  ;;  (entities .
  ;;            [((offset . 0)
  ;;              (length . 6)
  ;;              (type . "bot_command"))]))
  (let ((last-sync-time
         (with-temp-buffer
           (insert-file-contents tg-inbox-sync-time-file)
           (string-to-number
            (buffer-string)))))
    (> (alist-get 'date msg)
       last-sync-time)))

(defun tg-inbox--new-messages ()
  "Return a list of new messages within the Telegram bot."
  (thread-last
    (tg-inbox--fetch-json "getUpdates")
    (alist-get 'result)
    (mapcar 'cadr)
    ;; functions on messages
    (seq-filter
     #'tg-inbox--is-new-msg-p)
    tg-inbox--change-last-sync-time
    ;;
    (mapcar
     (apply-partially #'alist-get 'text))
    ;; remove nil-values, text is nil if the message is emoji or file
    (seq-filter #'identity)
    ;; remove useless whitespaces around messages
    (mapcar #'string-trim)
    ;; also remove /start command
    (remove "/start")))

(defun tg-inbox--fetch-json (method)
  "Fetch a JSON from the Telegram API for tg-inbox-bot with a given METHOD."
  (let ((url (tg-inbox--format-url method)))
    (with-current-buffer (url-retrieve-synchronously url)
      (json-parse-string
       (buffer-substring (1+ url-http-end-of-headers)
                         (point-max))
       :object-type 'alist))))

(defun tg-inbox--format-url (method)
  "Format an URL to the Telegram API for tg-inbox-bot with a given METHOD."
  (format "https://api.telegram.org/bot%s/%s"
          tg-inbox-bot-token
          method))

;;; Internals

(defun tg-inbox--ensure-empty-line ()
  "Ensure that this line is empty, it it's true insert new line."
  (when (> (point) (line-beginning-position))
    ;; when the cursor isn't located at the beginning of line
    (newline)))

(defun tg-inbox--change-last-sync-time (msgs)
  "Accept Telegram MSGS and change the `tg-inbox-sync-time-file'.

Note that it is one of functions which will be applied to list of MSGS and the
result of this function will be used after"
  (when msgs
    (with-temp-buffer
      ;; insert the sync time to this temp buffer
      (thread-last
        (last msgs)
        car
        (alist-get 'date)
        number-to-string
        insert)
      ;; write new info
      (write-region (point-min) (point-max)
                    tg-inbox-sync-time-file)))
  msgs)

(unless (file-exists-p tg-inbox-sync-time-file)
  (find-file-text tg-inbox-sync-time-file)
  (save-buffer)
  (kill-buffer (find-file-text tg-inbox-sync-time-file)))

(provide 'tg-inbox)
;;; tg-inbox.el ends here
