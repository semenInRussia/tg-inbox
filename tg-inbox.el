;;; tg-inbox.el --- Sync inbox.org with the Telegram chat -*- lexical-binding: t; -*-

;; Copyright (C) 2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

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
;; current buffer as `org-mode' headings with TODO label.  WOW!

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
  "Token of the bot on which you send your tasks.

Please, generate your own token using @BotFather (https://telegram.me/BotFather)")

(defvar tg-inbox-msgs '()
  "List of just accepted messages from the Telegram Bot using Telegram API.

It will be updated after every `tg-inbox--new-msgs' call.  You can use this
variable inside `tg-inbox-sync-post-hook'.  Every message is alist with the
fields of struct Message from the offical API

https://core.telegram.org/bots/api#message")

(defvar tg-inbox-all-msgs '()
  "Like `tg-inbox-msgs', but here filtering is not happened.")

(defvar tg-inbox-sync-post-hook '(tg-inbox--change-last-sync-time)
  "List of functions which will be called after `tg-inbox-sync'.

Inside this function you can use variable `tg-inbox-msgs' and
`tg-inbox-all-msgs' which have list of messages alists (see their docstrings).")

(defvar tg-inbox-sync-pre-hook '()
  "List of functions which will be called before `tg-inbox-sync'.")

(defvar tg-inbox-filter-msgs-functions '(tg-inbox--filter-new-msgs)
  "The list of functions which should filter accepted messages from the bot.

Each element of this variable should accept 1 argument messages and return
filtered messages.  Each of accepted and result messages is alist with fields of
struct Message from the offical API, see
https://core.telegram.org/bots/api#message

The filtered messages you can take with `tg-inbox--new-messages'.

Defaults to one function which filter messages with the last `tg-inbox-sync'")

(defvar tg-inbox-polling
  (* 31                                 ; days in 1month
     24                                 ; hours
     60                                 ; minutes
     60                                 ; seconds in 1month
     )
  "Here the ammount of seconds after which messages in Telegram will be deleted.

Defaults to 1month, it means that when you send a message to the bot, then
message will be located inside Telegram API database within 1month and when you
call `tg-inbox-sync' it receives messages from Telegram API in the last month.
After 1month")

;;; Public Commands

;;;###autoload
(defun tg-inbox-sync ()
  "Insert all new messages from the Telegram Bot as `org-mode' tasks.

Insert these messages to the end of the current buffer.  It tries to insert only
new messages with look up content of `tg-inbox-sync-time-file' which will be
updated after every `tg-inbox-sync' call.

Before and after insertion of new messages `tg-inbox' run hooks
`tg-inbox-sync-pre-hook' and `tg-inbox-sync-post-hook' respectively

Return the list of inserted messages"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (tg-inbox--ensure-empty-line)
    (run-hooks 'tg-inbox-sync-pre-hook)
    (mapc #'tg-inbox--insert-task-msg
          (tg-inbox--text-msgs))
    (run-hooks 'tg-inbox-sync-post-hook)
    tg-inbox-msgs))

;;;###autoload
(defun tg-inbox-create-internal-files ()
  "Create some files in which `tg-inbox' needed for correct work.

Call it before the first `tg-inbox-sync' call"
  (interactive)
  (unless (file-exists-p tg-inbox-sync-time-file)
    (find-file tg-inbox-sync-time-file)
    (save-buffer)
    (kill-buffer (find-file tg-inbox-sync-time-file))))

;;; `org-mode' Internals

(defun tg-inbox--insert-task-msg (msg)
  "Insert an `org-mode' heading as an inbox task with a MSG."
  (insert "* TODO " msg)
  (newline))

;;; Telegram API Internals

(defun tg-inbox--msg-to-last-chat (msg)
  "Send a text MSG to the chat from where was accepted the last user msg.

You can use it inside `tg-inbox-sync-pre-hook' and `tg-inbox-sync-post-hook'
hooks or after `tg-inbox--new-msgs'.  NOTE that if messages didn't accepted
then the last chat it can't be detected"
  (tg-inbox--send-msg msg
                      (tg-inbox--last-chat-id)))

(defun tg-inbox--last-chat-id ()
  "Return the chat ID from where was accepted the last user msg.

You can use it inside `tg-inbox-sync-pre-hook' and `tg-inbox-sync-post-hook'
hooks or after `tg-inbox--new-msgs'.  NOTE that if messages didn't accepted
then the last chat it can't be detected the result is nil value"
  (thread-last
    (last tg-inbox-msgs)
    (car)
    (alist-get 'chat)
    (alist-get 'id)))

(defun tg-inbox--send-msg (msg chat-id)
  "Send a text MSG to the chat with CHAT-ID."
  (tg-inbox--fetch-json "sendMessage"
                        `((chat_id . ,chat-id)
                          (text . ,msg))))

(defun tg-inbox--is-new-msg-p (msg)
  "Return non-nil if a given Telegram MSG is sent after the last sync time.

MSG is an alist which was parsed from JSON returned with the Telegram API method
getUpdates.

\"New message\" means that the date of this messages is later than content of
`tg-inbox-sync-time-file'.  The content of this file will be updated after every
`tg-inbox-sync' call (or call `tg-inbox--change-last-sync-time')"
  (declare (pure t) (side-effect-free t))
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

(defun tg-inbox--new-msgs ()
  "Return a list of new messages within the Telegram bot.

Note that each of result messages is alist which is parsed JSON from getUpdates
Telgram API method answer.  See the schema of the Message struct here:
https://core.telegram.org/bots/api#message

\"New message\" means that the date of this messages is later than content of
`tg-inbox-sync-time-file'.  The content of this file will be updated after every
`tg-inbox-sync' call (or call `tg-inbox--change-last-sync-time')."
  (thread-last
    (tg-inbox--fetch-json "getUpdates" `((polling . ,tg-inbox-polling)))
    (alist-get 'result)
    (print)
    (mapcar
     (apply-partially #'alist-get 'message))
    ;; change the variable for hooks
    (setq tg-inbox-all-msgs)
    ;; filter messages by time and other things
    (tg-inbox--filter-with-hook
     'tg-inbox-filter-msgs-functions)
    ;; change the variable for hooks
    (setq tg-inbox-msgs)))

(defun tg-inbox--text-msgs ()
  "Return a list of strings: new messages within the Telegram bot.

\"New message\" means that the date of this messages is later than content of
`tg-inbox-sync-time-file'.  The content of this file will be updated after every
`tg-inbox-sync' call (or call `tg-inbox--change-last-sync-time')."
  (declare (pure t) (side-effect-free t))
  (thread-last
    (tg-inbox--new-msgs)
    (mapcar
     (apply-partially #'alist-get 'text))
    ;; remove nil-values, text is nil if the message is emoji or file
    (seq-filter #'identity)
    ;; remove useless whitespaces around messages
    (mapcar #'string-trim)
    ;; also remove /start command
    (remove "/start")))

(defun tg-inbox--fetch-json (method &optional params)
  "Fetch a JSON from the Telegram API for tg-inbox-bot with a given METHOD.

Pass to the method data-alist PARAMS."
  (let ((url (tg-inbox--format-url method params)))
    (with-current-buffer (url-retrieve-synchronously url)
      (json-parse-string
       (buffer-substring (1+ url-http-end-of-headers)
                         (point-max))
       :object-type 'alist))))

(defun tg-inbox--format-url (method &optional params)
  "Format an URL to the Telegram API for tg-inbox-bot with a given METHOD.

Pass to the method data-alist PARAMS."
  (declare (pure t) (side-effect-free t))
  (format "https://api.telegram.org/bot%s/%s%s"
          tg-inbox-bot-token
          method
          (tg-inbox--query-string params)))

(defun tg-inbox--query-string (alist)
  "Return a query-string from a given ALIST.

It's a small wrapper around `url-build-query-string'.  Instead of the original
one this function accept an ALIST as a query data and add ? at the start of a
string (if an ALIST isn't empty, otherwise it return empty string)."
  (declare (pure t) (side-effect-free t))
  (if (not alist)
      ""
    (concat
     "?"
     (url-build-query-string
      (mapcar (lambda (bind)
                ;; a cons '(a . b) to list '(a b)
                (list (car bind) (cdr bind)))
              alist)))))

;;; Filter Telegram Messages functions

(defun tg-inbox--filter-new-msgs (msgs)
  "Remove messages from MSGS which later than last sync time.

The last sync time is stored inside file at `tg-inbox-sync-time-file' and
updated after every either `tg-inbox-sync' or `tg-inbox--change-last-sync-time'
call.

Each of msgs is alist with the the schema of the Message struct
https://core.telegram.org/bots/api#message

It's one of `tg-inbox-filter-msgs-functions'."
  (seq-filter #'tg-inbox--is-new-msg-p
              msgs))

;;; Some Optional Hooks

(defun tg-inbox-maybe-send-done-msg ()
  "Send to the chat message that syncing was done if messages was accepted.

This is a hook to `tg-inbox-sync-post-hook', but can be useful also outside it,
if you call `tg-inbox--new-msgs' before

If you need that the bot answers on syncing messages, that add this function as
hook to `tg-inbox-sync-post-hook' using anything like the following line:

\(add-hook \\='tg-inbox-sync-post-hook #\\='tg-inbox-maybe-send-done-msg)

Now, when you send some messages to the bot and call `tg-inbox-sync' the bot
tell \"Done\""
  (when tg-inbox-msgs
    (tg-inbox--msg-to-last-chat "Done...")
    (tg-inbox--msg-to-last-chat (format "%s messages was synchronized"
                                        (length tg-inbox-msgs)))))

;;; Internals

(defun tg-inbox--ensure-empty-line ()
  "Ensure that this line is empty, it it's true insert new line."
  (when (> (point) (line-beginning-position))
    ;; when the cursor isn't located at the beginning of line
    (newline)))

(defun tg-inbox--change-last-sync-time ()
  "Accept Telegram msgs and change the `tg-inbox-sync-time-file'.

Note that it is one of functions which will be applied to list of MSGS and the
result of this function will be used after.  See `tg-inbox-sync-post-hook'"
  (when tg-inbox-msgs
    (with-temp-buffer
      ;; insert the sync time to this temp buffer
      (thread-last
        (last tg-inbox-msgs)
        car
        (alist-get 'date)
        number-to-string
        insert)
      ;; write new info
      (write-region (point-min) (point-max)
                    tg-inbox-sync-time-file))))

(defun tg-inbox--filter-with-hook (hook &rest args)
  "Run HOOK with the specified arguments ARGS.

HOOK should be a symbol, a hook variable.  The value of HOOK may be nil, a
function, or a list of functions.  Call each function in order with arguments
ARGS, stopping at the first one that returns nil, and return nil.  Otherwise (if
all functions return non-nil, or if there are no functions to call), return the
final value."
  (let ((funcs (ensure-list (eval hook)))
        (result t))
    (while (and funcs result)
      (setq result (apply (car funcs) args))
      (setq funcs (cdr funcs)))
    result))

(provide 'tg-inbox)
;;; tg-inbox.el ends here
