;;; grammarly.el --- Grammarly API interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Shen, Jen-Chieh
;; Created date 2019-11-06 20:41:48

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-grammarly/grammarly
;; Version: 0.3.0
;; Package-Requires: ((emacs "26.1") (s "1.12.0") (request "0.3.0") (websocket "1.6"))
;; Keywords: convenience grammar api interface english

;; This file is NOT part of GNU Emacs.

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
;; Grammarly API interface.
;;

;;; Code:

(require 'cl-lib)
(require 's)
(require 'subr-x)
(require 'json)
(require 'request)
(require 'websocket)

(defgroup grammarly nil
  "Grammarly API interface."
  :prefix "grammarly-"
  :group 'text
  :link '(url-link :tag "Github" "https://github.com/emacs-grammarly/grammarly"))

(defcustom grammarly-username ""
  "Grammarly login username."
  :type 'string
  :group 'grammarly)

(defcustom grammarly-password ""
  "Grammarly login password."
  :type 'string
  :group 'grammarly)

(defconst grammarly--user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"
  "User agent.")

(defconst grammarly--browser-headers
  `(("User-Agent" . ())
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3")
    ("Accept-Language" . "en-GB,en-US;q=0.9,en;q=0.8")
    ("Cache-Control" . "no-cache")
    ("Pragma" . "no-cache"))
  "Header for simulate using a browser.")

(defconst grammarly--authorize-msg
  `(("origin" . "chrome-extension://kbfnbcaeplbcioakkpcpgfkobkghlhen")
    ("Cookie" . "$COOKIES$")
    ("User-Agent" . ,grammarly--user-agent))
  "Authorize message for Grammarly API.")

(defconst grammarly--init-msg
  '(("type" . "initial")
    ("token" . ())
    ("docid" . "dfad0927-7b35-e155-6de9-4a107053da35-43543554345")
    ("client" . "extension_chrome")
    ("protocolVersion" . "1.0")
    ("clientSupports" . ("free_clarity_alerts"
                         "readability_check"
                         "filler_words_check"
                         "sentence_variety_check"
                         "free_occasional_premium_alerts"))
    ("dialect" . "american")
    ("clientVersion" . "14.924.2437")
    ("extDomain" . "editpad.org")
    ("action" . "start")
    ("id" . 0))
  "Grammarly initialize message for verify use.")

(defconst grammarly--request-check
  '(("ch" . ("+0:0:$STR$:0"))
    ("rev" . 0)
    ("action" . "submit_ot")
    ("id" . 0))
  "Grammarly request package definition.")

(defvar grammarly-on-message-function-list nil
  "List of callback function when execute on message.")

(defvar grammarly-on-open-function-list nil
  "List of callback function when execute on open.")

(defvar grammarly-on-close-function-list nil
  "List of callback function when execute on close.")

(defvar grammarly--text ""
  "Current text that are going to check for.")

(defvar grammarly--client nil
  "Websocket for this client.")

(defvar grammarly--update-time 0.1
  "Run every this seconds until we received API request.")

(defvar grammarly--cookies ""
  "Record the cookie down.")

(defvar grammarly--timer nil
  "Universal timer for each await use.")

(defvar grammarly--start-checking-p nil
  "Flag to after we are done preparing; basically after authentication process.")

(defvar grammarly--show-debug-message nil
  "Flag to see if we show debug messages.")

;;
;; (@* "Util" )
;;

(defun grammarly--debug-message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when grammarly--show-debug-message
    (apply 'message fmt args)))

(defun grammarly--kill-websocket ()
  "Kill the websocket."
  (when grammarly--client
    (websocket-close grammarly--client)
    (setq grammarly--client nil)))

(defun grammarly--kill-timer ()
  "Kill the timer."
  (when (timerp grammarly--timer)
    (cancel-timer grammarly--timer)
    (setq grammarly--timer nil)))

(defun grammarly--execute-function-list (lst &rest args)
  "Execute all function LST with ARGS."
  (cond
   ((functionp lst) (apply lst args))
   ((listp lst) (dolist (fnc lst) (apply fnc args)))
   (t (user-error "[ERROR] Function does not exists: %s" lst))))

(defun grammarly-load-from-authinfo (&optional username)
  "Load Grammarly authentication info from auth-source.

Optionally pass the USERNAME, otherwise, it will be searched in the authinfo
file.  You will need to add a line in your authinfo file \"machine grammarly.com
login <YOUR-EMAIL> pass <YOUR-PASSWORD>\"."
  (require 'auth-source)
  (when-let* ((user-info (car (auth-source-search :host "grammarly.com" :user username)))
              (user (or username (plist-get user-info :user)))
              (secret (plist-get user-info :secret)))
    (setq grammarly-username user
          grammarly-password (funcall secret))
    t))

;;
;; (@* "Cookie" )
;;

(defvar grammarly--auth-cookie nil
  "Authorization cookie container.")

(defun grammarly--last-cookie (cookie cookies)
  "Check if current COOKIE the last cookie from COOKIES."
  (equal (nth (1- (length cookies)) cookies) cookie))

(defun grammarly--get-cookie-by-name (name)
  "Return cookie value by cookie NAME."
  (let ((len (length grammarly--auth-cookie)) (index 0) break cookie-val)
    (while (and (not break) (< index len))
      (let* ((cookie (nth index grammarly--auth-cookie))
             (cookie-name (car cookie)))
        (when (string= cookie-name name)
          (setq cookie-val (cdr cookie)
                break t)))
      (setq index (1+ index)))
    cookie-val))

(defun grammarly--form-cookie ()
  "Form all cookies into one string."
  (setq grammarly--auth-cookie nil)
  (let ((sec-cookies (request-cookie-alist ".grammarly.com" "/" t))
        (cookie-str "") cookie-name cookie-val)
    (dolist (cookie sec-cookies)
      (setq cookie-name (car cookie) cookie-val (cdr cookie)
            cookie-str
            (format "%s %s=%s%s" cookie-str cookie-name cookie-val
                    (if (grammarly--last-cookie cookie sec-cookies) "" ";")))
      (push (cons cookie-name cookie-val) grammarly--auth-cookie))
    (setq grammarly--auth-cookie (reverse grammarly--auth-cookie))
    (string-trim cookie-str)))

(defun grammarly--update-cookie ()
  "Refresh the cookie once."
  (setq grammarly--cookies (grammarly--form-cookie)))

(defun grammarly--get-cookie ()
  "Get cookie."
  (setq grammarly--start-checking-p nil
        grammarly--cookies "")  ; Reset to clean string.
  (request
    "https://grammarly.com/signin"
    :type "GET"
    :headers
    (append grammarly--browser-headers
            '(("Sec-Fetch-Mode" . "navigate")
              ("Sec-Fetch-Sit" . "same-origin")
              ("Sec-Fetch-User" . "?1")
              ("Upgrade-Insecure-Requests" . "1")
              ("Referer" . "https://www.grammarly.com/")))
    :success
    (cl-function
     (lambda (&key _response &allow-other-keys)
       (grammarly--update-cookie)
       (if (grammarly-premium-p)  ; Try login to use paid version.
           (grammarly--authenticate)
         (setq grammarly--start-checking-p t))))
    :error
    ;; NOTE: Accept, error.
    (cl-function
     (lambda (&rest args &key _error-thrown &allow-other-keys)
       (grammarly--debug-message "[ERROR] Error while getting cookie: %s" args)))))

;;
;; (@* "Login" )
;;

(defun grammarly-premium-p ()
  "Return non-nil means we are using premium version."
  (and (not (string-empty-p grammarly-username))
       (not (string-empty-p grammarly-password))))

(defun grammarly--authenticate ()
  "Login to Grammarly for premium version."
  (message "connecting as %s" grammarly-username)
  (request
    "https://auth.grammarly.com/v3/api/login"
    :type "POST"
    :headers
    `(("accept" . "application/json")
      ("accept-language" . "en-GB,en-US;q=0.9,en;q=0.8")
      ("content-type" . "application/json")
      ("user-agent" . ,grammarly--user-agent)
      ("x-client-type" . "funnel")
      ("x-client-version" . "1.2.2026")
      ("x-container-id" . ,(grammarly--get-cookie-by-name "gnar_containerId"))
      ("x-csrf-token" . ,(grammarly--get-cookie-by-name "csrf-token"))
      ("sec-fetch-site" . "same-site")
      ("sec-fetch-mode" . "cors")
      ("cookie" . ,(format "gnar_containrId=%s; grauth=%s; csrf-token=%s"
                           (grammarly--get-cookie-by-name "gnar_containerId")
                           (grammarly--get-cookie-by-name "grauth")
                           (grammarly--get-cookie-by-name "csrf-token"))))
    :data
    (json-encode
     `(("email_login" . (("email" . ,grammarly-username)
                         ("password" . ,grammarly-password)
                         ("secureLogin" . "false")))))
    :success
    (cl-function
     (lambda (&key _response &allow-other-keys)
       (setq grammarly--start-checking-p t)))
    :error
    ;; NOTE: Accept, error.
    (cl-function
     (lambda (&rest args &key _error-thrown &allow-other-keys)
       (setq grammarly--start-checking-p t)  ; Go back and use anonymous version
       (grammarly--debug-message
        "[ERROR] Error while authenticating login: %s" args)))))

;;
;; (@* "WebSocket" )
;;

(defun grammarly--form-authorize-list ()
  "Form the authorize list."
  (let ((auth (copy-sequence grammarly--authorize-msg)))
    ;; NOTE: Here we directly point it to the `$COOKIES$' keyword.
    (setcdr (nth 1 auth) grammarly--cookies)
    auth))

(defun grammarly--form-check-request (text)
  "Form a check request by TEXT."
  (let ((req (copy-sequence grammarly--request-check)))
    ;; NOTE: Here we directly point it to the `$STR$' keyword.
    (setf (nth 0 (cdr (nth 0 req))) (s-replace "$STR$" text "+0:0:$STR$:0"))
    req))

(defun grammarly--after-got-cookie ()
  "Execution after received all needed cookies."
  (grammarly--kill-websocket)
  (setq
   grammarly--client
   (websocket-open
    "wss://capi.grammarly.com/freews"
    :custom-header-alist (grammarly--form-authorize-list)
    :on-open
    (lambda (_ws)
      (grammarly--execute-function-list grammarly-on-open-function-list)
      ;; Verify valid client connection.
      (websocket-send-text grammarly--client (json-encode grammarly--init-msg))
      (websocket-send-text grammarly--client (json-encode (grammarly--form-check-request grammarly--text))))
    :on-message
    (lambda (_ws frame)
      (grammarly--execute-function-list grammarly-on-message-function-list (websocket-frame-payload frame))
      (grammarly--default-callback (websocket-frame-payload frame)))
    :on-error
    (lambda (_ws _type err)
      (grammarly--debug-message
       "[ERROR] Connection error while opening websocket: %s" err))
    :on-close
    (lambda (_ws)
      (grammarly--execute-function-list grammarly-on-close-function-list)))))

;;
;; (@* "Core" )
;;

(defun grammarly--reset-timer (fnc pred)
  "Reset the timer for the next run with FNC and PRED."
  (grammarly--kill-timer)
  (if (funcall pred)
      (setq grammarly--timer
            (run-with-timer grammarly--update-time nil
                            'grammarly--reset-timer fnc pred))
    (funcall fnc)))

(defun grammarly--default-callback (data)
  "Default callback, print out DATA."
  (when (string-match-p "\"action\":\"finished\"" data)
    ;; Clean up after last response action received.
    (grammarly--kill-websocket)))

;;;###autoload
(defun grammarly-check-text (text)
  "Send the TEXT to check."
  (if (or (not (stringp text)) (string-empty-p text))
      (user-error "[ERROR] Text can't be 'nil' or 'empty'")
    (setq grammarly--text text)
    (grammarly--get-cookie)
    ;; Delay, until we get the initial cookie.
    (grammarly--reset-timer #'grammarly--after-got-cookie
                            '(lambda () (null grammarly--start-checking-p)))))

(provide 'grammarly)
;;; grammarly.el ends here
