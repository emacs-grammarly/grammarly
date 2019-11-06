;;; grammarly.el --- Grammarly API interface.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-06 20:41:48

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Grammarly API interface.
;; Keyword: grammar api interface english
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;; URL: https://github.com/jcs090218/grammarly

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

(require 'request)

(defgroup grammarly nil
  "Grammarly API interface."
  :prefix "grammarly-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/grammarly"))


(defvar-local grammarly--cookie ""
  "Record the cookie down.")


(defun grammarly--get-cookie ()
  "Get cookie."
  (request
   "https://grammarly.com/"
   :type "GET"
   :success
   (cl-function
    (lambda (&key response  &allow-other-keys)
      (setq grammarly--cookie "")  ; Reset to clean string.
      (let* ((raw-headers (request-response--raw-header response))
             (cookies (split-string raw-headers "Set-Cookie: "))
             (index 1))
        (while (< index (length cookies))
          (setq grammarly--cookie
                (concat grammarly--cookie
                        (nth 0 (split-string (nth index cookies) " ")) " "))
          (setq index (1+ index))))))
   :error
   ;; NOTE: Accept, error.
   (cl-function
    (lambda (&rest args &key _error-thrown &allow-other-keys)
      (user-error "[ERROR] Error while getting cookie")))))


(message "%s" (grammarly--get-cookie))


(provide 'grammarly)
;;; grammarly.el ends here
