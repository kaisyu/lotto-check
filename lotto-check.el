;;; lotto-check.el --- Simple API for Korean Lotto 645.   -*- emacs-lisp -*-
;;; Copyright (C) 2010  Sang-gi Lee <kaisyu@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version 0.1.4
;; Author: Sang-gi Lee <kaisyu@gmail.com>

;; Requirements:
;; * Emacs 22+

;; Install:
;; * append following lines to your start-up script file (e.g. .emacs)
;;   (add-to-list 'load-path "<lotto-check.el path>")
;;   (require 'lotto-check)

;; Usage:
;; * interactive functions
;;   - M-x lotto-retrieve-numbers-i   : retrieve a specific lotto info
;;   - M-x lotto-check-numbers-list-i : check lotto numbers
;;   - M-x lotto-save-db-to-file-i    : save lotto database to the local file(`lotto-database-file')
;;   - M-x lotto-load-db-to-file-i    : load lotto database from the local file(`lotto-database-file')
;; * API functions
;;   - lotto-retrieve-numbers   : retrieve a specific lotto info
;;   - lotto-check-numbers-list : check lotto numbers
;;   - lotto-save-db-to-file    : save lotto database to the local file(`lotto-database-file')
;;   - lotto-load-db-from-file  : load lotto database from the local file(`lotto-database-file')


(eval-when-compile (require 'cl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup lotto nil
  "Simple API for Korean Lotto 645"
  :prefix "lotto-"
  :version "22.0"
  :group 'applications)


(defcustom lotto-info-retrieve-func-custom nil
  "a custom function to retrieve lotto info.

To enable this variable, you must set `lotto-info-retrieve-func' to `lotto-info-retrieve-func-custom'."
  :type 'function
  :group 'lotto)


(defcustom lotto-info-retrieve-func 'lotto-retrieve-numbers-from-lotto-k
  "a function to retrieve lotto info"
  :type 'function
  :options '(lotto-retrieve-numbers-from-lotto-k
             lotto-retrieve-numbers-from-naver
             lotto-retrieve-numbers-from-daum
             lotto-retrieve-numbers-from-nate
             lotto-retrieve-numbers-from-645lotto
             lotto-info-retrieve-func-custom)
  :group 'lotto)


(defcustom lotto-database-file "~/.lotto-database"
  "a file to store lotto database"
  :type 'file
  :group 'lotto)


(defcustom lotto-use-buffer-for-message t
  "use an own buffer for messages"
  :type 'boolean
  :group 'lotto)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *lotto-database*
  nil
  "Local cache database for Lotto info")


(defconst +lotto-message-buffer+
  "*lotto-check-messages*"
  "Message buffer for lotto-check module")


(defconst +lotto-msg-window-height+
  5
  "Height of the Lotto message window")


(defconst +http-retrieved-page-contents-buffer+
  "*http-retrieved-page-contents*"
  "Buffer for the retrieved page contents by http")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lotto-mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst +lotto-keywords+
  '(("Game [0-9]+:" . font-lock-keyword-face)
    ("Try #[0-9]+" . font-lock-keyword-face)
    ("Grade:\\|Matched Numbers:" . font-lock-keyword-face)
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    ("OK:" . font-lock-function-name-face))
  "Keywords for lotto-mode")


(define-derived-mode lotto-mode fundamental-mode
  "lotto-mode"
  "Major mode for Lotto Info"

  ;; code for syntax highlighting
  (setq font-lock-defaults '(+lotto-keywords+))

  ;; make the buffer read-only
  (setq buffer-read-only t)

  ;; key bindings
  (local-set-key (kbd "q") 'lotto-hide-message-buffer)
  (local-set-key (kbd "g") 'lotto-retrieve-numbers-i)
  (local-set-key (kbd "r") 'lotto-retrieve-numbers-i)
  (local-set-key (kbd "c") 'lotto-check-numbers-list-i)
  (local-set-key (kbd "l") 'lotto-load-db-from-file-i)
  (local-set-key (kbd "s") 'lotto-save-db-to-file-i))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lotto-save-db-to-file ()
  "save the contents of `*lotto-database*' to the local file(`lotto-database-file')"
  (unless (hash-table-p *lotto-database*)
    (return))
  (with-temp-buffer
    (insert (format "%s" *lotto-database*))
    (when (file-writable-p lotto-database-file)
      (write-region (point-min)
                    (point-max)
                    lotto-database-file)
      t)))


(defun lotto-load-db-from-file ()
  "load the contents of `*lotto-database*' from the local file(`lotto-database-file')"
  (with-temp-buffer
    (cond ((file-readable-p lotto-database-file)
           (insert-file-contents lotto-database-file)
           (goto-char (point-min))
           (setq *lotto-database* (read-from-whole-string (buffer-string)))
           t)
          (t
           (setq *lotto-database* (make-hash-table))
           nil))))


(defun lotto-gen-site-url-lotto-k (gno)
  "return url for Lotto_K's lotto info site

ex) (lotto-gen-site-url-lotto-k 101)"
  (format "http://lotto.kaisyu.com/api?method=get&type=emacs&gno=%d"
          (if gno gno 0)))


(defun lotto-retrieve-numbers-from-lotto-k (gno)
  "retrieve lotto numbers from Lotto_K
GNO: game no.
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-from-lotto-k 395)
    => ((11 15 20 26 31 35)
        7)"
  (let ((buf1 (url-retrieve-synchronously
               (lotto-gen-site-url-lotto-k gno)))
        (obj nil))
    (set-buffer buf1)
    (goto-char (point-min))
    (ignore-errors
      (unwind-protect
          (progn
            (re-search-forward "^(.+$")
            (setq obj (read-from-whole-string (match-string 0))))
        (kill-buffer buf1))
      (list (cdr (assoc 'nums obj)) (cdr (assoc 'bnum obj))))))


(defun lotto-retrieve-numbers-base (gno url-func rexp)
  "retrieve lotto numbers
GNO: game no.
URL-FUNC: a function for lotto info url
REXP: number string pattern
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-base 395 lotto-gen-site-url-naver \"ball\\\\([0-9]+\\\\).gif\")
    => ((11 15 20 26 31 35)
        7)"
  (let ((buf1 (url-retrieve-synchronously
               (funcall url-func gno)))
        (nums ())
        (bnum))
    (set-buffer buf1)
    (goto-char (point-min))
    (ignore-errors
      (unwind-protect
          (dotimes (i 7)
            (re-search-forward rexp)
            (push (string-to-number (match-string 1)) nums))
        (kill-buffer buf1))
      (setq bnum (pop nums))
      (list (reverse nums) bnum))))


(defun lotto-gen-site-url-daum (gno)
  "return url for Daum's lotto info site

ex) (lotto-gen-site-url-daum 101)"
  (if gno
      (format "http://search.daum.net/search?q=%%B7%%CE%%B6%%C7%d" gno)
    "http://search.daum.net/search?q=%B7%CE%B6%C7"))


(defun lotto-retrieve-numbers-from-daum (gno)
  "retrieve lotto numbers from Daum's lotto8
GNO: game no.
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-from-daum 395)
    => ((11 15 20 26 31 35)
        7)"
  ;; FIXME: `date-to-time' error on running the `url-retrieve-synchronously' function
  (let ((lval (lotto-retrieve-numbers-base
               gno
               'lotto-gen-site-url-daum
               "ball_\\([0-9]+\\).gif")))
    (if (or (member 0 (car lval))
            (zerop (cadr lval)))
        nil
      lval)))


(defun lotto-gen-site-url-naver (gno)
  "return url for Naver's lotto info site

ex) (lotto-gen-site-url-naver 101)"
  (if gno
      (format "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%d%%C8%%B8%%B7%%CE%%B6%%C7" gno)
    "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%B7%CE%B6%C7"))


(defun lotto-retrieve-numbers-from-naver (gno)
  "retrieve lotto numbers from Naver
GNO: game no.
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-from-naver 395)
    => ((11 15 20 26 31 35)
        7)"
  (lotto-retrieve-numbers-base
   gno
   'lotto-gen-site-url-naver
   "ball\\([0-9]+\\).gif"))


(defun lotto-gen-site-url-nate (gno)
  "return url for Nate's lotto info site

ex) (lotto-gen-site-url-nate 101)"
  (if gno
      (format "http://search.nate.com/search/all.html?q=%%B7%%CE%%B6%%C7%d" gno)
    "http://search.nate.com/search/all.html?q=%B7%CE%B6%C7"))


(defun lotto-retrieve-numbers-from-nate (gno)
  "retrieve lotto numbers from Nate
GNO: game no.
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-from-nate 395)
    => ((11 15 20 26 31 35)
        7)"
  (lotto-retrieve-numbers-base
   gno
   'lotto-gen-site-url-nate
   "ball\\([0-9]+\\).gif"))


(defun lotto-gen-site-url-645lotto (gno)
  "return url for 645lotto's lotto info site

ex) (lotto-gen-site-url-645lotto 101)"
  (if gno
      (format "http://www.645lotto.net/resultall/%d.asp" gno)
    "http://www.645lotto.net/resultall/dummy.asp"))


(defun lotto-retrieve-numbers-from-645lotto (gno)
  "retrieve lotto numbers from 645lotto.net
GNO: game no.
return: ((num_list) bonus_num)

ex) (lotto-retrieve-numbers-from-645lotto 395)
    => ((11 15 20 26 31 35)
        7)"
  (lotto-retrieve-numbers-base
   gno
   'lotto-gen-site-url-645lotto
   "Ball[\\\t ]:[\\\t ]\\\"\\([0-9]+\\)\\\""))


(defun lotto-retrieve-numbers-from-local-db (gno)
  "retrieve lotto numbers from local db
GNO: game no.
return: lotto info. ((num_list) bonus_num) OR nil if the info does not exist on the local db.

ex) (lotto-retrieve-numbers-from-local-db 395)
    => ((11 15 20 26 31 35)
        7)"
  (when (hash-table-p *lotto-database*)
    (gethash gno *lotto-database*)))


(defun lotto-retrieve-numbers (gno)
  "retrieve lotto numbers
GNO: game no.
return: lotto info. ((num_list) bonus_num)

ex) (lotto-retrieve-numbers 395)
    => ((11 15 20 26 31 35)
        7)"
  (let ((lval (lotto-retrieve-numbers-from-local-db gno)))
    (or lval
        (progn
          (setq lval (funcall lotto-info-retrieve-func gno))
          (if lval
              (puthash gno lval *lotto-database*))))))


(defun lotto-check-numbers (lotto-nums my-nums)
  "check the given lotto numbers
LOTTO-NUMS: lotto numbers (6 numbers and a bonus number)
MY-NUMS: my numbers to check (6 numbers)
return: result of check. (grade (matched_numbers))

ex) (lotto-check-numbers '((11 15 16 18 31 34) 44) '(11 15 20 26 31 35))
    => (5
        (11 15 31))"
  (let* ((intsec (delq
                  nil
                  (mapcar
                   (lambda (x) (car (member x (car lotto-nums))))
                   my-nums)))
         (ilen (length intsec)))
    (list
     (cond ((= ilen 6) 1)
           ((= ilen 5)
            (if (member (cadr lotto-nums) my-nums)
                (progn
                  (setq intsec (append intsec (list (cadr lotto-nums))))
                  2)
              3))
           ((> ilen 2)
            (- 8 ilen))
           (t 0))
     intsec)))


(defun lotto-check-numbers-list (gno my-num-list)
  "check the given list of the lotto numbers
GNO: game no.
MY-NUM-LIST: a list of numbers to check
return: result of check. ((grade (matched_numbers)) ...)

ex) (lotto-check-numbers-list 395 '((1 2 3 4 5 6) (11 15 20 28 32 36)))
    => ((0
         nil)
        (5
         (11 15 20)))"
  (let ((lotto-nums (lotto-retrieve-numbers gno))
        (my-list (if (consp (car my-num-list))
                     my-num-list
                   (list my-num-list)))
        (result ()))
    (dolist (nums my-list)
      (push (lotto-check-numbers lotto-nums nums)
            result))
    (reverse result)))


(defun lotto-retrieve-numbers-formatted (gno)
  "return a formatted string for the given game no.
GNO: game no.
return: a formatted lotto info string

ex) (lotto-retrieve-numbers-formatted 430)
    => \"Game 430: 1 3 16 18 30 34 and Bonus Number is 44.\""
  (let ((nums (lotto-retrieve-numbers gno)))
    (if nums
        (format "Game %d: %s and Bonus Number is %d."
                gno
                (substring
                 (format "%s" (car nums))
                 1 -1)
                (cadr nums))
      (format "Game %d: not exist yet."
              gno))))


(defun lotto-create-msg-window ()
  "create a window for the `*lotto-check-messages*' buffer"
  (let ((buf (get-buffer-create +lotto-message-buffer+))
        (win (split-window-vertically)))
    (other-window 1)
    (set-window-text-height nil +lotto-msg-window-height+)
    (switch-to-buffer buf)
    (lotto-mode)
    win))


(defun lotto-get-or-create-lotto-msg-window (&optional do-not-create)
  "find the window for the`*lotto-check-messages*' buffer
or create a window for the buffer
DO-NOT-CREATE: If its value is non NIL, this function does not create a new window.
return: the exist window or a new window (DO-NOT-CREATE is NIL)
        NIL or a new window (DO-NOT-CREATE is non NIL)"
  (let ((wlist (window-list))
        (lwin nil))
    (dolist (w wlist)
      (when (string= +lotto-message-buffer+
             (buffer-name (window-buffer w)))
        (setq lwin w)
        (return)))
    (if (and (not do-not-create) (null lwin))
        (lotto-create-msg-window)
      lwin)))


(defun lotto-message (msg &optional to-buf)
  "display the given message
MSG: a message to display
TO-BUF: whether to display the message on the `*lotto-check-messages*' buffer

* If the value of either `TO-BUF' or `lotto-use-buffer-for-message' is non NIL,
  the contents of `MSG' will be displayed on the `*lotto-check-messages*' buffer."
  (if (or to-buf lotto-use-buffer-for-message)
      (let ((win (lotto-get-or-create-lotto-msg-window)))
        (set-buffer (window-buffer win))
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (insert msg)
        (insert "\n")
        (goto-char (point-max))
        (setq buffer-read-only t))
    (message msg)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lotto-retrieve-numbers-i (gno)
  "retrieve lotto numbers and then
show it on the `*lotto-check-messages*' buffer
        or the `*Messages*' buffer"
  (interactive "ngame no: ")
  (lotto-message (lotto-retrieve-numbers-formatted gno)))


(defun lotto-check-numbers-list-i (gno my-num-list)
  "check the given list of the lotto numbers and then
show it on the `*lotto-check-messages*' buffer
        or the `*Messages*' buffer"
  (interactive "ngame no: \nxyour numbers: ")
  (let ((results (lotto-check-numbers-list gno my-num-list))
        (msgs nil))
    (push "---------------------------------------------------------------------------" msgs)
    (push (lotto-retrieve-numbers-formatted gno) msgs)
    (do ((cnt 1 (1+ cnt))
         (lst results (cdr lst))
         (mylst (if (consp (car my-num-list))
                    my-num-list
                  (list my-num-list))
                (cdr mylst)))
        ((or (null lst) (null mylst)))
      (push (format "Try #%d %-19s => Grade: %s, Matched Numbers: %s"
                    cnt
                    (car mylst)
                    (if (= (caar lst) 0)
                        "-"
                      (int-to-string (caar lst)))
                    (if (null (cadar lst))
                        "None"
                      (substring
                       (format "%s" (cadar lst))
                       1 -1)))
            msgs))
    (push "---------------------------------------------------------------------------" msgs)
    (lotto-message (mapconcat 'identity (reverse msgs) "\n"))))


(defun lotto-save-db-to-file-i ()
  "save the contents of `*lotto-database*' to the local file(`lotto-database-file')"
  (interactive)
  (if (lotto-save-db-to-file)
      (lotto-message (format "OK: Lotto DB has been successfully saved. (%s)" lotto-database-file))
    (lotto-message "ERROR: Saving Lotto DB failed!!")))


(defun lotto-load-db-from-file-i ()
  "load the contents of `*lotto-database*' from the local file(`lotto-database-file')"
  (interactive)
  (if (lotto-load-db-from-file)
      (lotto-message (format "OK: Lotto DB has been successfully loaded. (%s)" lotto-database-file))
    (lotto-message "ERROR: Loading Lotto DB failed!!")))


(defun lotto-hide-message-buffer ()
  "hide the window for the `*lotto-check-messages*' buffer"
  (interactive)
  (let ((win (lotto-get-or-create-lotto-msg-window t)))
    (print win)
    (if win
        (delete-window win))))


(defun http-retrieve-page-contents (url)
  "retrieve data from the given URL"
  (interactive "surl: ")
  (save-excursion
    (let ((buf1 (url-retrieve-synchronously url)))
      (set-buffer (get-buffer-create +http-retrieved-page-contents-buffer+))
      (erase-buffer)
      (insert-buffer-substring buf1)
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (decode-coding-region (point-min) (point-max) (intern (downcase (match-string 1))))
      (kill-buffer buf1)
      (switch-to-buffer +http-retrieved-page-contents-buffer+))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (eval load)
  (unless *lotto-database*
    (lotto-load-db-from-file)))


(add-hook 'kill-emacs-hook 'lotto-save-db-to-file)


(provide 'lotto-check)
