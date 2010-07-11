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

;; Version 0.1
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
;; * API functions
;;   - lotto-retrieve-numbers   : retrieve a specific lotto info
;;   - lotto-check-numbers-list : check lotto numbers
;;   - save-lotto-db-to-file    : save lotto database to the local file(`lotto-database-file')
;;   - load-lotto-db-from-file  : load lotto database from the local file(`lotto-database-file')


(eval-when-compile (require 'cl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup lotto nil
  "Simple API for Korean Lotto 645"
  :prefix "lotto"
  :version "22.0"
  :group 'applications)


(defcustom lotto-info-retrieve-func-custom nil
  "a custom function to retrieve lotto info\nTo enable this variable, you must set `lotto-info-retrieve-func' to `lotto-info-retrieve-func-custom'."
  :type 'function
  :group 'lotto)


(defcustom lotto-info-retrieve-func 'lotto-retrieve-numbers-from-lotto-k
  "a function to retrieve lotto info"
  :type 'function
  :options '(lotto-retrieve-numbers-from-lotto-k
             lotto-retrieve-numbers-from-naver
             lotto-retrieve-numbers-from-daum
             lotto-retrieve-numbers-from-645lotto
             lotto-info-retrieve-func-custom)
  :group 'lotto)


(defcustom lotto-database-file "~/.lotto-database"
  "a file to store lotto database"
  :type 'file
  :group 'lotto)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *lotto-database* nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun save-lotto-db-to-file ()
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


(defun load-lotto-db-from-file ()
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
  "return url for Lotto_K's lotto info site\nex) (lotto-gen-site-url-lotto-k 101)"
  (format "http://lotto.kaisyu.com/api?method=get&type=emacs&gno=%d"
          (if gno gno 0)))


(defun lotto-retrieve-numbers-from-lotto-k (gno)
  "retrieve lotto numbers from Lotto_K\nGNO: game no.\nreturn: ((num_list) bonus_num)\n\nex) (lotto-retrieve-numbers-from-lotto-k 395)\n=> ((11 15 20 26 31 35) 7)"
  (let ((buf1 (url-retrieve-synchronously
               (lotto-gen-site-url-lotto-k gno)))
        (obj nil))
    (set-buffer buf1)
    (goto-char (point-min))
    (unwind-protect
        (progn
          (re-search-forward "^(.+$")
          (setq obj (read-from-whole-string (match-string 0))))
      (kill-buffer buf1))
    (list (cdr (assoc 'nums obj)) (cdr (assoc 'bnum obj)))))


(defun lotto-retrieve-numbers-base (gno url-func rexp)
  "retrieve lotto numbers\nGNO: game no.\nURL-FUNC: a function for lotto info url\nREXP: number string pattern\nreturn: ((num_list) bonus_num)\n\nex) (lotto-retrieve-numbers-base 395 lotto-gen-site-url-naver \"ball\\\\([0-9]+\\\\).gif\")\n=> ((11 15 20 26 31 35) 7)"
  (let ((buf1 (url-retrieve-synchronously
               (funcall url-func gno)))
        (nums ())
        (bnum))
    (set-buffer buf1)
    (goto-char (point-min))
    (unwind-protect
        (dotimes (i 7)
          (re-search-forward rexp)
          (push (string-to-number (match-string 1)) nums))
      (kill-buffer buf1))
    (setq bnum (pop nums))
    (list (reverse nums) bnum)))


(defun lotto-gen-site-url-daum (gno)
  "return url for Daum's lotto info site\nex) (lotto-gen-site-url-daum 101)"
  (if gno
      (format "http://lotto8.daum.net/winInfo/last_jackpot.asp?TIMES=%d" gno)
    "http://lotto8.daum.net/wininfo/last_jackpot.asp"))


(defun lotto-retrieve-numbers-from-daum (gno)
  "retrieve lotto numbers from Daum's lotto8\nGNO: game no.\nreturn: ((num_list) bonus_num)\n\nex) (lotto-retrieve-numbers-from-daum 395)\n=> ((11 15 20 26 31 35) 7)"
  (lotto-retrieve-numbers-base gno 'lotto-gen-site-url-daum "ball\\([0-9]+\\).gif"))


(defun lotto-gen-site-url-naver (gno)
  "return url for Naver's lotto info site\nex) (lotto-gen-site-url-naver 101)"
  (if gno
      (format "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%d%%C8%%B8%%B7%%CE%%B6%%C7" gno)
    "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%B7%CE%B6%C7"))


(defun lotto-retrieve-numbers-from-naver (gno)
  "retrieve lotto numbers from Naver\nGNO: game no.\nreturn: ((num_list) bonus_num)\n\nex) (lotto-retrieve-numbers-from-naver 395)\n=> ((11 15 20 26 31 35) 7)"
  (lotto-retrieve-numbers-base gno 'lotto-gen-site-url-naver "ball\\([0-9]+\\).gif"))


(defun lotto-gen-site-url-645lotto (gno)
  "return url for 645lotto's lotto info site\nex) (lotto-gen-site-url-645lotto 101)"
  (if gno
      (format "http://www.645lotto.net/resultall/%d.asp" gno)
    "http://www.645lotto.net/resultall/dummy.asp"))


(defun lotto-retrieve-numbers-from-645lotto (gno)
  "retrieve lotto numbers from 645lotto.net\nGNO: game no.\nreturn: ((num_list) bonus_num)\n\nex) (lotto-retrieve-numbers-from-645lotto 395)\n=> ((11 15 20 26 31 35) 7)"
  (lotto-retrieve-numbers-base gno 'lotto-gen-site-url-645lotto "Ball[\\\t ]:[\\\t ]\\\"\\([0-9]+\\)\\\""))


(defun lotto-retrieve-numbers-from-local-db (gno)
  "retrieve lotto numbers from local db\nGNO: game no.\nreturn: lotto info. ((num_list) bonus_num) OR nil if the info does not exist on the local db.\nex) (lotto-retrieve-numbers-from-local-db 395)\n=> ((11 15 20 26 31 35) 7)"
  (when (hash-table-p *lotto-database*)
    (gethash gno *lotto-database*)))


(defun lotto-retrieve-numbers (gno)
  "retrieve lotto numbers\nGNO: game no.\nreturn: lotto info. ((num_list) bonus_num)\nex) (lotto-retrieve-numbers 395)\n=> ((11 15 20 26 31 35) 7)"
  (let ((lval (lotto-retrieve-numbers-from-local-db gno)))
    (or lval
        (puthash gno (funcall lotto-info-retrieve-func gno) *lotto-database*))))


(defun lotto-check-numbers (lotto-nums my-nums)
  ;; TODO add comments
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
  "check the given lotto numbers\nGNO: game no.\nMY-NUM-LIST: a list of numbers to check\nreturn: result of check. ((grade (matched_numbers)) ...)\n\nex) (lotto-check-numbers-list 395 '((1 2 3 4 5 6) (11 15 20 28 32 36)))\n=> ((0 nil) (5 (11 15 20)))"
  (let ((lotto-nums (lotto-retrieve-numbers gno))
        (my-list (if (consp (car my-num-list))
                     my-num-list
                   (list my-num-list)))
        (result ()))
    (dolist (nums my-list)
      (push (lotto-check-numbers lotto-nums nums)
            result))
    (reverse result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lotto-retrieve-numbers-i (gno)
  (interactive "ngame no: ")
  (print (lotto-retrieve-numbers gno)))


(defun lotto-check-numbers-list-i (gno my-num-list)
  (interactive "ngame no: \nxyour numbers: ")
  (print (lotto-check-numbers-list gno my-num-list)))


(defun http-retrieve-page-contents (url)
  (interactive "surl: ")
  (save-excursion
    (let ((buf1 (url-retrieve-synchronously url)))
      (set-buffer (get-buffer-create "*http-retrieved-page-contents*"))
      (erase-buffer)
      (insert-buffer-substring buf1)
      (kill-buffer buf1)
      (switch-to-buffer "*http-retrieved-page-contents*"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (eval load)
  (unless *lotto-database*
    (load-lotto-db-from-file)))


(add-hook 'kill-emacs-hook 'save-lotto-db-to-file)


(provide 'lotto-check)
