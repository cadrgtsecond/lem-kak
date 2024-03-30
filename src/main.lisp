(defpackage #:lem-kak
  (:use #:cl #:lem)
  (:export #:kak-mode))
(in-package #:lem-kak)

;;; Normal mode
(define-global-mode normal-mode ()
  (:name "Kakoune normal mode"
   :keymap *normal-mode-keymap*))
     
(define-keys *normal-mode-keymap*
  ("h" 'kak-backward-char)
  ("l" 'kak-forward-char)
  ("H" 'kak-backward-char-ext)
  ("L" 'kak-forward-char-ext)
  ("j" 'kak-next-line)
  ("k" 'kak-previous-line)
  ("J" 'kak-next-line-ext)
  ("K" 'kak-previous-line-ext)
  ("d" 'delete-previous-char)
  ("c" 'change-previous-char)
  (":" 'kak-execute-command))
 
(defun extend-cursor (cur extend-p)
  (if extend-p
      (unless (mark-active-p (cursor-mark cur))
        (set-cursor-mark cur cur))
      ; When not extending, we should cancel the selection
      (setf (mark-active-p (cursor-mark cur)) nil)))

(defun offset-all-cursors (n extend-p)
  (dolist (cur (buffer-cursors (current-buffer)))
    (extend-cursor cur extend-p)
    (character-offset cur n)))

(defun next-line-all-cursors (n extend-p)
  (dolist (cur (buffer-cursors (current-buffer)))
    (extend-cursor cur extend-p)
    (next-line n)))

(define-command kak-forward-char (&optional (n 1)) ("p")
  (offset-all-cursors n nil))
(define-command kak-backward-char (&optional (n 1)) ("p")
  (offset-all-cursors (- n) nil))
(define-command kak-forward-char-ext (&optional (n 1)) ("p")
  (offset-all-cursors n t))
(define-command kak-backward-char-ext (&optional (n 1)) ("p")
  (offset-all-cursors (- n) t))

(define-command kak-next-line (&optional (n 1)) ("p")
  (next-line-all-cursors n nil))
(define-command kak-previous-line (&optional (n 1)) ("p")
  (next-line-all-cursors (- n) nil))
(define-command kak-next-line-ext (&optional (n 1)) ("p")
  (next-line-all-cursors n t))
(define-command kak-previous-line-ext (&optional (n 1)) ("p")
  (next-line-all-cursors (- n) t))

;; Other modes
(define-key *normal-mode-keymap* "i" 'insert-mode)

;;; Insert mode
(define-global-mode insert-mode ()
  (:name "Kakoune insert mode"
   :keymap *insert-mode-keymap*))
(setf (keymap-undef-hook *insert-mode-keymap*) 'self-insert)

(define-keys *insert-mode-keymap*
  ("Return" 'newline)
  ("Left" 'backward-char)
  ("Right" 'forward-char)
  ("Backspace" 'delete-previous-char)
  ("Delete" 'delete-next-char))

(define-key *insert-mode-keymap* "Escape" 'kak-escape)

;;; Commands
(define-command kak-execute-command (arg) ("P")
  (insert-mode)
  (execute-command arg))
(define-command kak-escape () ()
  (normal-mode)
  (escape))