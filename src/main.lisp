(defpackage #:lem-kak
  (:use #:cl #:lem)
  (:export #:kak-mode))
(in-package #:lem-kak)

;;; Normal mode
(define-global-mode normal-mode ()
    (:name "Kakoune normal mode"
     :keymap *normal-mode-keymap*))
     
(define-key *normal-mode-keymap* "h" 'backward-char)
(define-key *normal-mode-keymap* "l" 'forward-char)
(define-key *normal-mode-keymap* "j" 'next-line)
(define-key *normal-mode-keymap* "k" 'previous-line)
(define-key *normal-mode-keymap* "d" 'delete-previous-char)
(define-key *normal-mode-keymap* "c" 'kak-change-previous-char)
(define-key *normal-mode-keymap* ":" 'kak-execute-command)
 
(define-command kak-change-previous-char () ()
  (delete-previous-char)
  (insert-mode))

;; Other modes
(define-key *normal-mode-keymap* "i" 'insert-mode)

;;; Insert mode
(define-global-mode insert-mode ()
    (:name "Kakoune insert mode"
     :keymap *insert-mode-keymap*))
(setf (keymap-undef-hook *insert-mode-keymap*) 'self-insert)

(define-key *insert-mode-keymap* "Return" 'newline)
(define-key *insert-mode-keymap* "Left" 'backward-char)
(define-key *insert-mode-keymap* "Right" 'forward-char)
(define-key *insert-mode-keymap* "Backspace" 'delete-previous-char)
(define-key *insert-mode-keymap* "Delete" 'delete-next-char)

(define-key *insert-mode-keymap* "Escape" 'kak-escape)

;;; Commands
(define-command kak-execute-command (arg) ("P")
  (insert-mode)
  (execute-command arg))
(define-command kak-escape () ()
  (normal-mode)
  (escape))