(defpackage #:lem-kak
  (:use #:cl #:lem)
  (:export #:kak-mode))
(in-package #:lem-kak)

(defun set-anchor ()
  "Sets the anchor to the current-point"
  (set-cursor-mark (current-point) (current-point)))

;;; Normal mode
(define-global-mode kak-normal-mode ()
  (:name "Kakoune normal mode"
   :keymap *normal-mode-keymap*
   :enable-hook #'set-anchor))

(define-global-mode kak-insert-mode (lem-core::emacs-mode)
  (:name "Kakoune insert mode"
   :keymap *insert-mode-keymap*))

(define-keys *normal-mode-keymap*
  (":" 'kak-execute-command)
  
  ;; Change modes
  ("i" 'kak-insert-mode)
  
  ;;; HJKL
  ("h" 'backward-char-clear)
  ("l" 'forward-char-clear)
  ("j" 'next-line-clear)
  ("k" 'previous-line-clear)
  ("H" 'backward-char-ext)
  ("J" 'next-line-ext)
  ("K" 'previous-line-ext)
  ("L" 'forward-char-ext)
  
  ("w" 'next-word-start)
  ("W" 'next-word-start-ext)
  ("e" 'next-word-end)
  ("E" 'next-word-end-ext)
  ("b" 'previous-word-start)
  ("B" 'previous-word-start)


  ("d" 'delete-selection)
  ("c" 'change-selection)
  ("u" 'undo)
  ("U" 'redo)
  ("Escape" 'escape))

(define-keys *insert-mode-keymap*
  ("Escape" 'kak-normal-mode))
 
(define-command kak-execute-command (arg) ("P")
  (kak-insert-mode)
  (execute-command arg))

;;;
;;;  HJKL
;;;
(define-command (forward-char-clear (:advice-classes movable-advice)) (n) ("p")
  (forward-char n)
  (set-anchor))
(define-command (forward-char-ext (:advice-classes movable-advice)) (n) ("p")
  (forward-char n))
(define-command (backward-char-clear (:advice-classes movable-advice)) (n) ("p")
  (backward-char n)
  (set-anchor))
(define-command (backward-char-ext (:advice-classes movable-advice)) (n) ("p")
  (backward-char n))
(define-command (next-line-clear (:advice-classes movable-advice)) (n) ("p")
  (next-line n)
  (set-anchor))
(define-command (next-line-ext (:advice-classes movable-advice)) (n) ("p")
  (next-line n))
(define-command (previous-line-clear (:advice-classes movable-advice)) (n) ("p")
  (previous-line n)
  (set-anchor))
(define-command (previous-line-ext (:advice-classes movable-advice)) (n) ("p")
  (previous-line n))

;;;
;;; word and WORD commands
;;;
(defun move-till-boundary (boundary-p step extend-p)
  "Moves the current point till the specified boundary has been reached.
BOUNDARY-P is a function that accepts two characters and determines if they form a boundary"
  ;; Whenever we are already at a boundary, we should skip one character forward before doing the
  ;; move. This way, we can chain actions together smoothly.
  (when (funcall boundary-p (character-at (current-point)) (character-at (current-point) step))
    (character-offset (current-point) step))
  (unless extend-p
    (set-anchor))
  (loop for char1 = (character-at (current-point))
        for char2 = (character-at (current-point) step)
        while (and char1 char2)
        until (funcall boundary-p char1 char2)
        do (character-offset (current-point) step)))


(defun word-character-p (char)
  (or (alphanumericp char) (member char '(#\_ #\-))))

;; TODO: Handle special(non-word-character and non-blank, e.g $%*) characters
(defun word-start-p (char1 char2)
  (and (not (word-character-p char1)) (word-character-p char2)))

(defun word-end-p (char1 char2)
  (and (word-character-p char1) (not (word-character-p char2))))

;; TODO: Support n
(define-command (next-word-start (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-start-p 1 nil))
(define-command (next-word-start-ext (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-start-p 1 t))

(define-command (next-word-end (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-end-p 1 nil))
(define-command (next-word-end-ext (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-end-p 1 t))

(define-command (previous-word-start (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-start-p -1 nil))
(define-command (previous-word-start-ext (:advice-classes movable-advice)) (n) ("p")
  (move-till-boundary #'word-start-p -1 t))
