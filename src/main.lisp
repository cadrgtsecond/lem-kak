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
  ("M-h" 'move-to-line-start)
  ("M-l" 'move-to-line-end)
  ;; These are undocumented
  ("M-H" 'move-to-line-start-ext)
  ("M-L" 'move-to-line-end-ext)
  
  ("w" 'next-word-start)
  ("W" 'next-word-start-ext)
  ("e" 'next-word-end)
  ("E" 'next-word-end-ext)
  ("b" 'previous-word-start)
  ("B" 'previous-word-start-ext)

  ("f" 'move-forward-till-character)
  ("t" 'move-forward-upto-character)
  ("F" 'move-forward-till-character-ext)
  ("T" 'move-forward-upto-character-ext)

  ("M-f" 'move-backward-till-character)
  ("M-t" 'move-backward-upto-character)
  ("M-F" 'move-backward-till-character-ext)
  ("M-T" 'move-backward-upto-character-ext)

  ("x" 'select-line)

  ("o" 'open-line-below-and-insert)
  ("O" 'open-line-above-and-insert)
  ("M-o" 'open-line-below)
  ("M-O" 'open-line-above)

  ("d" 'delete-selection)
  ("c" 'change-selection)
  ("u" 'undo)
  ("U" 'redo)
  ("Escape" 'escape))

(define-keys *insert-mode-keymap*
  ("Escape" 'kak-normal-mode))
 
(define-command kak-execute-command (arg) ("P")
  (kak-insert-mode)
  (unwind-protect
    (execute-command arg)
    (when (typep (current-global-mode) 'kak-insert-mode)
      (kak-normal-mode))))

(defclass kakoune-advice () ())

(defmethod execute :around (mode (command kakoune-advice) arg)
  (do-each-cursors ()
    (call-next-method)))

;;
;; This is the magic trick that allows us to have block cursors
;;

;; TODO: Once lem-project/lem#1817 is merged, make use of MAKE-REGION-OVERLAYS-USING-GLOBAL-MODE

;; This function is supposed to error if there is no region
;;
;; That is why we use NIL in place of say, T. Actually any non-erroring expression will do
(defmethod check-marked-using-global-mode ((global-mode kak-normal-mode) buffer)
  (declare (ignore buffer global-mode))
  nil)

(defmethod region-beginning-using-global-mode ((global-mode kak-normal-mode)
                                               &optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (point-min (current-point) (mark-point (cursor-mark (current-point)))))

(defmethod region-end-using-global-mode ((global-mode kak-normal-mode)
                                         &optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (let ((result (copy-point (point-max (current-point) (mark-point (cursor-mark (current-point)))) :temporary)))
    (character-offset result 1)
    result))


;;;
;;;  HJKL
;;;
(define-command (forward-char-clear (:advice-classes kakoune-advice)) (n) ("p")
  (forward-char n)
  (set-anchor))
(define-command (forward-char-ext (:advice-classes kakoune-advice)) (n) ("p")
  (forward-char n))
(define-command (backward-char-clear (:advice-classes kakoune-advice)) (n) ("p")
  (backward-char n)
  (set-anchor))
(define-command (backward-char-ext (:advice-classes kakoune-advice)) (n) ("p")
  (backward-char n))
(define-command (next-line-clear (:advice-classes kakoune-advice)) (n) ("p")
  (next-line n)
  (set-anchor))
(define-command (next-line-ext (:advice-classes kakoune-advice)) (n) ("p")
  (next-line n))
(define-command (previous-line-clear (:advice-classes kakoune-advice)) (n) ("p")
  (previous-line n)
  (set-anchor))
(define-command (previous-line-ext (:advice-classes kakoune-advice)) (n) ("p")
  (previous-line n))

(defun matches-boundary (boundary-p point step)
  (alexandria:when-let
      ((char1 (character-at point))
       (char2 (character-at point step)))
    (funcall boundary-p char1 char2)))

;;;
;;; word and WORD commands
;;;
(defun move-till-boundary (boundary-p step)
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

(defun test ()
  (repeatn 10 (print "Hello")))

;; Whenever we are just one character off from a word, we should snap to it
(defun snap-to-word (point step)
  (when (matches-boundary (alexandria:disjoin #'word-start-p #'word-end-p) point step)
    (character-offset (current-point) step)))

;; TODO: Support n
;; TODO: Support strict words
(define-command (next-word-start (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) 1)
  (set-anchor)
  (move-till-boundary #'word-start-p 1))

(define-command (next-word-start-ext (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) 1)
  (move-till-boundary #'word-start-p 1))

(define-command (next-word-end (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) 1)
  (set-anchor)
  (move-till-boundary #'word-end-p 1))

(define-command (next-word-end-ext (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) 1)
  (move-till-boundary #'word-end-p 1))

(define-command (previous-word-start (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) -1)
  (set-anchor)
  (move-till-boundary #'word-start-p -1))

(define-command (previous-word-start-ext (:advice-classes kakoune-advice)) (n) ("p")
  (snap-to-word (current-point) -1)
  (move-till-boundary #'word-start-p -1))

;; Actions
(define-command (delete-selection (:advice-classes kakoune-advice)) (start end) ("r")
  (delete-between-points start end))
(define-command (change-selection (:advice-classes kakoune-advice)) (start end) ("r")
  (delete-between-points start end)
  (kak-insert-mode))

;; Selecting full lines
(define-command (select-line (:advice-classes kakoune-advice)) () ()
  (line-start (current-point))
  (set-anchor)
  (line-end (current-point)))

(define-command (open-line-below (:advice-classes kakoune-advice)) () ()
  (with-point ((p (current-point)))
    (line-end p)
    (insert-character p #\Newline)))

(define-command (open-line-above (:advice-classes kakoune-advice)) () ()
  (with-point ((p (current-point)))
    (line-start p)
    (insert-character p #\Newline)))

(define-command (open-line-below-and-insert (:advice-classes kakoune-advice)) () ()
  (line-end (current-point))
  (insert-character (current-point) #\Newline)
  (indent-line (current-point))
  (kak-insert-mode))

(define-command (open-line-above-and-insert (:advice-classes kakoune-advice)) () ()
  (line-start (current-point))
  (insert-character (current-point) #\Newline)
  (line-offset (current-point) -1)
  (indent-line (current-point))
  (kak-insert-mode))

(define-command (move-to-line-start (:advice-clasess kakoune-advice)) () ()
  (set-anchor)
  (line-start (current-point)))

(define-command (move-to-line-end (:advice-clasess kakoune-advice)) () ()
  (set-anchor)
  (line-end (current-point))
  (character-offset (current-point) -1))

(define-command (move-to-line-start-ext (:advice-clasess kakoune-advice)) () ()
  (line-start (current-point)))

(define-command (move-to-line-end-ext (:advice-clasess kakoune-advice)) () ()
  (line-end (current-point))
  (character-offset (current-point) -1))

;;; ftFT <a-f><a-t><a-F><a-T>

(defun move-upto-character (step char)
  (loop for curr = (character-at (current-point) step)
        until (or (null curr) (eql curr char))
        do (character-offset (current-point) step)))
(defun ask-character ()
  (let ((key (read-key)))
    (if (abort-key-p key)
        (error 'editor-abort)
        (key-to-char key))))

;; TODO: Support n
(define-command (move-forward-upto-character (:advice-classes kakoune-advice)) () ()
  (set-anchor)
  (move-upto-character 1 (ask-character)))

(define-command (move-forward-till-character (:advice-classes kakoune-advice)) () ()
  (move-forward-upto-character)
  (character-offset (current-point) 1))

(define-command (move-forward-upto-character-ext (:advice-classes kakoune-advice)) () ()
  (move-upto-character 1 (ask-character)))

(define-command (move-forward-till-character-ext (:advice-classes kakoune-advice)) () ()
  (move-forward-upto-character-ext)
  (character-offset (current-point) 1))

(define-command (move-backward-upto-character (:advice-classes kakoune-advice)) () ()
  (set-anchor)
  (move-upto-character -1 (ask-character)))

(define-command (move-backward-till-character (:advice-classes kakoune-advice)) () ()
  (move-forward-upto-character)
  (character-offset (current-point) -1))

(define-command (move-backward-upto-character-ext (:advice-classes kakoune-advice)) () ()
  (move-upto-character -1 (ask-character)))
(define-command (move-backward-till-character-ext (:advice-classes kakoune-advice)) () ()
  (move-backward-upto-character-ext)
  (character-offset (current-point) -1))
