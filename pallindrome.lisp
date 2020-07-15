;;;;;;;;;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
	 ,then-form
	 ,else-form)))

(defun string->listchar (string)
  (mapcar 'string
	  (coerce string
		  'list)))

(defun listchar->string (list)
  (reduce (lambda (x y) (concatenate 'string
				     x
				     y))
	  list))

(defun sanitize (word)

  (defun remove-spaces (string)
    (remove #\Space
	    string
	    :test 'char=))

  (defun return-delimitedp (word)
    (string= #\return
	     (car (last (string->listchar word)))))

  (let ((downcased-concatenated (remove-spaces (string-downcase word))))
    (if (return-delimitedp downcased-concatenated)
	(listchar->string (butlast (string->listchar downcased-concatenated)))
	downcased-concatenated)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change these files to suit the need

;;;;;;;;; LOADING THE FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *file-words-alpha* "words_alpha.txt")
(defvar *file-corncob-lowercase* "corncob_lowercase.txt")
(defvar *file-npdict* "npdict.txt")

(defun load-words-to-list (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect (cons (sanitize line) t))))

(defparameter *words* nil)
(when (null *words*)
  (setf *words* (load-words-to-list *file-corncob-lowercase*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; FUNCTIONS ON SEQUENCES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pallindrome-p (obj &key (test 'eq))

  (defun list-pallindrome-p (list &key (test 'eq))
    (let ((rlist (reverse list)))
      (not (eq 'not-pallindrome
	       (loop for element in list for relement in rlist
		     unless (funcall test element relement)
		       return 'not-pallindrome)))))

  (cond ( (consp obj)
	  (list-pallindrome-p obj
			      :test test))
	( (stringp obj)
	  (list-pallindrome-p (string->listchar obj)
			      :test test))))

(defun starts-with (word letters)
  (and (every (lambda (x y) (equal x y))
	      word
	      letters)
       (>= (length word)
	   (length letters))))

(defun ends-with (word letters)
  (starts-with (reverse word)
	       (reverse letters)))

(defun extract-substring (word start end)
  (when (and word
	     (< start end))
    (listchar->string (subseq (string->listchar word)
			      start
			      end))))

(defun restructure (list)
  ;; converts lists of form '(D C E B F A)
  ;; to lists of form '(A B C D E F)

  (defun every-nth (list n)
    (labels (( rec (list loop-k acc)
	       (if (null list)
		   acc
		   (rec (cdr list)
			(+ 1 loop-k)
			(if (zerop (mod loop-k n))
			    (cons (car list) acc)
			    acc)))))
      (rec list 0 nil)))

  (append
   (reverse (every-nth (reverse list) 2))
   (every-nth (cdr (reverse list)) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; PALLINDROME LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-remainder (seed remainder direction reversed)
  ;; computes the remainder of the pallindromic search
  ;; for example
  ;; (next-remainder "able" nil 'ends-with nil) -> "able"
  ;; (next-remainder "helba" "able" 'ends-with t) -> "h"
  ;; (next-remainder "hait" "h" 'starts-with nil) -> "ait"
  ;; (next-remainder "alsatia" "ait" 'ends-with t) -> "alsa"

  (defun beginning-substring (seed split-pt)
    (extract-substring seed 0 split-pt))

  (defun ending-substring (seed split-pt)
    (extract-substring seed split-pt (length seed)))

  (when remainder
    (let ((beginning (beginning-substring seed
					  (- (length seed)
					     (length remainder))))
	  (ending (ending-substring seed
				    (length remainder))))
      (case direction
	(ends-with (if reversed
		       beginning
		       (reverse ending)))
	(starts-with (if reversed
			 (reverse beginning)
			 ending))))))

(defun next-generator (&key (terms nil) (direction nil) (space nil))
  ;; creates a generator starting from a partial and
  ;; searching for words from space that are in the direction

  ;; for example
  ;; (next-generator :terms "able" :direction 'starts-with :space *words*
  ;; yields a generator creating words that start with "able...."
  ;;
  ;; (next-generator :terms "able" :direction 'ends-with :space *words*
  ;; yields a generator creating words that end in "....able"

  (defun find-first-match (words letters matchfn)
    ;; finds the first matching word from words satisfying matchfn on letters
    ;; the results are a values form. the first value is the solution
    ;; and the second value is the next search list

    ;; for example
    ;; (find-first-match *words* "ab" 'starts-with) ->
    ;;    ("aba" . T)
    ;;    ( ("abc" . T) ("abls" . T) ("abm" . T) ... ("zoophori" . T) )

    ;; (find-first-match *words* "ab" 'ends-with) ->
    ;;    ("achab" . T)
    ;;    ( ("achad" . T) ("achaea" . T) ("achaean" . T) ... ("zoophori" . T) )

    (cond ( (null words) nil)
	  ( (and (cdar words)
		 (funcall matchfn
			  (caar words)
			  letters))
	    (values (caar words)
		    (cdr words)))
	  (t (find-first-match (cdr words)
			       letters
			       matchfn))))

  (when terms
    (let ((letters terms)
	  (wordpairs space))
      (lambda () (multiple-value-bind (solution next)
		     (case direction
		       (ends-with (find-first-match wordpairs
						    letters
						    direction))
		       (starts-with (find-first-match wordpairs
						      letters
						      direction)))
		   (setf wordpairs next)
		   solution)))))

(defstruct drome
  word
  remainder
  direction
  reversed
  space)

(defun top (stack obj)
  (case obj
    (drome (caar stack))
    (fn (cdar stack))))

(defun flip-direction (drome)
  (case (drome-direction drome)
    (ends-with 'starts-with)
    (starts-with 'ends-with)))

(defun mark (word status space)
  (setf (cdr (assoc word space :test 'equal))
	(case status
	  (used nil)
	  (unused t))))

(defmacro refresh (stack acc)
  `(progn
    (pop ,stack)
    (pop ,acc)
    (create-pallindromes :stack ,stack :acc ,acc)))

(defun pallindrome (list)
  (pallindrome-p (apply 'concatenate
			'string
			list)
		 :test 'equal))

(defvar *solution* nil)
(defun create-pallindromes (&key (stack nil) (acc nil))

  (when (and (pallindrome (restructure acc))
	     (> (length acc) (length *solution*)))
    (setf *solution* (restructure acc))
    (print *solution*))

  (cond ( (null stack) acc )
	( (null (top stack 'fn))
	  (refresh stack acc))
	( t (let* ((word (funcall (top stack 'fn))))
	      (cond ((null word)
		     (refresh stack acc))
		    (t (let* ((remainder (next-remainder word
							 (drome-remainder (top stack 'drome))
							 (flip-direction (top stack 'drome))
							 (drome-reversed (top stack 'drome))))
			      (drome (make-drome :word word
						 :remainder remainder
						 :direction (flip-direction (top stack 'drome))
						 :reversed (eq (drome-direction (top stack 'drome))
							       'starts-with)
						 :space (drome-space (top stack 'drome))))
			      (generator (prog2
					     (mark word 'used (drome-space drome))
					     (next-generator :terms remainder
							     :direction (drome-direction drome)
							     :space (drome-space drome)))))
			 (push (cons drome generator) stack)
			 (create-pallindromes :stack stack :acc (cons word acc)))))))))

(defun create-pallindromes-starter (&key (seed nil) (space *words*))
  (setf *solution* nil)

  (let* ((drome (make-drome :word seed
			    :remainder seed
			    :direction 'ends-with
			    :reversed t
			    :space space))
	(generator (next-generator :terms (reverse (drome-remainder drome))
				   :direction 'ends-with
				   :space space)))
    (create-pallindromes :stack (list (cons drome generator)) :acc (list seed))))
