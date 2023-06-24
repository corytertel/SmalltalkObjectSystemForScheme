
(import r7rs)
(import srfi-1)

(define (make-animal name noise)
  (define (+ animals)
    (make-animal
     (apply string-append (cons name (map animal-name animals)))
     (apply string-append (cons noise (map animal-noise animals)))))
  (cons '(*animal*)
	(lambda (message . args)
	  (cond ((eq? message 'name) name)
		((eq? message 'noise) noise)
		((eq? message '+) (+ args))
		(else (error "Object does not know how to deal with message." message))))))

(define (animal-name animal)
  ((cdr animal) 'name))

(define (animal-noise animal)
  ((cdr animal) 'noise))

(define (animal? animal)
  (and (pair? animal) (if (memq '*animal* (car animal)) #t #f)))

(define +
  (let ((old+ +))
    (lambda args
      (if (animal? (car args))
	  (apply (cdar args) (cons '+ (cdr args)))
	  (apply old+ args)))))

(define dog (make-animal "Dog" "Bark"))
(define dog2 (make-animal "Dog" "Bark"))
(define dog3 (make-animal "Dog" "Bark"))
((cdr dog) 'name)
(animal-name dog)
(animal? dog)
(animal? dog2)
(animal-name (+ dog dog2))
(animal-noise (+ dog dog2 dog3))
(+ 1 1 2 4)


(define +
  (let ((old+ +))
    (lambda args
      (if (string? (car args))
          (apply string-append args)
          (apply old+ args)))))

(define +
  (let ((old+ +))
    (lambda args
      (if (vector? (car args))
          (apply vector-append args)
          (apply old+ args)))))

(+ 20 83)
(+ dog dog2)
(+ "Hello " "World!")
(+ (vector 1 2 3) (vector 4 5 6))

;; Inheritance
(define (make-cat color)
  (let ((super (make-animal "Cat" "Meow")))
    (cons (cons '*cat* (car super))
	  (lambda (message . args)
	    (cond ((eq? message 'color) color)
		  ((eq? message 'name) (string-append "I am a Cat"))
		  (else (apply (cdr super) (cons message args))))))))

(define (cat-color x)
  ((cdr x) 'color))

(define cat (make-cat "Tabby"))
cat
(cat-color cat)
(animal-name (+ dog cat))


Guidelines:
1. Don't name internal methods the same as global "messages".


Classes are defined in terms of their constructor.

(define (make-CLASSNAME property1 property2)
  *INTERNAL METHODS HERE*
  (cons '(*CLASSNAME*)
	(lambda (message . args)
	  (cond *MESSAGE DISPATCHING HERE*
		((eq? message 'property1) property1)
		((eq? message 'property2) property2)
		((eq? message '+) (+ args))
		(else (error "Object does not know how to deal with message." message))))))

Functions wrapping around message handling for easier and more consistant use here.
Keeps the "algorithmic" part of scheme around.

(define (CLASSNAME-name x)
  ((cdr x) 'name))

(define (CLASSNAME-noise x)
  ((cdr x) 'noise))

Predicate here.

(define (CLASSNAME? x)
  (and (pair? x) (if (memq '*CLASSNAME* (car x)) #t #f)))

Functions that need to be dispatched are dispatched properly.

(define +
  (let ((old+ +))
    (lambda args
      (if (CLASSNAME? (car args))
	  (apply (cdar args) (cons '+ (cdr args)))
	  (apply old+ args)))))

Certain functions cannot be dispatched like `display`.

Inheritance

(define (make-SUBCLASS property3)
  (let ((super SUPERCLASS-CONSTRUCTOR))
    (cons (cons '*SUBCLASS* (car super))
	  (lambda (message . args)
	    (cond ((eq? message 'property3) property3)
		  *METHOD OVERRIDING HERE*
		  (else (apply (cdr super) (cons message args))))))))
