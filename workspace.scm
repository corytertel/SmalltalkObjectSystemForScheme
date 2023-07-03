
(import r7rs)
(import srfi-1)

(define (animal? animal)
  (and (pair? animal) (if (memq '*animal* (car animal)) #t #f)))

How to deal with method naming conflicts? TODO
Need to make properties prefixed with "property-" in order to deal with naming conficts.
Need to make methods prefixed with "method-" in order to deal with naming conficts.
FIXME in the future.

(define (make-animal property-name property-noise)
  (define (method-name)
    property-name)
  (define (method-noise)
    property-noise)
  (define (method-+ . animals)
    (make-animal
     (apply string-append (cons property-name (map name animals)))
     (apply string-append (cons property-noise (map noise animals)))))
  (cons '(*animal*)
	(lambda (message . args)
	  (cond ((eq? message 'name) (apply method-name args))
		((eq? message 'noise) (apply method-noise args))
		((eq? message '+) (apply method-+ args))
		(else (error "Message not understood" message))))))

(define (get-name animal)
  ((cdr animal) 'name))

(define (get-noise animal)
  ((cdr animal) 'noise))

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
(name dog)
(animal? dog)
(animal? dog2)
(name (+ dog dog2))
(noise (+ dog dog2 dog3))
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
;; Issue? with inheritance: can't access variables of super class
(define (make-cat color)
  (let ((super (make-animal "Cat" "Meow")))
    (cons (cons '*cat* (car super))
	  (lambda (message . args)
	    (cond ((eq? message 'color) color)
		  ((eq? message 'name) (string-append "I am a Cat"))
		  (else (apply (cdr super) (cons message args))))))))

(define (cat? x)
  (and (pair? x) (if (memq '*cat* (car x)) #t #f)))

(define (cat-color x)
  ((cdr x) 'color))

(define cat (make-cat "Tabby"))
cat
(cat-color cat)
(animal-name (+ dog cat))


Guidelines:
1. Don't name internal methods the same as global "messages". FIXME


Classes are defined in terms of their constructor.

(define (make-CLASSNAME property1 property2)
  *INTERNAL METHODS HERE*
  (cons '(*CLASSNAME*)
	(lambda (message . args)
	  (cond *MESSAGE DISPATCHING HERE*
		((eq? message 'property1) property1)
		((eq? message 'property2) property2)
		((eq? message '+) (+ args))
		(else (make-error message))))))

Functions wrapping around message handling for easier and more consistant use here.
Keeps the "algorithmic" part of scheme around.

(define (CLASSNAME-name self)
  ((cdr self) 'name))

(define (CLASSNAME-noise self)
  ((cdr self) 'noise))

Predicate here.

(define (CLASSNAME? self)
  (and (pair? self) (if (memq '*CLASSNAME* (car self)) #t #f)))

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

Errors are defined with their own special class.

(define (make-error m)
  (cons '(*error*)
	(lambda (message . args)
	  (cond *MESSAGE DISPATCHING HERE*
		((eq? message 'property1) property1)
		((eq? message 'property2) property2)
		((eq? message '+) (+ args))
		(else (error "does-not-understand" message))))))


How the macro works

(define-class CLASSNAME SUPERCLASS CONSTRUCTOR-ARGLIST
  METHODS)






Alist

alist?
alist-ref
alist-cons
alist-copy
alist-update
alist-update!
alist-delete
alist-delete!

(delete 5 '(1 2 3 4 5 1 2 3 4 5) =)

(alist-delete 'a '((a . 1) (b . 2) (c . 3)))


(define (alist? x)
  (and (pair? x) (if (memq '*alist* (car x)) #t #f)))

(define (make-alist data)
  (define (ref key)
    (alist-ref key data))
  ;; (define (cons key value)
  ;;   (make-alist (alist-cons key value data)))
  (define (copy)
    (make-alist (alist-copy data)))
  (define (update key value)
    (make-alist (alist-update key value data)))
  (define (update! key value)
    (alist-update! key value data))
  (define (delete key)
    (make-alist (alist-delete key data)))
  (define (delete! key)
    (alist-delete! key data))
  (cons '(*alist*)
	(lambda (message . args)
	  (cond ((eq? message 'ref) (apply ref args))
		((eq? message 'cons) (apply cons args))
		((eq? message 'copy) (apply copy args))
		((eq? message 'update) (apply update args))
		((eq? message 'update!) (apply update! args))
		((eq? message 'delete) (apply delete args))
		((eq? message 'delete!) (apply delete! args))
		(else (error "does-not-understand" message))))))

Right now it's up to the user to determine whether or not the symbol is bound.

(define (ref x key)
  ((cdr x) 'ref key))

;; Broken
(define cons
  (let ((old-cons cons))
    (lambda args
      (if (alist? (car args))
	  (apply (cdar args) `(cons ,@(cdr args)))
	  (apply old-cons args)))))

(define delete
  (let ((old-delete delete))
    (lambda args
      (if (alist? (car args))
	  (apply (cdar args) `(delete ,@(cdr args)))
	  (apply old-delete args)))))

(define a (make-alist '((a . 1 ) (b . 2) (c . 3))))
(ref a 'a)
(delete a 'a)






In order to make symbol lookup work:

(import apropos)
(import srfi-13)

;; TODO Make more efficient with both apropos lookup and substring operation
(define (defined? symbol)
  (if (null? (filter (lambda (x) (eq? x symbol))
		     (map (lambda (x) (let* ((s (symbol->string x))
					(y (string-index s #\#)))
				   (if y (string->symbol (substring s (+ 1 y))) x)))
			  (apropos-list symbol))))
      #f
      #t))

(defined? 'alist-ref)
(defined? 'test)




(string-trim "")

(string-trim)
