
Remove types from naming conventions.

Switch syntax of the -> ? i.e. string->symbol

A type prefix is only specified if it is absolutely necessary to the workings of the message,
or if the message will be ambiguious in it's function.
Example: mutation. set! will be ambiguous when used with a data type. Does it set the value or a value within the type?
Example: alists. Does delete delete the values from the list or the alist, because an alist is technically a list. (this limitation is only because an alist is not a proper object)

Do we make an alist it's own object?
Redefine alist?

;;;; Scheme ;;;;

list-tail
list-ref
list=
list-of?
list-copy
list-index
list-tabulate


char<?
char>?
char=?
char<=?
char>=?
char-upcase
char-downcase
char-alphabetic?
char-numeric?
char-whitespace?
char-upper-case?
char-lower-case?


string<?
string>?
string=?
string<=?
string>=?
string-ref
string-copy
string-append
string-length


vector-ref
vector-length
vector-resize


alist-ref
alist-cons
alist-copy
alist-update
alist-delete


;;;; Chicken Base ;;;;



;;;; Rebindings ;;;;

(define tail list-tail)


(define ref list-ref)

alist-ref
list-ref
string-ref
vector-ref

;;;; Extra ;;;;

(define (alist? x)
  (and (list? x)
     (null? (filter not (map pair? x)))))
