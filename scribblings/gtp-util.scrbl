#lang scribble/manual

@require[
  scribble/example
  (for-label
    file/glob
    gtp-util/system
    gtp-util
    racket/base
    racket/contract
    racket/path
    (only-in racket/math natural?)
    (only-in openssl/md5 md5))]

@(define (make-gtp-util-eval) (make-base-eval '(require gtp-util)))

@; -----------------------------------------------------------------------------
@title[#:tag "top"]{GTP utilities}

@defmodule[gtp-util]{
  General helper functions.
}

If you think one of these functions should "graduate" to another library
 or package, let me know at: @hyperlink["https://github.com/bennn/gtp-util/issues"]{github.com/bennn/gtp-util/issues}

@defproc[(filename/c [x any/c]) boolean?]{
  Flat contract for a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{path or string}
   that @racket[path-only] returns
   @racket[#false] for.

  @examples[#:eval (make-gtp-util-eval)
    (filename/c "segfault.mp3")
    (filename/c (build-path "code.rkt"))
    (filename/c (build-path "Desktop" "passwd.txt"))
  ]
}

@defproc[(nonnegative-real/c [x any/c]) boolean?]{
  Flat contract for non-negative real numbers.

  @examples[#:eval (make-gtp-util-eval)
    (nonnegative-real/c 0)
    (nonnegative-real/c (sqrt 2))
    (nonnegative-real/c -2)
  ]
}

@defproc[(digit/c [x any/c]) boolean?]{
  Flat contract for exact integers bewteen 0 and 9, inclusive.

  @examples[#:eval (make-gtp-util-eval)
    (digit/c 0)
    (digit/c 5)
    (digit/c -1)
    (digit/c 1.1)
    (digit/c 10)
  ]
}

@defproc[(unique-listof/c [c contract?]) list-contract?]{
  Similar to @racket[listof], but rejects lists that contain two or more @racket[equal?] elements.

  @examples[#:eval (make-gtp-util-eval)
    ((unique-listof/c symbol?) '(u n i q))
    ((unique-listof/c symbol?) '(r e p e a t))
  ]
}

@defproc[(confidence-interval [r* (listof real?)] [#:cv confidence-value nonnegative-real/c 1.96]) (cons/c real? real?)]{
  Return a 95% confidence interval for the given numbers at the given confidence value.
}

@defproc[(tab-split [str string?]) (listof string?)]{
  Split a string by tab characters.
}

@defproc[(tab-join [str* (listof string?)]) string?]{
  Join a list of strings by tab characters.
}

@defproc[(path-string->string [ps path-string?]) string?]{
  Convert a path or string to a string.
}

@defproc[(path-string->path [ps path-string?]) path?]{
  Convert a path or string to a path.
}

@defproc[(ensure-directory [ps path-string?]) void?]{
  If the given directory exists, do nothing.
  Otherwise, create it.
}

@defproc[(rnd [r real?]) string?]{
  Round the given number to two decimal places.
}

@defproc[(pct [a real?] [b real?]) real?]{
  Same as @racket[(* 100 (/ a b))].
}

@defproc[(log2 [n natural?]) natural?]{
  Compute the base-2 logarithm of a number.

  Assumes @racket[n] is a power of 2.
}

@defproc[(order-of-magnitude [n real?]) natural?]{
  Count the number of digits in the given number.
}

@defproc[(file-remove-extension [ps path-string?]) path-string?]{
  Remove the extension from the given filename.
}

@defproc[(save-pict [out-path path-string?] [p pict?]) boolean?]{
  Write the given pict to the given filename in @tt{.png} format.
}

@defproc[(columnize [x* list?] [num-cols natural?]) (listof list?)]{
  Divide a list into almost-equally-sized lists.
}

@defproc[(force/cpu-time [thunk (-> any)]) (values any/c natural?)]{
  Force the given thunk and record its running time.
  Return both the result of the thunk and the CPU time (as reported by @racket[time-apply]).
}

@defproc[(bitstring? [x any/c]) boolean?]{
  Predicate for a string of @racket[#\1] and @racket[#\0] characters.

  @examples[#:eval (make-gtp-util-eval)
    (bitstring? "0011")
    (bitstring? 3)
    (bitstring? " 1")
  ]
}

@defproc[(natural->bitstring [n natural?] [#:bits k natural?]) string?]{
  Return a binary representation of @racket[n] using exactly @racket[k] bits.
}

@defproc[(bitstring->natural [str string?]) natural?]{
  Parse a string of @racket{1} and @racket{0} digits as a binary number,
   return the base-10 representation of the parsed number.
}

@defproc[(count-zero-bits [str string?]) natural?]{
  Count the number of @racket[#\0] characters in a string of @racket{1} and
   @racket{0} digits.
}

@defproc[(copy-file* [src directory-exists?] [dst directory-exists?] [pattern string? "*"]) void?]{
  Copy every file from the directory @racket[src] whose name matches the given @racket[glob] pattern into the directory @racket[dst].
}

@defproc[(copy-racket-file* [src directory-exists?] [dst directory-exists?]) void?]{
  Same as @racket[(copy-file* src dst "*.rkt")].
}

@defproc[(enumerate [x* (listof any/c)]) (listof (cons/c natural? any/c))]{
  Given a list of values @racket['(A B C)] return a list with each value indexed by its position @racket['((0 . A) (1 . B) (2 . C))].

  See also @racket[in-indexed] and @racket[in-naturals].
}

@defproc[(integer->digit* [i exact-integer?]) (listof digit/c)]{
  Explode an integer to a list of its digits.

  @examples[#:eval (make-gtp-util-eval)
    (integer->digit* 8675309)
    (integer->digit* 0)
  ]
}

@defproc[(digit*->integer [d* (listof digit/c)]) exact-integer?]{
  Concatenate a sequence of digits into an integer.

  @examples[#:eval (make-gtp-util-eval)
    (digit*->integer '(9 1 1))
  ]
}

@defproc[(filename-sort [ps* (listof filename/c)]) (listof filename/c)]{
  Sort a list of filenames.

  @examples[#:eval (make-gtp-util-eval)
    (filename-sort (list (build-path "foo.md") (build-path "bar.rkt")))
  ]
}


@; -----------------------------------------------------------------------------

@section{System Calls}
@defmodule[gtp-util/system]{
  Convenience API for making system calls.
}

See also @racketmodname[racket/system].

@defproc[(shell [cmd path-string?] [arg* (or/c path-string? (listof path-string?))] ...) string?]{
  Finds the executable that @racket[cmd] denotes, then invokes it with the given arguments.
  Returns a string containing all output from the system call.
  Raises an @racket[exn:fail:user?] exception if the executable exits uncleanly.
}

@defproc[(md5sum [filename path-string?]) string?]{
  Same as @racket[(call-with-input-file filename md5)].
}
