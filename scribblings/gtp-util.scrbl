#lang scribble/manual

@require[
  scribble/example
  (for-label
    file/glob
    gtp-util/system
    gtp-util
    racket/base
    racket/contract
    (only-in racket/math natural?)
    (only-in openssl/md5 md5))]

@(define (make-gtp-util-eval) (make-base-eval '(require gtp-util)))

@; -----------------------------------------------------------------------------
@title[#:tag "top"]{GTP utilities}

General helper functions.

@defmodule[gtp-util]

@defproc[(nonnegative-real/c [x any/c]) boolean?]{
  Flat contract for non-negative real numbers.

  @examples[#:eval (make-gtp-util-eval)
    (nonnegative-real/c 0)
    (nonnegative-real/c (sqrt 2))
    (nonnegative-real/c -2)
  ]
}

@defproc[(unique-listof/c [c contract?]) list-contract?]{
  Similar to @racket[listof], but rejects lists that contain two or more @racket[equal?] elements.

  @examples[#:eval (make-gtp-util-eval)
    ((unique-listof/c symbol?) '(u n i q))
    ((unique-listof/c symbol?) '(r e p e a t))]
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
