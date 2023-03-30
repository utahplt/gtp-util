#lang racket/base

;; Misc. helper functions

(require racket/contract)
(provide

  filename/c

  nonnegative-real/c

  digit/c

  unique-listof/c

  no-duplicates?

  (contract-out
    [confidence-interval
     (->* [(listof nonnegative-real/c)] [#:cv nonnegative-real/c] (cons/c real? real?))]

    [confidence-offset
     (->* [(listof nonnegative-real/c)] [#:cv nonnegative-real/c] nonnegative-real/c)]

    [tab-split
     (-> string? (listof string?))]
    ;; Split a list of string by its tab characters

    [tab-join
     (-> (listof string?) string?)]
    ;; Join a list of strings by tab characters

    [path-string->string
     (-> path-string? string?)]
    ;; Convert a string or a path to a string

    [path-string->path
     (-> path-string? path?)]

    [ensure-directory
     (-> path-string? void?)]
    ;; If given directory exists, do nothing. Else create it.

    [rnd
     (-> real? string?)]
    ;; Render a number as a string, round to 2 decimal places.

    [pct
     (-> real? real? real?)]
    ;; `(pct 1 4)` returns 25

    [log2
     (-> exact-nonnegative-integer? exact-nonnegative-integer?)]

    [file-remove-extension
     (-> path-string? path-string?)]
    ;; Removes a Racket-added extension from a filename.
    ;; `(file-remove-extension "foo_tab.gz")` returns "foo.tab"

    [save-pict
     (-> path-string? pict? boolean?)]
    ;; Save the given pict to the given filename (in .png format)

    [columnize
     (-> list? exact-nonnegative-integer? (listof list?))]
    ;; Split a list into almost-equally-sized components.
    ;; Order / partitioning of elements is unspecified.

    [take*
     (-> list? exact-nonnegative-integer? (listof list?))]
    ;; Division. Splits a list into lists of size N except
    ;; the final list may be shorter.

    [force/cpu-time
     (-> (-> any) (values any/c exact-nonnegative-integer?))]

    [time-string->values
      (-> string? (values exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?))]

    [time-string->cpu-time
      (-> string? exact-nonnegative-integer?)]

    [time-string->real-time
      (-> string? exact-nonnegative-integer?)]

    [time-string->gc-time
      (-> string? exact-nonnegative-integer?)]

    [bitstring?
      (-> any/c boolean?)]

    [natural->bitstring
     (-> exact-nonnegative-integer? #:bits exact-positive-integer? string?)]
    ;; (natural->bitstring n k) converts `n` into a `k`-digit string of 1's and 0's

    [bitstring->natural
     (-> string? exact-nonnegative-integer?)]
    ;; Inverse of natural->bitstring

    [count-zero-bits
     (-> string? exact-nonnegative-integer?)]

    [copy-file*
      (->* [(and/c path-string? directory-exists?)
            (and/c path-string? directory-exists?)]
           [path-string?]
          void?)]

    [copy-directory/files*
      (->* [(and/c path-string? directory-exists?)
            (and/c path-string? directory-exists?)]
           [path-string?]
           void?)]

    [copy-racket-file*
      (-> (and/c path-string? directory-exists?)
          (and/c path-string? directory-exists?)
          void?)]

    [enumerate
      (-> list?  list?)]

    [digit*->integer
      (-> (listof digit/c) exact-integer?)]

    [integer->digit*
      (-> exact-integer? (listof digit/c))]

    [filename-sort
      (-> (listof filename/c) (listof filename/c))]

    [whitespace-string?
      (-> string? boolean?)]

    [simple-comment-string?
      (-> string? boolean?)]

    [timestamp
      (->* [] [any/c] string?)]

    [string->value
      (-> string? any/c)]

    [seconds->sql-timestamp
      (-> (or/c string? real?) string?)]

    [sql-timestamp->seconds
      (-> string? exact-nonnegative-integer?)]

    [glob-first
      (->* (any/c) (#:choose (or/c #f (-> (listof path-string?) any))) any)]

))

(require
  file/glob
  (only-in math/statistics
    mean
    stddev/mean)
  (only-in racket/file
    copy-directory/files)
  (only-in racket/format
    ~r)
  (only-in racket/class
    send)
  (only-in racket/date
    date-display-format
    date->string
    date->seconds
    current-date)
  (only-in racket/port
    with-input-from-string)
  (only-in pict
    pict?
    pict->bitmap)
  (only-in racket/set
    set-union
    list->set)
  (only-in racket/path
    file-name-from-path
    path-only)
  (only-in racket/list
    make-list)
  (only-in racket/string
    string-trim
    string-prefix?
    string-join
    string-split)
  (for-syntax racket/base))

;; =============================================================================

(define-logger gtp-util)

(define filename/c
  (flat-named-contract 'filename/c (and/c path-string?  (not/c path-only))))

(define nonnegative-real/c
  (flat-named-contract 'nonnegative-real/c (>=/c 0)))

(define digit/c
  (flat-named-contract 'digit/c (integer-in 0 9)))

(define (unique-listof/c ctc)
  (and/c (listof ctc)
         no-duplicates?))

(define (no-duplicates? x*)
  (let loop ([x* x*])
    (or (null? x*)
        (and (not (member (car x*) (cdr x*)))
             (loop (cdr x*))))))

(define TAB "\t")

(define (tab-split str)
  (string-split str TAB))

(define (tab-join str*)
  (string-join str* TAB))

(define (path-string->string ps)
  (if (string? ps) ps (path->string ps)))

(define (path-string->path ps)
  (if (string? ps) (string->path ps) ps))

(define (rnd n)
  (~r n #:precision '(= 2)))

(define (ensure-directory d)
  (unless (path-string? d)
    (raise-argument-error 'ensure-directory "path-string?" d))
  (unless (directory-exists? d)
    (make-directory d)))

(define (pct part total)
  (* 100 (/ part total)))

(define (string-last-index-of str c)
  (for/fold ([acc #f])
            ([c2 (in-string str)]
             [i (in-naturals)])
    (if (eq? c c2) i acc)))

(define (file-remove-extension fn)
  (define str (path-string->string fn))
  (define no-ext (path->string (path-replace-extension str #"")))
  (define i (string-last-index-of no-ext #\_))
  (string-append (substring no-ext 0 i) "." (substring no-ext (+ i 1))))

(define (confidence-offset x* #:cv [cv 1.96])
  (define u (mean x*))
  (define n (length x*))
  (define s (stddev/mean u x*))
  (define cv-offset (/ (* cv s) (sqrt n)))
  (if (negative? cv-offset)
    (raise-user-error 'confidence-interval "got negative cv offset ~a\n" cv-offset)
    cv-offset))

(define (confidence-interval x* #:cv [cv 1.96])
  (define offset (confidence-offset x* #:cv cv))
  (define u (mean x*))
  (cons (- u offset) (+ u offset)))

(define (log2 n)
  (cond
   [(zero? n)
    (raise-argument-error 'log2 "power-of-2" n)]
   [(= n 1)
    0]
   [else
    (let loop ([k 1])
      (define k^ (expt 2 k))
      (cond
       [(= n k^)
        k]
       [(< n k^)
        (raise-argument-error 'log2 "power-of-2" n)]
       [else
        (loop (+ k 1))]))]))

(define (save-pict fn p)
  (define bm (pict->bitmap p))
  (send bm save-file fn 'png))

(define (safe-take x* n)
  (cond
   [(zero? n)
    (values '() x*)]
   [(null? x*)
    (values '() '())]
   [else
    (define-values [hd tl] (safe-take (cdr x*) (- n 1)))
    (values (cons (car x*) hd) tl)]))

(define (columnize x* n)
  (let loop ([x* x*])
    (define-values [hd tl] (safe-take x* n))
    (define l (length hd))
    (cond
     [(< l n)
      (append (map list hd) (make-list (- n l) '()))]
     [else
      (define y** (loop tl))
      (for/list ([h (in-list hd)]
                 [y* (in-list y**)])
        (cons h y*))])))

(define (take* x* n)
  (let loop ([x* x*])
    (if (null? x*)
      '()
      (let-values (((hd tl) (safe-take x* n)))
        (cons hd (loop tl))))))

(define (force/cpu-time t)
  (let-values ([(r* cpu real gc) (time-apply t '())])
    (values (car r*) cpu)))

(define time-string->values
  (let ()
    (define TIME-RX #rx"^cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)")
    (define-syntax (time-match->values stx)
      (syntax-case stx ()
       [(_ match-val)
        (with-syntax ([(acc ...) (list #'cadr #'caddr #'cadddr)])
          (syntax/loc stx (values (string->number (acc match-val)) ...)))]))
    (lambda (str)
      (define m (regexp-match TIME-RX str))
      (if m
        (time-match->values m)
        (raise-arguments-error 'time-string->values "param does not match 'time' regexp" "param" str "regexp" TIME-RX)))))

(define (time-string->cpu-time str)
  (let-values ([(cpu real gc) (time-string->values str)])
    cpu))

(define (time-string->real-time str)
  (let-values ([(cpu real gc) (time-string->values str)])
    real))

(define (time-string->gc-time str)
  (let-values ([(cpu real gc) (time-string->values str)])
    gc))

(define (bitstring? x)
  (and (string? x)
       (for/and ([c (in-string x)])
         (or (eq? c #\0)
             (eq? c #\1)))))

;; Convert a natural number to a binary string, padded to the supplied width
(define (natural->bitstring n #:bits pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

(define (bitstring->natural str)
  (define N (string-length str))
  (for/sum ([i (in-range N)])
    (define c (string-ref str (- N (add1 i))))
    (if (equal? #\1 c)
        (expt 2 i)
        0)))

(define (count-zero-bits str)
  (for/sum ([c (in-string str)]
            #:when (eq? c #\0))
    1))

(define (copy-file* src dst [pattern "*.*"])
  (copy-thing copy-file src dst pattern))

(define (copy-directory/files* src dst [pattern "*"])
  (copy-thing copy-directory/files src dst pattern))

(define (copy-thing f-copy src dst pattern)
  (for ([src-file (in-glob (build-path src pattern))])
    (define src-name (file-name-from-path src-file))
    (f-copy src-file (build-path dst src-name))))

(define (copy-racket-file* src dst)
  (copy-file* src dst "*.rkt"))

(define (enumerate x*)
  (for/list ([x (in-list x*)]
             [i (in-naturals)])
    (cons i x)))

(define (digit*->integer d*)
  (cond
   [(null? d*)
    0]
   [else
    (define m*
      (for/fold ([acc '(1)])
                ([_ (in-list (cdr d*))])
        (cons (* 10 (car acc)) acc)))
    (for/sum ([d (in-list d*)]
              [m (in-list m*)])
      (* d m))]))

(define (integer->digit* i)
  (let loop ([acc '()]
             [prev 0]
             [m 1])
    (define m+ (* m 10))
    (define n (quotient (- (modulo i m+) prev) m))
    (define acc+ (cons n acc))
    (if (< i m+)
      acc+
      (loop acc+ (+ prev n) m+))))

(define (filename-sort ps*)
  (sort (map path-string->path ps*) path<?))

(define (whitespace-string? str)
  (zero? (string-length (string-trim str))))

(define (simple-comment-string? str)
  (define trimmed (string-trim str #:right? #f))
  (string-prefix? trimmed ";"))

(define (timestamp [time? #true])
  (parameterize ((date-display-format 'iso-8601))
    (date->string (current-date) time?)))

(define (string->value str)
  (with-input-from-string str read))

(define (seconds->sql-timestamp pre-n)
  (define dd
    (let* ((n (if (string? pre-n) (string->number pre-n) pre-n)))
      (seconds->date n)))
  (format "~a-~a-~a ~a:~a:~a"
          (date-year dd)
          (~r2 (date-month dd))
          (~r2 (date-day dd))
          (~r2 (date-hour dd))
          (~r2 (date-minute dd))
          (~r2 (date-second dd))))

(define (sql-timestamp->seconds ts)
  (define-values [year-str time-str]
    (let* ((s* (string-split ts " "))
           (s* (if (= 2 (length s*)) s* (string-split ts "T"))))
      (values (car s*) (cadr s*))))
  (define dd
    (let* ((yymmdd (string-split year-str "-"))
           (hhmmss (string-split time-str ":")))
      (date
        (string->number (caddr hhmmss))
        (string->number (cadr hhmmss))
        (string->number (car hhmmss))
        (string->number (caddr yymmdd))
        (string->number (cadr yymmdd))
        (string->number (car yymmdd))
        0
        0
        #f
        0)))
  (date->seconds dd))

(define (~r2 n)
  (~r n #:min-width 2 #:pad-string "0"))

(define (glob-first str #:choose [choose-fn #f])
  (define mm* (glob str))
  (cond
    ((null? mm*)
     (raise-user-error 'glob-first "No results for pattern '~a'" str))
    ((null? (cdr mm*))
     (car mm*))
    (choose-fn
     (choose-fn mm*))
    (else
     (log-gtp-util-error "glob-first: ambiguous results for pattern '~a', returning the first" str)
     (car mm*))))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs racket/runtime-path (only-in pict blank))

  (define CI? (equal? "true" (getenv "CI")))
  (define temp-dir (find-system-path 'temp-dir))
  (define-runtime-path cwd ".")

  (define (make-fresh-filename)
    (let loop ((s (gensym)))
      (define p (build-path temp-dir (symbol->string s)))
      (if (or (directory-exists? p) (file-exists? p))
        (loop (gensym))
        p)))

  (test-case "filename/c"
    (check-apply* filename/c
     ["a.rkt"
      ==> #true]
     [(string->path "a.rkt")
      ==> #true]
     [(build-path "a.rkt")
      ==> #true]
     [(build-path "b" "a.rkt")
      ==> #false]
     [42
      ==> #false]))

  (test-case "nonnegative-real/c"
    (check-pred nonnegative-real/c 0)
    (check-pred nonnegative-real/c 1)
    (check-pred nonnegative-real/c 200)
    (check-pred nonnegative-real/c 3.14)

    (check-false (nonnegative-real/c #f))
    (check-false (nonnegative-real/c -1))
    (check-false (nonnegative-real/c -0.00099)))

  (test-case "digit/c"
    (check-apply* digit/c
     [0 ==> #true]
     [5 ==> #true]
     [-1 ==> #false]
     [1.1 ==> #false]
     [10 ==> #false]))

  (test-case "unique-listof/c"
    (check-pred (unique-listof/c symbol?) '())
    (check-pred (unique-listof/c symbol?) '(A))
    (check-pred (unique-listof/c symbol?) '(A B C))
    (check-pred (unique-listof/c string?) '("A" "B" "C"))
    (check-pred (unique-listof/c (listof integer?)) '((1) (1 2) (3)))

    (check-false ((unique-listof/c integer?) 1))
    (check-false ((unique-listof/c integer?) '#(1)))
    (check-false ((unique-listof/c integer?) '(1 2 1)))
    (check-false ((unique-listof/c string?) '("A" "B" "B"))))

  (test-case "confidence-interval"
    (define n* '(1 2 2 2 2 2 3))
    (define ivl (confidence-interval n* #:cv 0.95))
    (check < 1 (car ivl))
    (check < (car ivl) 2)
    (check < 2 (cdr ivl))
    (check < (cdr ivl) 3))

  (test-case "confidence-offset"
    (let ([n* '(1 1 1 1)])
      (define offset (confidence-offset n* #:cv 0.95))
      (check-equal? offset 0))

    (let ([n* '(1 2 3)])
      (define offset (confidence-offset n* #:cv 0.5))
      (check < 0 offset)
      (check < offset 1)))

  (test-case "tab-split"
    (check-equal? (tab-split "hello") '("hello"))
    (check-equal? (tab-split "dr racket") '("dr racket"))
    (check-equal? (tab-split "dr\tracket") '("dr" "racket")))

  (test-case "tab-join"
    (check-apply* tab-join
     ['()
      ==> ""]
     ['("a" "b" "c")
      ==> "a\tb\tc"]))

  (test-case "path-string->string"
    (check-equal? (path-string->string "hi") "hi")
    (check-equal? (path-string->string (string->path "hi")) "hi"))

  (test-case "ensure-directory"
    (unless CI?
      (define temp-dir (find-system-path 'temp-dir))
      (define fresh-dirname
        (make-fresh-filename))
      (check-false (directory-exists? fresh-dirname))
      (ensure-directory fresh-dirname)
      (check-pred directory-exists? fresh-dirname)
      (ensure-directory fresh-dirname)
      (check-pred directory-exists? fresh-dirname)
      (delete-directory fresh-dirname)))

  (test-case "rnd"
    (check-equal? (rnd 2) "2.00")
    (check-equal? (rnd 1/3) "0.33"))

  (test-case "pct"
    (check-equal? (pct 1 2) 50)
    (check-equal? (rnd (pct 1 3)) "33.33"))

  (test-case "log2"
    (check-equal? (log2 1) 0)
    (check-equal? (log2 2) 1)
    (check-equal? (log2 8) 3)
    (check-equal? (log2 4096) 12)

    (check-exn exn:fail:contract?
      (λ () (log2 0)))
    (check-exn exn:fail:contract?
      (λ () (log2 3)))
    (check-exn exn:fail:contract?
      (λ () (log2 -1)))
    (check-exn exn:fail:contract?
      (λ () (log2 72))))

  (test-case "file-remove-extension"
    (check-equal? (file-remove-extension "foo_tab.gz") "foo.tab")
    (check-equal? (file-remove-extension "a_b.c") "a.b"))

  (test-case "save-pict"
    (unless CI?
      (define fname (make-fresh-filename))
      (define p (blank 10 10))
      (check-false (file-exists? fname))
      (check-true (save-pict fname p))
      (check-pred file-exists? fname)
      (delete-file fname)))

  (test-case "columnize"
    (define (check-columnize x* n)
      (define y** (columnize x* n))
      (define L (length (car y**)))
      (check-true (for/and ([y* (in-list (cdr y**))])
                    (define m (length y*))
                    (or (= L m) (= L (+ m 1)))))
      (check-equal? (list->set x*) (apply set-union (map list->set y**))))

    (check-columnize '() 2)
    (check-columnize '(1) 2)
    (check-columnize '(1 2) 2)
    (check-columnize '(1 2 3) 2)
    (check-columnize '(1 2 3 4 5 6 7) 3))

  (test-case "force/cpu-time"
    (let-values ([(v c) (force/cpu-time (λ () 42))])
      (check-equal? v 42)
      (check-true (< c 10))))

  (test-case "bitstring?"
    (check-apply* bitstring?
      [2
       ==> #false]
      ["asdf"
       ==> #false]
      ["003"
       ==> #false]
      ["0101"
       ==> #true]
      ["111111111"
       ==> #true]))

  (test-case "natural->bitstring"
    (check-apply* natural->bitstring
     [0 #:bits 4
      ==> "0000"]
     [2 #:bits 2
      ==> "10"]
     [2 #:bits 10
      ==> "0000000010"]))

  (test-case "bitstring->natural"
    (check-apply* bitstring->natural
     ["10"
      ==> 2]
     ["111"
      ==> 7]
     ["0000000010"
      ==> 2]))

  (test-case "count-zero-bits"
    (check-apply* count-zero-bits
     ["10"
      ==> 1]
     ["0000000010"
      ==> 9]))

  (test-case "string-last-index-of"
    (check-equal? (string-last-index-of "hello" #\h) 0)
    (check-equal? (string-last-index-of "hello" #\o) 4)
    (check-equal? (string-last-index-of "hello" #\l) 3)
    (check-equal? (string-last-index-of "hello" #\Q) #f))

  (test-case "enumerate"
    (check-equal?
      (enumerate '())
      '())
    (check-equal?
      (enumerate '(A))
      '((0 . A)))
    (check-equal?
      (enumerate '(A B C))
      '((0 . A) (1 . B) (2 . C))))

  #;(test-case "copy-racket-file*"
    (when CI?
      (when (directory-exists? MY-DIR)
        (delete-directory/files MY-DIR))
      (make-directory MY-DIR)
      (check-true
        (zero?  (set-count (racket-filenames MY-DIR))))
      (check-false
        (zero? (set-count (racket-filenames T-DIR))))
      (void
        (copy-racket-file* T-DIR MY-DIR))
      (check-false
        (zero? (set-count (racket-filenames MY-DIR))))
      (check-equal?
        (set-count (racket-filenames T-DIR))
        (set-count (racket-filenames MY-DIR)))
      ;; cleanup
      (delete-directory/files MY-DIR)))

  (test-case "integer->digit*"
    (check-apply* integer->digit*
      [0 => '(0)]
      [1 ==> '(1)]
      [123 ==> '(1 2 3)]))

  (test-case "digit*->integer"
    (check-apply* digit*->integer
      ['() ==> 0]
      ['(1) ==> 1]
      ['(1 2 3) ==> 123]))

  (test-case "filename-sort"
    (check-apply* filename-sort
      ['("a.rkt" "b.rkt" "c.rkt")
       ==> (map string->path '("a.rkt" "b.rkt" "c.rkt"))]
      ['()
       ==> '()]
      [(map string->path '("procedure" "is" "generally" "the" "right" "choice"))
       ==> (map string->path '("choice" "generally" "is" "procedure" "right" "the"))]))

  (test-case "whitespace-string?"
    (check-apply* whitespace-string?
     [""
      ==> #true]
     ["    \t"
      ==> #true]
     ["    x"
     ==> #false]))

  (test-case "simple-comment-string?"
    (check-apply* simple-comment-string?
     [""
      ==> #false]
     ["   ;"
      ==> #true]
     ["    #;   (1 2 4)"
     ==> #false]
     ["#| |#"
     ==> #false]
     ["#|"
     ==> #false]))

  (test-case "time-string->values"
    (check-apply* time-string->cpu-time
     ["cpu time: 243240 real time: 242930 gc time: 92"
      ==> 243240]
     ["cpu time: 924 real time: 925 gc time: 80"
      ==> 924])
    (check-apply* time-string->real-time
     ["cpu time: 243240 real time: 242930 gc time: 92"
      ==> 242930]
     ["cpu time: 924 real time: 925 gc time: 80"
      ==> 925])
    (check-apply* time-string->gc-time
     ["cpu time: 243240 real time: 242930 gc time: 92"
      ==> 92]
     ["cpu time: 924 real time: 925 gc time: 80"
      ==> 80]))

  (test-case "string->value"
    (check-equal? (string->value "hello") 'hello)
    (check-equal? (string->value "42") 42)
    (check-equal? (string->value "(\"oh\" no)") '("oh" no)))

  (test-case "seconds->sql-timestamp"
    (check-equal? (seconds->sql-timestamp "1611969956") "2021-01-29 20:25:56")
    (check-equal? (seconds->sql-timestamp 1611970000) "2021-01-29 20:26:40"))

  (test-case "sql-timestamp->seconds"
    (check-equal? (sql-timestamp->seconds "2021-01-29 20:25:56") 1611969956)
    (check-equal? (sql-timestamp->seconds "2021-01-29T20:26:40") 1611970000))

  (test-case "glob-first"
    (unless CI?
      (define lname "LICENSE.txt")
      (define rkt-pat (build-path cwd "*.rkt"))
      (check-equal? (path->string (file-name-from-path (glob-first (build-path cwd lname)))) lname)
      (check-equal? (file-name-from-path (glob-first rkt-pat))
                    (file-name-from-path (car (glob rkt-pat))))
      (check-exn exn:fail:user?
                 (lambda () (glob-first "NOFILEMATCHING PATTERN")))))

)
