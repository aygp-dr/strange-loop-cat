  ;; Functor instance for lists
  (define (list-map f lst)
    (map f lst))

  ;; Functor instance for promises
  (define (promise-map f promise)
    (promise-then promise 
                 (lambda (value) (promise-resolve (f value)))))

  ;; Contravariant functor for predicates
  (define (contramap f pred)
    (lambda (x) (pred (f x))))

  ;; Simple lens as a profunctor
  (define-record-type <lens>
    (make-lens getter setter)
    lens?
    (getter lens-getter)
    (setter lens-setter))

  ;; Maybe monad bind
  (define (maybe-bind maybe-val f)
    (if (nothing? maybe-val)
        nothing
        (f (just-value maybe-val))))

  ;; List monad bind (flatMap)
  (define (list-bind lst f)
    (apply append (map f lst)))

  ;; State monad operations
  (define (state-return x)
    (lambda (s) (cons x s)))
  
  (define (state-bind m f)
    (lambda (s)
      (let* ((result (m s))
             (value (car result))
             (new-state (cdr result)))
        ((f value) new-state))))

  ;; IO monad in Scheme
  (define-record-type <io>
    (make-io thunk)
    io?
    (thunk io-thunk))
  
  (define (io-return x)
    (make-io (lambda () x)))
  
  (define (io-bind io-action f)
    (make-io
     (lambda ()
       (let ((result ((io-thunk io-action))))
         ((io-thunk (f result)))))))

  ;; Free monad for GEB MIU system
  (define-record-type <free>
    (make-free type payload)
    free?
    (type free-type)  ;; 'pure or 'free
    (payload free-payload))
  
  (define (free-pure x)
    (make-free 'pure x))
  
  (define (free-lift action)
    (make-free 'free action))

  ;; Fixed point of functor for recursive types
  (define-record-type <fix>
    (make-fix unfix)
    fix?
    (unfix fix-unfix))
  
  ;; List functor
  (define-record-type <list-f>
    (make-list-f head tail)
    list-f?
    (head list-f-head)
    (tail list-f-tail))

  ;; Yoneda conversion for lists
  (define (yoneda-from-list lst)
    (lambda (f)
      (map f lst)))
  
  (define (yoneda-to-list y)
    (y identity))

  ;; Kleisli composition for monads
  (define (kleisli-compose f g)
    (lambda (x)
      (bind (f x) g)))

  ;; Catamorphism (fold) with explicit functor
  (define (cata algebra structure)
    (algebra (map (lambda (substructure)
                    (cata algebra substructure))
                  (unfix structure))))

  ;; Comonad for one-dimensional cellular automaton
  (define-record-type <stream>
    (make-stream focus left right)
    stream?
    (focus stream-focus)
    (left stream-left)
    (right stream-right))
  
  (define (stream-extract stream)
    (stream-focus stream))

  ;; StateT monad transformer
  (define (state-t-return m-return state-val)
    (lambda (s)
      (m-return (cons state-val s))))
  
  (define (state-t-bind m-bind state-m f)
    (lambda (s)
      (m-bind (state-m s)
              (lambda (result)
                (let ((val (car result))
                      (s-prime (cdr result)))
                  ((f val) s-prime))))))

  ;; Simple adjunction example between lists and non-empty lists
  (define (list-to-nonempty lst)
    (if (null? lst)
        (error "Cannot convert empty list")
        lst))
  
  (define (nonempty-to-list ne-lst)
    ne-lst)
