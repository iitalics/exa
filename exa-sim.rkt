#lang racket
(require racket/flonum
         racket/generator)

(provide (struct-out eqnsys))
(struct eqnsys
  (cfg-desc
   out-desc
   gen-init
   mutator))

(provide (struct-out eqnsim))
(struct eqnsim
  (sys dt cfg [data #:mutable]))

(provide user-config?)
(define user-config?
  (listof (cons/c symbol? real?)))

(provide output-spec?)
(define output-spec?
  (listof symbol?))



;;; make a new simulation based on a given system, user config & delta time value
(provide make-eqnsim)
(define/contract make-eqnsim (-> eqnsys?
                                 user-config?
                                 #:dt (and/c positive? real?)
                                 eqnsim?)
  (lambda (sys #:dt dt user-cfg)
    (let ([cfg (parse-cfg sys user-cfg)])
      (eqnsim sys
              (real->double-flonum dt)
              cfg
              ((eqnsys-gen-init sys) cfg)))))

(define (parse-cfg sys user-cfg)
  (let ([cfg (make-flvector (length (eqnsys-cfg-desc sys)))])
    (for ([i (in-naturals)]
          [cd (in-list (eqnsys-cfg-desc sys))])
      (match* ((assq (car cd) user-cfg) cd)
        [((cons _ val) _)  (flvector-set! cfg i (real->double-flonum val))]
        [(#f (cons key #f)) (error (format "required variable `~a' not specified" key))]
        [(#f (cons _ auto)) (flvector-set! cfg i auto)]))
    cfg))


;;; create a sequence to iterate through this simulator
(provide in-eqnsim)
(define/contract in-eqnsim (->* (eqnsim?
                                 output-spec?)
                                (#:count integer?)
                                sequence?)
  (lambda (sim out-spec #:count [max-count #f])
    (define outputter (parse-outputter (eqnsim-sys sim) out-spec))
    (in-generator
     (let loop ([work #f]
                [n 0])
       (when (and (when max-count
                    (< n max-count)))
         (yield (outputter (eqnsim-data sim)))
         (loop (eqnsim-adv! sim work)
               (add1 n)))))))

;;; advance the simulation; returns old data set
(provide eqnsim-adv!)
(define/contract eqnsim-adv! (->* (eqnsim?)
                                  ((or/c #f flvector?))
                                  flvector?)
  (lambda (sim [work #f])
    (let* ([sys (eqnsim-sys sim)]
           [data (eqnsim-data sim)]
           [cfg (eqnsim-cfg sim)]
           [dt (eqnsim-dt sim)]
           [work (or work (flvector-copy data))])
      ((eqnsys-mutator sys) cfg dt data work)
      (set-eqnsim-data! sim work)
      data)))

;;; get output from simulation in the given format
(provide eqnsim-output)
(define/contract eqnsim-output (-> eqnsim? output-spec? list?)
  (lambda (sim out-spec)
    (let ([outputter (parse-outputter (eqnsim-sys sim)
                                      out-spec)])
      (outputter (eqnsim-data sim)))))

;;; advance the simulation and return the prev data using output format
(define (eqnsim-next! sim out-spec)
  (let ([outputter (parse-outputter (eqnsim-sys sim)
                                    out-spec)]
        [data (eqnsim-adv! sim)])
    (outputter data)))

(define (parse-outputter sys out-spec)
  (let ([ixs (map (lambda (v)
                    (or (index-of (eqnsys-out-desc sys) v)
                        (error (format "invald output variable: `~a'" v))))
                  out-spec)])
    (lambda (data)
      (map (lambda (i) (flvector-ref data i)) ixs))))






(define sys-2d-motion
  (eqnsys '((g . 9.8)
            (p.x-init . #f)
            (p.y-init . #f)
            (v.y-init . 0.0)
            (v.x . #f))
          '(p.x p.y v.y)
          (lambda (cfg)
            (flvector (flvector-ref cfg 1)
                      (flvector-ref cfg 2)
                      (flvector-ref cfg 3)))
          (lambda (cfg dt in out)
            (let ([p.x (flvector-ref in 0)]
                  [p.y (flvector-ref in 1)]
                  [v.y (flvector-ref in 2)]
                  [g (flvector-ref cfg 0)]
                  [v.x (flvector-ref cfg 4)]
                  )
              (flvector-set! out 0 (+ p.x (* dt v.x)))
              (flvector-set! out 1 (max 0.0 (+ p.y (* dt v.y))))
              (flvector-set! out 2 (if (and (<= p.y 0)
                                            (< v.y 0))
                                       (* v.y -0.8)
                                       (+ v.y (* -1 dt g))))))
          ))

(define (sim-2d-motion)
  (make-eqnsim sys-2d-motion
               #:dt 1/28
               '((p.x-init . 0.0)
                 (p.y-init . 3.0)
                 (v.x . +2)
                 (v.y-init . +5))))
