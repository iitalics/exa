# exa
racket library for simulations with simple derivatives

## planned syntax

```racket
;; create a system
(require exa)
(define 1d-sys
  (generate-sys
   (cfg g) = -9.8
   (d/dt v #:init v-0) = (- g)
   (d/dt y #:init y-0) = v))

;; create a simulation of the system
(define 1d-sim
  (sim 1d-sys
       #:dt 0.1
       'y-0 10.0
       'v-0 +4.0))

;; run & plot simulation
(require plot)
(plot (lines (in-sim 1d-sim '(t y)))
      #:x-label "Time"
      #:y-label "Position (Y)")
```
