#!r7rs

(import (scheme base) (scheme inexact) (scheme write))

;;; --------------------------------------------
;;;  Projet de Programmation Avancée
;;;  Simulateur de mini-réseau ferroviaire
;;;  Pierre-Alexandre Rusthul - Samuel Bertrand
;;; --------------------------------------------

(define (make-top n)
  (let ((i 0))
    (lambda (msg)
      (cond ((eq? msg 'see) (lambda () i)) ; Voir la valeur de la borne
            ((eq? msg 'incr) (lambda ()
                                 ;; Passer à la borne suivante
                                 (set! i (modulo (+ i 1) n))
                                 i))))))

;; structure tlist pour une file FIFO
(define (make-tlist)
  (let ((front '())
        (back  '()))
    (define (normalize!)
      (when (null? front)
        (set! front (reverse back))
        (set! back '())))
    (lambda (msg)
      (cond
         ((eq? msg 'see)
         (lambda ()
           (append front (reverse back))))
         ((eq? msg 'empty?)
         (lambda ()
           (and (null? front) (null? back))))
         ((eq? msg 'enqueue)
         (lambda (x)
           (set! back (cons x back))))
        ((eq? msg 'dequeue)
         (lambda ()
           (normalize!)
           (if (null? front)
               #f
               (let ((val (car front)))
                 (set! front (cdr front))
                 val))))))))
 
;; structure de train
;; name : nom
;; circuit-length : taille du circuit
(define (make-train name circuit-length)
  (let ((top      (make-top circuit-length))
        (waiting? #t))   ; au départ, tous les trains attendent à la gare
    (lambda (msg)
      (cond
        ((eq? msg 'name)
         (lambda () name))
        ((eq? msg 'position)
         (lambda () ((top 'see))))
        ((eq? msg 'waiting?)
         (lambda () waiting?))
        ((eq? msg 'set-waiting!)
         (lambda ()
           (set! waiting? #t)))
         ((eq? msg 'set-running!)
         (lambda ()
           (set! waiting? #f)))
         ((eq? msg 'incr)
         (lambda ()
           ((top 'incr))))))))
 
;; structure de la gare centrale
;; initial-trains : liste de trains
(define (make-station initial-trains)
  (let ((queue (make-tlist)))
     (for-each (lambda (t) ((queue 'enqueue) t))
              initial-trains)
    (lambda (msg)
      (cond
         ((eq? msg 'see-queue)
         (lambda ()
           (map (lambda (t) ((t 'name))) ((queue 'see)))))
         ((eq? msg 'queueing)
         (lambda (train)
           ((queue 'enqueue) train)))
         ((eq? msg 'dequeueing)
         (lambda ()
           ((queue 'dequeue))))
         ((eq? msg 'empty?)
         (lambda ()
           ((queue 'empty?))))))))
 
;; fait avancer d'une étape la simulation
(define (step! station trains)
   (let ((departing ((station 'dequeueing))))
    (when departing
      ((departing 'set-running!))))
   (for-each
    (lambda (t)
      (when (not ((t 'waiting?)))          
        (let ((new-pos ((t 'incr))))
          (when (= new-pos 0)   
            ((t 'set-waiting!))
            ((station 'queueing) t)))))
    trains))
 
;; retourne une liste comportant la liste des trains dans la gare principale et la position de chaque train sur son circuit
(define (snapshot station trains)
  (cons ((station 'see-queue)) (map (lambda (t) ((t 'position))) trains)))

;; fonction principale
;; train-list : liste de trains
;; steps : nombre d'étapes à simuler
;; renvoie une liste de snapshots contenant l'état initial et les étapes simulées
(define (trains train-list steps)
  (let ((station (make-station train-list)))
     (let loop ((remaining steps)
               (acc (list (snapshot station train-list))))
      (if (= remaining 0)
          (reverse acc)
          (begin
            (step! station train-list)
            (loop (- remaining 1)
                  (cons (snapshot station train-list) acc)))))))
              
;; affichage chaque état de manière lisible
(define (display-simulation result)
  (let loop ((states result) (step 0))
    (unless (null? states)
      (display "Étape ")
      (display step)
      (display " : ")
      (display (car states))
      (newline)
      (loop (cdr states) (+ step 1)))))
  
;;; --------
;;; Exemples
;;; --------
  

(define example1 (trains (list (make-train 'train-1 10) (make-train 'train-2 14) (make-train 'train-3 18)) 35))

(define example2 (trains (list (make-train 'train-1 10) (make-train 'train-2 9) (make-train 'train-3 8)) 35))

(define example3 (trains (list (make-train 'train-1 10) (make-train 'train-2 8) (make-train 'train-3 6) (make-train 'train-4 4) (make-train 'train-5 2)) 35))


(display "=== Simulation 1 du réseau ferroviaire ===")
(newline)
(display-simulation example1)
(newline)
(display "=== Simulation 2 du réseau ferroviaire ===")
(newline)
(display-simulation example2)
(newline)
(display "=== Simulation 3 du réseau ferroviaire ===")
(newline)
(display-simulation example3)
