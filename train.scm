#!r7rs

(import (scheme base) (scheme inexact) (scheme write))

;;; --------------------------------------------
;;;  Projet de Programmation Avancée
;;;  Simulateur de mini-réseau ferroviaire
;;;  Pierre-Alexandre Rusthul - Samuel Bertrand
;;; --------------------------------------------


;;; ----------------------------------------------------------------
;;; Structure tlist (file FIFO)
;;;   - front : éléments à retirer (tête de file)
;;;   - back  : éléments ajoutés (queue de file, en ordre inverse)
;;; -----------------------------------------------------------------

(define (make-tlist)
  (let ((front '())
        (back  '()))
 
    (define (normalize!)
      ;; Si front est vide, on retourne back en le reversant dans front.
      (when (null? front)
        (set! front (reverse back))
        (set! back '())))
 
    (lambda (msg)
      (cond
 
        ;; Voir l'état complet de la file (front + back retourné)
        ((eq? msg 'see)
         (lambda ()
           (append front (reverse back))))
 
        ;; Tester si la file est vide
        ((eq? msg 'empty?)
         (lambda ()
           (and (null? front) (null? back))))
 
        ;; Enfiler un élément (ajout en queue)
        ((eq? msg 'enqueue)
         (lambda (x)
           (set! back (cons x back))))
 
        ;; Défiler un élément (retrait en tête)
        ;; Retourne #f si la file est vide.
        ((eq? msg 'dequeue)
         (lambda ()
           (normalize!)
           (if (null? front)
               #f
               (let ((val (car front)))
                 (set! front (cdr front))
                 val))))
 
        (else (error "tlist: message inconnu" msg))))))
 
 
;;; ------------------------------------------------------------
;;; Compteur circulaire (make-top)
;;; Fourni dans l'énoncé — compte de 0 à n-1 en boucle.
;;; ------------------------------------------------------------
 
(define (make-top n)
  (let ((i 0))
    (lambda (msg)
      (cond
        ;; Lire la borne courante
        ((eq? msg 'see)
         (lambda () i))
        ;; Passer à la borne suivante
        ((eq? msg 'incr)
         (lambda ()
           (set! i (modulo (+ i 1) n))
           i))
        (else (error "make-top: message inconnu" msg))))))
 
 
;;; ------------------------------------------------------------
;;; make-train — création d'un train
;;;
;;; Un train encapsule :
;;;   - son nom
;;;   - un compteur circulaire (make-top) pour sa position
;;;   - un flag indiquant s'il est en attente à la gare
;;;
;;; Messages supportés :
;;;   'name         → retourne le nom du train
;;;   'position     → retourne la borne actuelle
;;;   'waiting?     → #t si le train est en attente à la gare
;;;   'set-waiting! → place le train en attente (borne = 0 déjà gérée par top)
;;;   'incr         → avance d'une borne ; retourne la nouvelle valeur
;;; ------------------------------------------------------------
 
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
 
        ;; Le train entre en gare : marquer comme en attente
        ((eq? msg 'set-waiting!)
         (lambda ()
           (set! waiting? #t)))
 
        ;; Le train quitte la gare : marquer comme en route
        ((eq? msg 'set-running!)
         (lambda ()
           (set! waiting? #f)))
 
        ;; Avancer d'une borne et retourner la valeur
        ((eq? msg 'incr)
         (lambda ()
           ((top 'incr))))
 
        (else (error "make-train: message inconnu" msg))))))
 
 
;;; ------------------------------------------------------------
;;; make-station — la gare centrale
;;;
;;; La gare gère une file FIFO de trains en attente.
;;; Au départ, tous les trains fournis sont dans la file.
;;;
;;; Messages supportés :
;;;   'see-queue    → retourne la liste des noms en attente
;;;   'queueing     → un train entre dans la file
;;;   'dequeueing   → un train sort de la file (premier entré)
;;;   'empty?       → #t si aucun train en attente
;;; ------------------------------------------------------------
 
(define (make-station initial-trains)
  (let ((queue (make-tlist)))
 
    ;; Initialisation : on enfile tous les trains de départ
    (for-each (lambda (t) ((queue 'enqueue) t))
              initial-trains)
 
    (lambda (msg)
      (cond
 
        ;; Liste des NOMS des trains en attente
        ((eq? msg 'see-queue)
         (lambda ()
           (map (lambda (t) ((t 'name))) ((queue 'see)))))
 
        ;; Enfile un train
        ((eq? msg 'queueing)
         (lambda (train)
           ((queue 'enqueue) train)))
 
        ;; Défile le premier train (FIFO) ; #f si vide
        ((eq? msg 'dequeueing)
         (lambda ()
           ((queue 'dequeue))))
 
        ;; Teste si la file est vide
        ((eq? msg 'empty?)
         (lambda ()
           ((queue 'empty?))))
 
        (else (error "make-station: message inconnu" msg))))))
 
 
;;; ------------------------------------------------------------
;;; snapshot — capture de l'état courant
;;;
;;; Retourne une liste de la forme :
;;;   ((nom1 nom2 ...) pos1 pos2 pos3 ...)
;;; où les noms sont ceux des trains en attente, et les positions
;;; sont les bornes de chaque train (0 si en attente).
;;; ------------------------------------------------------------
 
(define (snapshot station trains)
  (cons
    ;; Noms des trains en attente
    ((station 'see-queue))
    ;; Position de chaque train
    (map (lambda (t) ((t 'position))) trains)))
 
 
;;; ------------------------------------------------------------
;;; step! — un pas de simulation
;;;
;;; Règles (appliquées simultanément, dans l'ordre décrit) :
;;;
;;;  1. Si un train attend en gare, on le fait partir (un seul).
;;;  2. Les trains EN ROUTE avancent d'une borne.
;;;     → Si l'un d'eux atteint la borne 0, il entre en gare.
;;;
;;; Retourne #f (la mutation se fait en place).
;;; ------------------------------------------------------------
 
(define (step! station trains)
 
  ;; 1. Faire partir UN train en attente, s'il y en a un
  (let ((departing ((station 'dequeueing))))
    (when departing
      ((departing 'set-running!))))
 
  ;; 2. Avancer tous les trains EN ROUTE
  (for-each
    (lambda (t)
      (when (not ((t 'waiting?)))          ; seulement ceux qui roulent
        (let ((new-pos ((t 'incr))))       ; avancer d'une borne
          (when (= new-pos 0)             ; borne 0 → retour en gare
            ((t 'set-waiting!))
            ((station 'queueing) t)))))
    trains))
 
 
;;; ------------------------------------------------------------
;;; trains — fonction principale
;;;
;;; Construit la simulation sur `steps` pas.
;;; Retourne une liste de snapshots (longueur = steps + 1,
;;; car on inclut l'état initial).
;;;
;;; Arguments :
;;;   train-list  — liste d'objets train (créés avec make-train)
;;;   steps       — nombre de transitions à simuler
;;; ------------------------------------------------------------
 
(define (trains train-list steps)
  (let ((station (make-station train-list)))
 
    ;; Construire la liste résultat par récursion terminale
    (let loop ((remaining steps)
               (acc (list (snapshot station train-list))))
 
      (if (= remaining 0)
          (reverse acc)
          (begin
            (step! station train-list)
            (loop (- remaining 1)
                  (cons (snapshot station train-list) acc)))))))
 
 
;;; --------
;;; Exemples
;;; --------
  
;;; Affichage lisible de chaque état
(define (display-simulation result)
  (let loop ((states result) (step 0))
    (unless (null? states)
      (display "Étape ")
      (display step)
      (display " : ")
      (display (car states))
      (newline)
      (loop (cdr states) (+ step 1)))))

(define example1 (trains (list (make-train 'train-1 10) (make-train 'train-2 14) (make-train 'train-3 18)) 35))

(define example2 (trains (list (make-train 'train-1 5) (make-train 'train-2 5) (make-train 'train-3 5)) 35))

(define example3 (trains (list (make-train 'train-1 10) (make-train 'train-2 9) (make-train 'train-3 8)) 35))

(display "=== Simulation 1 du réseau ferroviaire ===")
(newline)
(display-simulation example1)
(newline)
(display "=== Simulation 2 du réseau ferroviaire ===")
(newline)
(display-simulation example2)
(display "=== Simulation 3 du réseau ferroviaire ===")
(newline)
(display-simulation example3)

