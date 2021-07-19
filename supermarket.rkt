#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index isclose tt et queue) #:transparent)

(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 0 empty-queue))

(define (update f counters index)
 (cond
   ((null? counters) counters)
   (( eq? index (counter-index (car counters))) (cons (f(car counters)) (update f (cdr counters) index)))
   (else (cons (car counters) (update f (cdr counters) index)))))

(define (tt+ minutes)
  (lambda (x) (match x
    [(counter index isclose tt et queue)
     (make-counter (counter-index x) (counter-isclose x) (+ (counter-tt x) minutes) (counter-et x) (counter-queue x))])))

(define (queue-size q)
  (+ (queue-size-l q) (queue-size-r q)))

(define (et+ minutes)
  (lambda (x) (match x
    [(counter index isclose tt et queue)
     (make-counter (counter-index x) (counter-isclose x) (counter-tt x) (+ (counter-et x) minutes) (counter-queue x))])))

(define (add-to-counter name items)
  (lambda (x) (match x
    [(counter index isclose tt et queue)
     (make-counter (counter-index x) (counter-isclose x) (counter-tt ((tt+ items) x ))
                   (if (queue-empty? (counter-queue x)) (counter-et ((et+ items) x)) (counter-et x) )
                   (enqueue (cons name items) (counter-queue x)))])))

(define (sort-by KEY)
  (lambda (L)
    (cons (counter-index (car(sort L < #:key KEY))) (KEY (car(sort L < #:key KEY))))))

(define min-tt (sort-by counter-tt))
(define min-et (sort-by counter-et))

(define (remove-first-from-counter C)
  (match C
    [(counter index isclose tt et queue)
     (make-counter (counter-index C) (counter-isclose C) ( - (counter-tt C) (min (counter-tt C) (counter-et C)))
                   (if (or (queue-empty? (counter-queue C)) (= (queue-size (counter-queue C)) 1))  0 (cdr(top(dequeue (counter-queue C)))))
                   (dequeue (counter-queue C)))]))


  
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (>= minutes (counter-et C))
        (match C [(counter index isclose tt et queue) (make-counter (counter-index C) (counter-isclose C) (max (-(counter-tt C) minutes) 0) 0 (counter-queue C))])
        (match C [(counter index isclose tt et queue) (make-counter (counter-index C) (counter-isclose C) (-(counter-tt C) minutes) (-(counter-et C) minutes) (counter-queue C))]))))

(define (pass-helper minutes)
  (lambda (C)
    (if (and(>= minutes (counter-et C)) (> (queue-size(counter-queue C)) 0))
         ((pass-helper (- minutes (counter-et C))) (remove-first-from-counter C))
         ((pass-time-through-counter  minutes ) C))))

(define (get-different minutes acc)
  (lambda (x)
    (if (or (> (counter-et x) minutes) (= 0 (counter-et x) ))
        acc
    (if (> (queue-size (counter-queue ((pass-time-through-counter (counter-et x)) x))) (queue-size (counter-queue ((pass-helper (counter-et x)) x))))
      ((get-different ( - minutes (counter-et x)) (remove '()(append acc (list(cons (counter-index x) (car(top(counter-queue x)))))))) (remove-first-from-counter x))
      acc))))

(define (close index)
  (lambda (C)
  (if (= (counter-index C) index)
      (match C [(counter index isclose tt et queue) (make-counter (counter-index C) 1 (counter-tt C) (counter-et C) (counter-queue C))])
      C)))

(define (get-all counters minutes acc)
  (if (null? counters)
      acc
      (get-all (cdr (sort counters < #:key counter-et )) minutes (append acc ((get-different minutes '()) (car counters))))))

(define (rem-empty lst)
  (if (not(member '()  lst))  lst
      (rem-empty (remove '() lst))))

(define (get-ans)
  (lambda (x)
    (cons (counter-index x) (counter-queue x))))
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (get-sum counters sum)
  (cond
    ((null? counters) sum)
    (else (get-sum (cdr counters) (+ sum (counter-tt (car counters)))))))

(define (find-num-new average counters)
  (if (>= ( - (/ (get-sum (get-open counters '()) 0) average) (length (get-open counters '()))) 0 )
      (ceiling( - (/ (get-sum (get-open counters '()) 0) average) (length (get-open counters '() ))))
      0))

(define remove-close
  (lambda (x)
  (cond
    ((= 0 (counter-isclose x)) x ))))

(define (add-empty-counters slow num)
  (if (= num 0) slow
      (add-empty-counters (flatten(cons slow (empty-counter ( + (counter-index(list-ref slow (- (length slow) 1))) 1)))) (- num 1) )))

(define (add-to-index name n-items index)
  (lambda (x)
    (if (= index (counter-index x))
        ((add-to-counter name n-items) x)
        x)))

(define (min-nclose counters x)
  (cond
    ((null? counters) x)
    ((if (and (< (counter-tt (car counters)) (cdr x))  (= 0 (counter-isclose (car counters))))
         (min-nclose (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)) ))
        (min-nclose (cdr counters) x)))))


(define (get-open counters acc)
  (if (null? counters) acc
      (get-open (cdr counters) (if (= 0 (counter-isclose (car counters))) (append acc (list (car counters))) acc))))
(define (remove-empty counters acc)
  (if (null? counters) acc
  ( if (queue-empty? (counter-queue (car counters)))
       (remove-empty (cdr counters) acc)
       (remove-empty (cdr counters) (append acc (list (car counters))) )))   )

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()  ))

(define (serve-helper requests fast-counters slow-counters mylist)
  (if (null? requests)
      (append  (list(rem-empty mylist)) (map (get-ans)(remove-empty fast-counters '()))  (map (get-ans) (remove-empty slow-counters '())))
      (match (car requests)
        [(list 'close index) (serve-helper (cdr requests) (map (close index) fast-counters) (map (close index) slow-counters) mylist )]
        [(list 'ensure minutes) (serve-helper (cdr requests) fast-counters (add-empty-counters slow-counters (find-num-new minutes ( append fast-counters slow-counters) )) mylist)]
        [(list name n-items)(serve-helper (cdr requests)
        (if (and (<= (cdr(min-nclose fast-counters (cons -1 1000))) (cdr(min-nclose slow-counters (cons -1 1000)))) (<= n-items ITEMS))
            (update (add-to-counter name n-items) fast-counters (car(min-nclose fast-counters (cons -1 1000)))) fast-counters)
        (if (or (> n-items ITEMS) (> (cdr(min-nclose fast-counters (cons -1 1000))) (cdr(min-nclose slow-counters (cons -1 1000)))))  
            (update (add-to-counter name n-items) slow-counters (car (min-nclose slow-counters (cons -1 1000)))) slow-counters)
        mylist
        )                  ]
        [(list 'delay index minutes) (serve-helper (cdr requests)
             (if (< index (counter-index (car slow-counters))) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) fast-counters )
             (if (>= index (counter-index (car slow-counters))) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) slow-counters ) mylist ) ]
        [minutes (serve-helper (cdr requests)  (map (pass-helper minutes) fast-counters) (map (pass-helper minutes) slow-counters)
                               (append mylist (get-all (sort (append fast-counters slow-counters) < #:key counter-et) minutes '())))])))

