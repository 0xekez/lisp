;; The list manipulation trifecta: cons, cdr, and car.

(let list (cons 1 (cons 2 (cons 3 (cons 4 ())))))

(let first (car list))
(let pop-front (cdr list))

(let second (car pop-front))

(let third (car (cdr pop-front)))
(let fourth (car (cdr (cdr pop-front))))

(add first (add second (add third fourth)))

