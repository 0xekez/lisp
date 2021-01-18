;; A hacky factorial implementation while we wait for closures to be
;; implemented.

(let fact (fn (n)
              (if (eq n 1)
                  1
                (mul n (fact (sub n 1))))))

(fact 10)
