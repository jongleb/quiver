; shop.scm — how quiver masks shop for test and dev copies.
; Typed against every column before anything is read. Try a function with no server:
;   quiver mask --mask-script shop.scm '(phone value)' '+7 (912) 345-67-89'

(words first-names "first-names.txt")     ; one per line beside this file, # starts a comment
(words cities "cities.txt")

(define (phone p)                         ; +7 (912) 345-67-89 → +7 (9xx) xxx-67-89
  (concat "+7 (9" (digits p 2) ") " (digits (concat p "/") 3) "-" (right p 5)))
(define (person n)                        ; Иван Петров → Мария Gzhfmpb
  (concat (pick-word n first-names) " " (title (translit (shape (after n " "))))))
(define (card c)                          ; same bank (first 6 digits), valid check digit
  (let ((body (concat (left c 6) (digits c 9)))) (concat body (luhn body))))

; Keys. One rule on each parent: every foreign key pointing at it follows,
; so joins still work and no reference points nowhere.
(permute "shop.users.id")                 ; → users.manager_id, addresses.user_id, orders.user_id
(permute "shop.users.email")              ; → orders.email (a foreign key to users.email)
(permute "shop.orders.id")                ; → items.order_id, payments.order_id
(permute "shop.payments.id" 1 1000000000) ; its own domain instead of 1 .. 2^53 − 1
(references "shop.events.user_ref" "shop.users.id")  ; no FOREIGN KEY in the schema

; Nothing of these tables leaves unless a rule decided on it: a column added
; to the schema later stops the copy with its name instead of leaking.
(strict "shop.users" "shop.payments" "shop.addresses")

(keep "shop.users.country")               ; refers to countries, which is public
(keep "shop.users.status")
(keep "shop.addresses.id")
(keep (where (table "shop.*") (type datetime)))

(mask "shop.users.name" (person value))
(mask "shop.users.phone" (phone value))
(mask "shop.users.born"                   ; the same year, another day
  (keyed-date (str (row id)) (make-date (year value) 1 1) (make-date (year value) 12 31)))
(mask "shop.users.inn" (digits value 12))
(mask "shop.users.passport" (hash-bytes value))
(mask "shop.users.salary" (round-to (keyed-noise value 20) 1000))
(mask "shop.users.note" null)
(mask "shop.payments.card" (card value))
(keep "shop.payments.amount")
(mask "shop.addresses.city" (pick-word value cities))
(mask "shop.addresses.street" (shape value))
(mask "shop.addresses.zip" (digits value 6))
(mask "shop.orders.comment" (if (contains value "@") (redact value 0 0) value))

(after-load "shop.users" "UPDATE {table} SET status = 'active' WHERE status = 'blocked'")
