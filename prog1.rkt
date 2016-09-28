#lang racket

#|

Jack Riales
Programming Language Theory - CS333

Programming Assignment 1 - Scheme Calls Database

Usage:

Create entries using (db_insert [record])
Record format: list starting with a name followed by lists containing (LOCATION DURATION RATE)
Fair warning: Do not quote ( ' ) inside the record list. The functions won't work correctly!
example: (db_insert '( Jack (AL 10 3) (FL 13 5) ...))

Usability functions --
Search and return records by name using (db_search '[name])
bill_amt returns the total amount owed by a customer (bill_amt '[name])
calls returns all calls made (calls '[name])
total_minutes returns the total duration of all calls made (total_minutes '[name])
most_expensive returns the most expensive phone charge (most_expensive '[name])
short_calls returns the number of calls made that took less time than the time given (short_calls '[name] [length])

 |#

; Actual calls db
(define db '())

; [Helper] Returns if the given name matches the given record in the db
(define (db_match_record name record)
  (if (equal? name (car record)) #t #f)
  )

; [Helper] Searches out and returns the record using a name
; Usage: (search 'Jack db)
(define (db_search name db)
  (cond
    [(null? name) #f]
    [(null? db) #f]
    [(not (list? db)) #f]
    [(empty? db) '()]
    [else
     (let ((tmp_db db))
       (if (db_match_record name (car tmp_db)) (car tmp_db) (db_search name (cdr tmp_db))))
     ]
    )
  )

; [Helper] Returns the number of minutes a call took
(define (call_duration call)
  (car (cdr call))
  )

; [Helper] Returns a call's rate
(define (call_rate call)
  (car (cdr (cdr call)))
  )

; [Helper] Calculates rate x duration of a call
; The duration of the call is the second part of the list (car (cdr call)),
; The rate is the third. (car (cdr (cdr call)))
(define (call_price call)
  (* (call_duration call) (call_rate call))
  )

; [Helper] Gathers all the call records for a name and runs a callback for each of them
(define (map_call_record name function)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [else (map function (cdr record))]
      )
    )
  )

; Inserts a call into the db
; Format: (cust-name (location duration rate))
; Usage: (db_insert (Jack (CA 56 23) (FL 58 23) ...)
; Check using: (display db) or (db)
(define (db_insert record)
  (cond
    [(null? record) #f]
    [else
     (set! db (append db (list record)))
     (length db)
     ]
    )  
  )

; Searches for a customer and returns the calls that they made
(define (calls name)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [else (cdr record)]
      )
    )
  )

; Searches for a customer and returns the total amount they owe
(define (bill_amt name)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [else (apply + (map call_price (cdr record)))]
      )
    )
  )

; Searches for a customer and returns the total minutes that they've used
(define (total_minutes name)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [else (apply + (map call_duration (cdr record)))]
      )
    )
  )

; Returns the most expensive phone charge on the list for a given name
(define (most_expensive name)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [else (apply max (map call_price (cdr record)))]
      )
    )
  )

; Returns the number of calls that were made that took less than the given time
(define (short_calls name length)
  (let ((record (db_search name db)))
    (cond
      [(not record) #f]
      [(empty? record) 0]
      [(= 0 length) 0]
      [else
       (let [(call_num 0)]
         
         ; Get all call lengths
         (let [(calls (map call_duration (cdr record)))]
           (map (lambda (x)
                  (if (< x length)
                      (set! call_num (+ 1 call_num)) (+ 0 call_num)))
                calls)
           
           ; Return the call num
           call_num
           ))]
      )
    )
  )