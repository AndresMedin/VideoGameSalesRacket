#lang racket

(require csv-reading)
(require data/maybe)

; Prompts the user with a message and reads their input
(define (prompt-and-read message)
  (display message)
  (read-line (current-input-port) 'any))

; Sorts a list of video games by their rating, in descending order
(define (order-by-rating videogames)
  (sort videogames (lambda (game1 game2) (string>? (list-ref game1 12) (list-ref game2 12)))))

; Prints detailed sales information for a single video game
(define (print-game-details index game)
  (fprintf (current-output-port) "~a. ~a: \n Worldwide_______$~a million~n North America___$~a million~n Europe__________$~a million~n Japan___________$~a million~n Other___________$~a million~n~n"
           (+ index 1)
           (third game)
           (second (reverse game))
           (eighth game)
           (ninth game)
           (tenth game)
           (list-ref game 10)))

; Iterates over a list of video games and prints their details (Sorting by sales not implemented here)
(define (list-sorted-by-sales videogames region) 
  (for ([game videogames])
    (print-game-details (index-of videogames game) game)))

; Prints video games sorted by their rating and includes detailed stats
(define (list-sorted-by-rating videogames region)
  (for ([game (order-by-rating videogames)])
    (fprintf (current-output-port) "~a. ~a - Rating: ~a/100~n" 
             (+ (index-of (order-by-rating videogames) game) 1)
             (third game)
             (last game))
    (fprintf (current-output-port) " ..........STATS..........\n ")
    (if (null? region)
        (fprintf (current-output-port) "Worldwide___$~a million~n Platform____~a~n Year________~a~n Genre_______~a~n~n" (second (reverse game)) (fourth game) (fifth game) (sixth game))
        (fprintf (current-output-port) "~a ___$~a million~n Platform____~a~n Year________~a~n Genre_______~a~n~n" (first region) (list-ref game (second region)) (fourth game) (fifth game) (sixth game)))))

; Returns the index corresponding to a specific geographical region in the game data list
(define (determine-region-index location)
  (cond [(equal? location "North America") 7]
        [(equal? location "Europe") 8]
        [(equal? location "Japan") 9]
        [(equal? location "Other Territories") 10]))


; Filters video games by title
(define (search-by-title videogames title)
  (filter (lambda (game) (string-contains? (string-foldcase (caddr game)) (string-foldcase title))) videogames))

; Filters video games within a specified year range
(define (search-by-year videogames years)
  (filter (lambda (game) (and (string>=? (car (cddddr game)) (first years))
                              (string<=? (car (cddddr game)) (second years))))
          videogames))

; Filters video games by the publisher's name
(define (search-by-publisher videogames publisher)
  (filter (lambda (game) (string-ci=? (caddr (cddddr game)) publisher)) videogames))

; Filters video games by market (Incorrectly named, it filters by title)
(define (search-by-market videogames market)
  (filter (lambda (game) (string-ci=? (caddr game) market)) videogames))

; Filters video games by genre
(define (search-by-genre videogames genre)
  (filter (lambda (game) (string-ci=? (cadr (cddddr game)) genre)) videogames))

; Prompts the user for a start and end year, returning a list with these two values
(define (capture-year-range)
  (list (prompt-and-read "Start Year (Oldest Game Year: 1983): ")
        (prompt-and-read "End Year (Newest Game Year: 2018): ")))

; Converts a user-specified location into a format suitable for filtering
(define (search-for-location location)
  (list location (determine-region-index location)))

; Prompts the user to choose between sorting by sales or rating, then displays the filtered and sorted video game list
(define (show-filtered-results videogames [location '()])
  (display "Sort by sales or by rating?:\n1) Sales\n2) Rating\n")
  (let ([choice (prompt-and-read "")])
    (cond
     [(string-ci=? choice "1") (list-sorted-by-sales videogames location)]
     [(string-ci=? choice "2") (list-sorted-by-rating videogames location)]
     [else (displayln "Invalid option") (show-filtered-results videogames)])))

; Provides a menu for the user to apply various filters to the video game list
(define (query-menu)
  (let recur ([filters '()])
    (if (= (length filters) 6) filters 
        (let ([choice (prompt-and-read "     Choose:\n 1) -- Game Title\n 2) -- Year\n 3) -- Publisher\n 4) -- Region\n 5) -- Genre\n Q) -- Quit\n")]
              )
      (cond
        [(string-ci=? choice "1") (recur (append filters (list "Game Title" (prompt-and-read "Game Title:\n"))))]
        [(string-ci=? choice "2") (recur (append filters (list "Year" (capture-year-range))))]
        [(string-ci=? choice "3") (recur (append filters (list "Publisher" (prompt-and-read "Publisher name:\n"))))]
        [(string-ci=? choice "4") (recur (append filters (list "Region" (search-for-location (prompt-and-read "Region:\n")))))]
        [(string-ci=? choice "5") (recur (append filters (list "Genre" (prompt-and-read "Game genre:\n"))))]
        [(string-ci=? choice "Q") filters]
        [else (displayln "Invalid") (recur (filters))])))))

; Applies a series of filters to the video game list based on criteria specified
(define (apply-filters criteria-list videogames) 
  (if (null? criteria-list) nothing
      (let recursively ([criteria criteria-list]
                        [filtered-videogames videogames]
                        [location '()])
        (cond
          [(null? criteria) (show-filtered-results filtered-videogames location)]
          [(equal? (first criteria) "Game Title")
           (recursively (cdr (cdr criteria)) (search-by-title filtered-videogames (second criteria)) location)]
          [(equal? (first criteria) "Year")
           (recursively (cdr (cdr criteria)) (search-by-year filtered-videogames (second criteria)) location)]
          [(equal? (first criteria) "Publisher")
           (recursively (cdr (cdr criteria)) (search-by-publisher filtered-videogames (second criteria)) location)]
          [(equal? (first criteria) "Region")
           (recursively (cdr (cdr criteria)) filtered-videogames (second criteria))]
          [(equal? (first criteria) "Genre")
           (recursively (cdr (cdr criteria)) (search-by-genre filtered-videogames (second criteria)) location)]
          ))))

; Consolidating setup and startup logic into one function
(define (init-and-start)
  (define file-path "Video Games Sales.csv")
  (define csv-reader-options '((strip-leading-whitespace? . #t)
                               (strip-trailing-whitespace? . #t)))
  (define csv-reader (make-csv-reader-maker csv-reader-options))
  (define list-of-games (csv->list (csv-reader (open-input-file file-path))))
  ; Main function to start the program, uses list-of-games as an argument
  (let loop ([result (apply-filters (query-menu) list-of-games)])
    (if (equal? result nothing)
        (display "\n~Closed~")
        (loop (apply-filters (query-menu) list-of-games)))))

; Call init-and-start to consolidate setup and start the program
(init-and-start)