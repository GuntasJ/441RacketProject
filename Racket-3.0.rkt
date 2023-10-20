#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require data/either)

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;This is to count the newlines for the error printing
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

(define nl-count 1)

(define (inc-nl-count!)
  (set! nl-count (+ nl-count 1)))
              
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;This is to make tokens of the input. This greatly simplifies the parsing that needs to be done
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(define create-tokens-from-input
  (lexer
   ;end of file
   [(eof) '()]

   ;reserved words
   ;if
   ["if"
    (cons `(IF, nl-count) (create-tokens-from-input input-port))]
   
   ;while
   ["while"
    (cons `(WHILE, nl-count) (create-tokens-from-input input-port))]
   
   ;read
   ["read"
    (cons `(READ, nl-count) (create-tokens-from-input input-port))]

   ;write
   ["write"
    (cons `(WRITE, nl-count) (create-tokens-from-input input-port))]

   ;goto
   ["goto"
    (cons `(GOTO, nl-count) (create-tokens-from-input input-port))]

   ;gosub
   ["gosub"
    (cons `(GOSUB, nl-count) (create-tokens-from-input input-port))]

   ;return
   ["return"
    (cons `(RETURN, nl-count) (create-tokens-from-input input-port))]

   ;break
   ["break"
    (cons `(BREAK, nl-count) (create-tokens-from-input input-port))]

   ;endwhile
   ["endwhile"
    (cons `(ENDWHILE, nl-count) (create-tokens-from-input input-port))]
   
   ;end
   ["end"
    (cons `(END, nl-count) (create-tokens-from-input input-port))]

   ;left parenthesis
   [#\(
    (cons `(LEFT_PARENTHESIS, nl-count) (create-tokens-from-input input-port))]

   ;right parenthesis
   [#\)
    (cons `(RIGHT_PARENTHESIS, nl-count) (create-tokens-from-input input-port))]
   
   ;multiplication operator
   [#\*
    (cons `(MULTIPLICATION_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;division operator
   [#\/
    (cons `(DIVISION_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;addition operator
   [#\+
    (cons `(PLUS, nl-count) (create-tokens-from-input input-port))]

   ;subtraction operator
   [#\-
    (cons `(MINUS, nl-count) (create-tokens-from-input input-port))]

   ;less than or equal
   ["<="
    (cons `(LESS_THAN_OR_EQUAL_BOOLEAN_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;not equal 
   ["<>"
    (cons `(NOT_EQUAL_BOOLEAN_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;greater than or equal
   [">="
    (cons `(GREATER_THAN_OR_EQUAL_BOOLEAN_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;less than
   [#\<
    (cons `(LESS_THAN_BOOLEAN_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;greater than
   [#\>
    (cons `(GREATER_THAN_BOOLEAN_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;equal could be boolean or assignment
   [#\=
    (cons `(EQUAL_OPERATOR, nl-count) (create-tokens-from-input input-port))]

   ;true
   ["true"
    (cons `(TRUE, nl-count) (create-tokens-from-input input-port))]

   ;false
   ["false"
    (cons `(FALSE, nl-count) (create-tokens-from-input input-port))]
   
   ;id
   [(:: (char-range #\a #\z) (:* (:or (char-range #\A #\Z) (char-range #\a #\z) (char-range #\0 #\9))))
    (cons `(ID, nl-count) (create-tokens-from-input input-port))]

   ;simple number, without the + or - before it
   ;this is so we can do x+5 as expression and not as x +5
   [(:+ (char-range #\0 #\9))
    (cons `(SIMPLE_NUMBER, nl-count) (create-tokens-from-input input-port))]
   ;semi colon
   [#\;
    (cons `(SEMI_COLON, nl-count) (create-tokens-from-input input-port))]

   ;colon
   [#\:
    (cons `(COLON, nl-count) (create-tokens-from-input input-port))]

   ;$$ end of program
   ["$$"
    (cons `(END_OF_PROGRAM, nl-count) (create-tokens-from-input input-port))]

   ;new line
   [#\newline
    (begin
      (inc-nl-count!)
      (create-tokens-from-input input-port))]

   ;white space, ignore it
   [whitespace
    (create-tokens-from-input input-port)]

   [any-char
    (cons `(ERROR, nl-count) (create-tokens-from-input input-port))]))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;This section of the code is to make chcekers for all of the tokens listed above. This is done so we do not have to work with #true or #false, only success and failure
;The empty check if a chain of functions are being passed or for end of list when it is not correct. This will ensure that it will return failure rather than a random error
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(define ($$? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'END_OF_PROGRAM) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (colon? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'COLON) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (semi-colon? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'SEMI_COLON) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (simple-number? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'SIMPLE_NUMBER) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (id? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'ID) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (f? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'FALSE) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (t? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'TRUE) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (=? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'EQUAL_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (>? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'GREATER_THAN_BOOLEAN_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (<? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'LESS_THAN_BOOLEAN_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (>=? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'GREATER_THAN_OR_EQUAL_BOOLEAN_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (<>? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'NOT_EQUAL_BOOLEAN_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (<=? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'LESS_THAN_OR_EQUAL_BOOLEAN_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (-? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'MINUS) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (+? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'PLUS) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (/? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'DIVISION_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (*? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'MULTIPLICATION_OPERATOR) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (rp? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'RIGHT_PARENTHESIS) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (lp? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'LEFT_PARENTHESIS) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (end? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'END) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (endwhile? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'ENDWHILE) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (break? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'BREAK) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (return? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'RETURN) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (gosub? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'GOSUB) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (goto? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'GOTO) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (write? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'WRITE) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (read? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'READ) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (while? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'WHILE) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (if? token-list)
  (cond
    [(empty? token-list) (failure (~a "Syntax Error at line " nl-count))]
    [(equal? (first (first token-list)) 'IF) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; This section is for helper methods. Using a custom chain function
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

(define (chain single-arg-function either-value)
  (if (failure? either-value)
      either-value
      (single-arg-function (from-success 0 either-value))))


;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; functions to check the grammar

;NOTES FOR ME
;if the condition is only to check one thing, ie any of the stuff i've defined above, then the result can be done with the rest token list in a function
;if the condition is more than one, however, then need to call that condition again in the result. This is redudent code. May fix. 

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(define (is-expression token-list)
  (displayln (~a "is-expression: " token-list))
  (cond
    [(success? (id? token-list)) (is-expression-tail (rest token-list))]
    [(success? (is-num token-list)) (chain is-expression-tail (is-num token-list))]
    [(success? (lp? token-list)) (chain rp? (is-expression (rest token-list)))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))


(define (is-expression-tail token-list)
  (displayln (~a "is-expression-tail: " token-list))
  (cond
    [(empty? token-list) (success token-list)]
    [(success? (+? token-list)) (is-expression (rest token-list))]
    [(success? (-? token-list)) (is-expression (rest token-list))]
    [(success? (*? token-list)) (is-expression (rest token-list))]
    [(success? (/? token-list)) (is-expression (rest token-list))]
    [else (success token-list)]))

(define (is-num token-list)
  (displayln (~a "is-num: " token-list))
  (cond
    [(success? (+? token-list)) (simple-number? (rest token-list))]
    [(success? (-? token-list)) (simple-number? (rest token-list))]
    [(success? (simple-number? token-list)) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (is-boolean-operator token-list)
  (displayln (~a "is-boolean-operator: " token-list))
  (cond
    [(success? (<=? token-list)) (success (rest token-list))]
    [(success? (<>? token-list)) (success (rest token-list))]
    [(success? (>=? token-list)) (success (rest token-list))]
    [(success? (<?  token-list)) (success (rest token-list))]
    [(success? (>?  token-list)) (success (rest token-list))]
    [(success? (=?  token-list)) (success (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

(define (is-boolean token-list)
  (displayln (~a "is-boolean: " token-list))
  (cond
    [(success? (t? token-list)) (success (rest token-list))]
    [(success? (f? token-list)) (success (rest token-list))]
    [(success? (is-expression token-list)) (chain is-expression (chain is-boolean-operator (is-expression token-list)))] ;note to self, may just replace with right side being returned. This would elimnate the need for else and reduce code.  
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

#|
stmt -> id = expr;
| if (boolean) stmt;
| while (boolean) linelist endwhile;
| read id;
| write expr;
| goto id;
| gosub id;
| return;
| break;
| end;
|#
(define (is-statement token-list)
  (displayln (~a "is-statement: " token-list))
  (cond
    [(success? (id? token-list)) (chain semi-colon? (chain is-expression (=? (rest token-list))))]
    [(success? (if? token-list)) (chain semi-colon? (chain is-statement (chain rp? (chain is-boolean (lp? (rest token-list))))))]
    [(success? (while? token-list)) (chain semi-colon? (chain endwhile? (chain is-line-list (chain rp? (chain is-boolean (lp? (rest token-list)))))))]
    [(success? (read? token-list)) (chain semi-colon? (id? (rest token-list)))]
    [(success? (write? token-list)) (chain semi-colon? (is-expression (rest token-list)))]
    [(success? (goto? token-list)) (chain semi-colon? (id? (rest token-list)))]
    [(success? (gosub? token-list)) (chain semi-colon? (id? (rest token-list)))]
    [(success? (return? token-list)) (semi-colon? (rest token-list))]
    [(success? (break? token-list)) (semi-colon? (rest token-list))]
    [(success? (end? token-list)) (semi-colon? (rest token-list))]
    [else (failure (~a "Syntax Error at line " (rest (first token-list))))]))

;label -> id: | epsilon
(define (is-label token-list)
  (displayln (~a "is-label: " token-list))
  (cond
    [(empty? token-list) (success token-list)]
    [(success? (chain colon? (id? token-list))) (chain colon? (id? token-list))] ; this bizare duplication is for when id is there but not : . This ensures that the returned list is accruate.
    [else (success token-list)]))

;linetail -> stmt+ | epsilon
(define (is-line-tail token-list)
  (displayln (~a "is-line-tail: " token-list))
  (cond
    [(empty? token-list) (success token-list)]
    [(success? (is-statement token-list)) (chain is-line-tail (is-statement token-list))]
    [else (success token-list)]))

;line -> label stmt linetail
(define (is-line token-list)
  (displayln (~a "is-line: " token-list))
  (chain is-line-tail (chain is-statement (is-label token-list))))

;linelist -> line linelist | epsilon
(define (is-line-list token-list)
  (displayln (~a "is-line-list: " token-list))
  (cond
    [(empty? token-list) (success token-list)]
    [(success? (chain is-line-list (is-line token-list))) (chain is-line-list (is-line token-list))]
    [else (success token-list)]))

;program -> linelist $$
(define (is-program token-list)
  (displayln (~a "is-program: " token-list))
  (chain $$? (is-line-list token-list)))

(define (parse-string input-string)
  (define token-list (create-tokens-from-input (open-input-string input-string)))
  (displayln (~a "Start of program: " token-list))
  (define end-tokens (is-program token-list))
  (set! nl-count 1)
  (if (failure? end-tokens)
      end-tokens
      (if (chain empty? end-tokens)
          "Accept"
          (failure (~a "Syntax Error at line " (rest (chain first end-tokens)))))))

(define (parse file-name)
  (define token-list (create-tokens-from-input (open-input-file file-name)))
  (displayln (~a "Start of program: " token-list))
  (define end-tokens (is-program token-list))
  (set! nl-count 1)
  (if (failure? end-tokens)
      end-tokens
      (if (chain empty? end-tokens)
          "Accept"
          (failure "Syntax Error"))))

(define input-string
"y = 3 * (x + 5);
$$


consdf
"
)

(current-directory "/Users/guntasjammu/Desktop/441-Racket-Project")

(parse-string input-string)
;(parse "Text.txt")

  



                        