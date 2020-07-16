#lang racket
(require brag/support)
(require br-parser-tools/lex)

(provide tokenize)

(define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(:: (:+ numeric) "." (:+ numeric))
        (token 'NUMBER (string->number lexeme))]
       [(repetition 1 +inf.0 numeric)
        (token 'NUMBER (string->number lexeme))]
       ["add"
        (token "add" lexeme)]
       ["void"
        (token "void" lexeme)]
       ["initSched"
        (token "initSched" lexeme)]
       ["steadySched"
        (token "steadySched" lexeme)]
       ["stateful"
        (token "stateful" lexeme)]
       ["init"
        (token "init" lexeme)]
       ["work"
        (token "work" lexeme)]
       ["print"
        (token "print" lexeme)]
       ["int"
        (token 'INT lexeme)]
       ["float"
        (token 'FLOAT lexeme)]
       ["pipeline"
        (token 'PIPELINE lexeme)]
       ["filter"
        (token 'FILTER lexeme)]
       ["splitjoin"
        (token 'SPLITJOIN lexeme)]
       ["split"
        (token "split" lexeme)]
       ["join"
        (token "join" lexeme)]
       ["duplicate"
        (token "duplicate" lexeme)]
       ["roundrobin"
        (token "roundrobin" lexeme)]
       ["fun"
        (token "fun" lexeme)]
       ["var"
        (token "var" lexeme)]
       ["if"
        (token "if" lexeme)]
       ["else"
        (token "else" lexeme)]
       ["for"
        (token "for" lexeme)]
       [(:: "\"" any-string "\"")
        (token 'STRING lexeme)]
       
       [ (union (:+ alphabetic)(:+ numeric))
        (token 'IDENTIFIER lexeme)]
       ["+"
        (token "+" lexeme)]
       ["-"
        (token "-" lexeme)]
       ["*"
        (token "*" lexeme)]
       ["/"
        (token "/" lexeme)]
       [";"
        (token ";" lexeme)]
       ["="
        (token "=" lexeme)]
       ["("
        (token "(" lexeme)]
       [")"
        (token ")" lexeme)]
       ["{"
        (token "{" lexeme)]
       ["}"
        (token "}" lexeme)]
       ["["
        (token "[" lexeme)]
       ["]"
        (token "]" lexeme)]
       [">"
        (token ">" lexeme)]
       ["<"
        (token "<" lexeme)]
       ["<="
        (token "<=" lexeme)]
       [">="
        (token ">=" lexeme)]
       ["!="
        (token "!=" lexeme)]
       ["=="
        (token "==" lexeme)]
       ["++"
        (token "++" lexeme)]
       ["--"
        (token "--" lexeme)]
       [","
        (token "," lexeme)]
       ["->"
        (token "->" lexeme)]
       [(:: "//" (complement (:: (:* any-char) "\n" (:* any-char))) (:or "\n" ""))
        (token "//" lexeme #:skip? #t)]
       [whitespace
        (token 'WHITESPACE lexeme #:skip? #t)]
       [(eof)
        (void)]))
    (define (next-token) (my-lexer ip))
    next-token)
