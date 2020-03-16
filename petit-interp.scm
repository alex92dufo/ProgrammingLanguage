#! /usr/bin/env gsi -:dar

;;;Auteur: Marc Feeley
;;;Modifié et complété par: Jules Cohen (p20032498) et Alexandre Dufour (p1054564)
;;;Date: 14 décembre 2018

;;; Fichier : petit-interp.scm
;;;Description: interpréteur Scheme permettant de d'interpréter et
;;;             un programme écrit en langage procédural (C).

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction parse-and-execute
;;; doit etre definie, et vous pouvez modifier et ajouter des
;;; definitions de fonction afin de bien decomposer le traitement a
;;; faire en petites fonctions.  Il faut vous limiter au sous-ensemble
;;; *fonctionnel* de Scheme dans votre codage (donc n'utilisez pas
;;; set!, set-car!, vector-set!, list-set!, begin, print, display,
;;; etc).

;; La fonction parse-and-execute recoit en parametre une liste des
;; caracteres qui constituent le programme a interpreter.  La
;; fonction retourne une chaine de caracteres qui sera imprimee comme
;; resultat final du programme.  S'il y a une erreur lors de
;; l'analyse syntaxique ou lors de l'execution, cette chaine de
;; caracteres contiendra un message d'erreur pertinent.  Sinon, la
;; chaine de caracteres sera l'accumulation des affichages effectues
;; par les enonces "print" executes par le programme interprete.

(define parse-and-execute
  (lambda (inp)
    (parse inp execute)))

;; La fonction next-sym recoit deux parametres, une liste de
;; caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole.  La continuation
;; sera appelee avec deux parametres, la liste des caracteres restants
;; (apres le symbole analyse) et le symbole qui a ete lu (soit un
;; symbole Scheme ou une chaine de caractere Scheme dans le cas d'un
;; <id> ou un entier Scheme dans le cas d'un <int>).  S'il y a une
;; erreur d'analyse (tel un caractere inapproprie dans la liste de
;; caracteres) la fonction next-sym retourne une chaine de caracteres
;; indiquant une erreur de syntaxe, sans appeler la continuation.

(define next-sym
  (lambda (inp cont)
    (cond ((null? inp)
           (cont inp 'EOI)) ;; retourner symbole EOI a la fin de l'input
          ((blanc? (@ inp))
           (next-sym ($ inp) cont)) ;; sauter les blancs
          (else
           (let ((c (@ inp)))
             (cond ((chiffre? c)   (symbol-int inp cont))
                   ((lettre? c)    (symbol-id inp cont))
                   ((char=? c #\() (cont ($ inp) 'LPAR))
                   ((char=? c #\)) (cont ($ inp) 'RPAR))
                   ((char=? c #\{) (cont ($ inp) 'LBRA))
                   ((char=? c #\}) (cont ($ inp) 'RBRA))
                   ((char=? c #\;) (cont ($ inp) 'SEMI))
                   ((char=? c #\<) (cont ($ inp) 'LTHAN))
                   ((char=? c #\>) (cont ($ inp) 'GTHAN))
                   ((char=? c #\+) (cont ($ inp) 'ADD))
                   ((char=? c #\-) (cont ($ inp) 'SUB))
                   ((char=? c #\*) (cont ($ inp) 'MULT))
                   ((char=? c #\/) (cont ($ inp) 'DIV))
                   ((char=? c #\%) (cont ($ inp) 'MOD))
                   ((char=? c #\=) (cont ($ inp) 'EQ))
                   ((char=? c #\!) (cont ($ inp) 'EXC))

                   (else
                    (syntax-error))))))))

;; La fonction @ prend une liste de caractere possiblement vide et
;; retourne le premier caractere, ou le caractere #\nul si la liste
;; est vide.

(define @
  (lambda (inp)
    (if (null? inp) #\nul (car inp))))

;; La fonction $ prend une liste de caractere possiblement vide et
;; retourne la liste des caracteres suivant le premier caractere s'il
;; y en a un.

(define $
  (lambda (inp)
    (if (null? inp) '() (cdr inp))))

;; La fonction syntax-error retourne le message d'erreur indiquant une
;; erreur de syntaxe.

(define syntax-error
  (lambda ()
    "syntax error\n"))

;; La fonction blanc? teste si son unique parametre est un caractere
;; blanc.

(define blanc?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\return) (char=? c #\tab))))

;; La fonction chiffre? teste si son unique parametre est un caractere
;; numerique.

(define chiffre?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

;; La fonction lettre? teste si son unique parametre est une lettre
;; minuscule.

(define lettre?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

;; La fonction symbol-int recoit deux parametres, une liste de
;; caracteres qui debute par un chiffre et une continuation.  La liste
;; de caracteres sera analysee pour en extraire le symbole <int>.  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole <int> analyse et le symbole
;; <int> qui a ete lu (un entier Scheme qui est la valeur numerique du
;; symbole <int>).

(define symbol-int
  (lambda (inp cont)
    (symbol-int-aux inp cont 0)))

(define symbol-int-aux
  (lambda (inp cont n)
    (if (chiffre? (@ inp))
        (symbol-int-aux ($ inp)
                        cont
                        (+ (* 10 n) (- (char->integer (@ inp)) 48)))
        (cont inp n))))

;; La fonction symbol-id recoit deux parametres, une liste de
;; caracteres qui debute par une lettre minuscule et une continuation.
;; La liste de caracteres sera analysee pour en extraire le prochain
;; symbole (soit un mot cle comme "print" ou un <id>).  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole analyse et le symbole qui a
;; ete lu (soit un symbole Scheme, comme PRINT-SYM, ou une chaine de
;; caracteres Scheme qui correspond au symbole <id>).

(define symbol-id
  (lambda (inp cont)
    (symbol-id-aux inp cont '())))

(define symbol-id-aux
  (lambda (inp cont lst)
    (if (lettre? (@ inp))
        (symbol-id-aux ($ inp) cont (cons (@ inp) lst))
        (let ((id (list->string (reverse lst))))
          (cond ((string=? id "print")
                 (cont inp 'PRINT-SYM))
                ((string=? id "if")
                 (cont inp 'IF-SYM))
                ((string=? id "else")
                 (cont inp 'ELSE-SYM))
                ((string=? id "while")
                 (cont inp 'WHILE-SYM))
                ((string=? id "do")
                 (cont inp 'DO-SYM))
                (else
                 (cont inp id)))))))

;; La fonction expect recoit trois parametres, un symbole, une liste
;; de caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole qui doit etre le meme
;; que le premier parametre de la fonction.  Dans ce cas la
;; continuation sera appelee avec un parametre, la liste des
;; caracteres restants apres le symbole analyse.  Si le prochain
;; symbole n'est pas celui qui est attendu, la fonction expect
;; retourne une chaine de caracteres indiquant une erreur de syntaxe.

(define expect
  (lambda (expected-sym inp cont)
    (next-sym inp
              (lambda (inp sym)
                (if (equal? sym expected-sym)
                    (cont inp)
                    (syntax-error))))))


;; La fonction parse recoit deux parametres, une liste de caracteres
;; et une continuation.  La liste de caracteres sera analysee pour
;; verifier qu'elle est conforme a la syntaxe du langage.  Si c'est le
;; cas, la continuation sera appelee avec une S-expression qui
;; represente l'ASA du programme.  Sinon la fonction parse retourne
;; une chaine de caracteres indiquant une erreur de syntaxe.

(define parse
  (lambda (inp cont)
    (<program> inp ;; analyser un <program>
               (lambda (inp program)
                 (expect 'EOI ;; verifier qu'il n'y a rien apres
                         inp
                         (lambda (inp)
                           (cont program)))))))

;; Les fonctions suivantes, <program>, <stat>, ... recoivent deux
;; parametres, une liste de caracteres et une continuation.  La liste
;; de caracteres sera analysee pour verifier qu'elle debute par une
;; partie qui est conforme a la categorie correspondante de la
;; grammaire du langage.  Si c'est le cas, la continuation sera
;; appelee avec deux parametres : une liste des caracteres restants du
;; programme et une S-expression qui represente l'ASA de ce fragment
;; de programme.  Sinon ces fonctions retournent une chaine de
;; caracteres indiquant une erreur de syntaxe.

(define <program>
  (lambda (inp cont)
    (<stat> inp cont))) ;; analyser un <stat>

(define <stat>
  (lambda (inp cont)
    (next-sym inp
              (lambda (inp2 sym)
                (case sym ;; determiner quel genre de <stat>

                  ((PRINT-SYM)
                   (<print_stat> inp2 cont))

                  ((IF-SYM)
                   (<if_stat> inp2 cont))

                  ((WHILE-SYM)
                   (<while_stat> inp2 cont))

                  ((DO-SYM)
                   (<do_stat> inp2 cont))

                  ((LBRA)
                   ( <seq_stat> inp2 cont))

                  ((SEMI)
                   (cont inp2 (list 'EMPTY)));;Si on a un ; seule, c'est un statement considéré vide.

                  
                  (else
                   (<expr_stat> inp cont)))))))

(define <seq_stat> ;;Permet une séquence de plusieurs statement
    (lambda (inp cont)
      (<stat> inp ;; La séquence debut necessairement avec un statement
        (lambda(inp stat1)
          (next-sym inp
            (lambda (inp2 sym)
              (if (equal? sym 'RBRA) ;; fin de la sequence
                  (cont inp2
                    (list 'SEQ stat1 '(EMPTY)))               
                  (<seq_stat> inp              ;;Imbrication de statement dans une même séquence
                    (lambda (inp stat2)
                     (cont inp
                         (list 'SEQ stat1 stat2 )))))))))))

(define <if_stat>
  (lambda(inp cont)
    (<paren_expr> inp ;; analyser un <expr>
            (lambda (inp expr)
                  (<stat> inp
                    (lambda (inp stat)
                      (next-sym inp
                        (lambda (inp2 sym)
                          (if (equal? sym 'ELSE-SYM) ;;cas de if... else
                             (<stat> inp2
                                (lambda (inp stat2)
                                 (cont inp
                                     (list 'ELIF expr stat stat2 )))) ;; if... else
                              (cont inp
                                (list 'IF expr stat)))))))))));; if


(define <do_stat>
  (lambda(inp cont)
    (<stat> inp ;; analyser un <expr>
            (lambda (inp stat)
              (expect 'WHILE-SYM ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (<paren_expr> inp
                                    (lambda (inp expr)
                                        (expect 'SEMI ;; verifier qu'il y a ";" apres
                                                inp
                                                (lambda (inp)
                                                  (cont inp
                                                        (list 'DO stat expr))))))))))))                                      

(define <while_stat>
  (lambda(inp cont)
    (<paren_expr> inp 
            (lambda (inp expr)
                  (<stat> inp
                    (lambda (inp stat)
                       (cont inp
                            (list 'WHILE expr stat))))))))

(define <print_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;; analyser un <paren_expr>
                  (lambda (inp expr)
                    (expect 'SEMI ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (cont inp
                                    (list 'PRINT expr))))))))

(define <paren_expr>
  (lambda (inp cont)
    (expect 'LPAR ;; doit debuter par "("
            inp
            (lambda (inp)
              (<expr> inp ;; analyser un <expr>
                      (lambda (inp expr)
                        (expect 'RPAR ;; doit etre suivi de ")"
                                inp
                                (lambda (inp)
                                  (cont inp
                                        expr)))))))))

(define <expr_stat>
  (lambda (inp cont)
    (<expr> inp ;; analyser un <expr>
            (lambda (inp expr)
              (expect 'SEMI ;; doit etre suivi de ";"
                      inp
                      (lambda (inp)
                        (cont inp
                              (list 'EXPR expr))))))))

(define <expr>
  (lambda (inp cont)
    (next-sym inp ;; verifier 1e symbole du <expr>
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <expr>
                          (lambda (inp3 sym2)
                            (next-sym inp3 ;; verifier 3e symbole du <expr>
                              (lambda (inp4 sym3)                                    ;; combinaison "id =" ?
                                  (if (and (not(equal? sym3 'EQ))(and (string? sym1) ;; si le symbole apres le "=" 
                                           (equal? sym2 'EQ)))                       ;; n'est pas un "="
                                      (<expr> inp3
                                              (lambda (inp expr)
                                                (cont inp
                                                      (list 'ASSIGN
                                                            sym1
                                                            expr))))
                                      (<test> inp cont))))))))))

(define <test>
  (lambda (inp cont)
    (<sum> inp 
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <test>
                          (lambda (inp3 sym2)
                            (next-sym inp3 ;; verifier 3e symbole du <test> 
                              (lambda (inp4 sym3)
                                    (cond ((equal? sym2 'LTHAN) ;; test pour <
                                            (if (equal? sym3 'EQ) ;; test pour <=
                                              (<sum> inp
                                                (lambda (inp sum)
                                                  (<sum> inp4
                                                    (lambda (inp sum2)
                                                      (cont inp
                                                           (list 'LTE
                                                                  sum
                                                                  sum2))))))
                                               
                                                (<sum> inp
                                                  (lambda (inp sum)
                                                    (<sum> inp3
                                                      (lambda (inp sum2)
                                                        (cont inp
                                                             (list 'LT
                                                                    sum
                                                                    sum2))))))))
                                          ((equal? sym2 'GTHAN) ;; test pour >
                                            (if (equal? sym3 'EQ) ;; test pour >=
                                              (<sum> inp
                                                (lambda (inp sum)
                                                  (<sum> inp4
                                                    (lambda (inp sum2)
                                                      (cont inp
                                                           (list 'GTE
                                                                  sum
                                                                  sum2))))))
                                                (<sum> inp
                                                  (lambda (inp sum)
                                                    (<sum> inp3
                                                      (lambda (inp sum2)
                                                        (cont inp
                                                             (list 'GT
                                                                    sum
                                                                    sum2))))))))
                                          ((equal? sym2 'EQ) ;; test pour =
                                            (if (equal? sym3 'EQ) ;; test pour ==
                                              (<sum> inp
                                                (lambda (inp sum)
                                                  (<sum> inp4
                                                    (lambda (inp sum2)
                                                      (cont inp
                                                           (list 'EQ
                                                                  sum
                                                                  sum2))))))
                                             (syntax-error)))

                                           ((equal? sym2 'EXC) ;; test pour !
                                            (if (equal? sym3 'EQ) ;; test pour !=
                                              (<sum> inp
                                                (lambda (inp sum)
                                                  (<sum> inp4
                                                    (lambda (inp sum2)
                                                      (cont inp
                                                           (list 'NEQ
                                                                  sum
                                                                  sum2))))))
                                             (syntax-error)))
                                         (else
                                            (<sum> inp cont)))))))))))   

(define <sum>
  (lambda (inp cont)
    (<mult>  inp
    (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <sum>
                          (lambda (inp3 sym2)
                            (if (or (equal? sym2 'ADD)(equal? sym2 'SUB)) ;; test pour + ou -, s'il s'agit d'un calcul
                                     (<mult> inp
                                        (lambda (inp sum)
                                          (<sum> inp3
                                            (lambda (inp mult)
                                              (cont inp
                                                   (list sym2
                                                          sum
                                                          mult))))))
                                    (<mult> inp cont))))))))
  
(define <mult>
  (lambda (inp cont)
    (<term> inp
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <mult>
                          (lambda (inp3 sym2)
                                (if (assoc sym2 opersMult) ;; associe le symbole avec               
                                     (<term> inp           ;;un des symboles de opersMult
                                        (lambda (inp term2)
                                          (<mult> inp3
                                            (lambda (inp mult)
                                              (cont inp
                                                   (let ((fn (cdr (assoc sym2 opersMult))))
                                   (fn term2 mult)))))))
                                
                                    (<term> inp cont))))))))

;; associe les symboles * , \  et % au noeud à creer

(define opersMult   
  (list             
        (cons 'MULT (lambda (x y) (list 'MULT x y)))
        (cons 'DIV (lambda (x y ) (list 'DIV x y)))
        (cons 'MOD (lambda (x y ) (list 'MOD x y)))))

;;Vérifie si le nombre est inférieur à zéro: cas de division par zéro
(define zero
  (lambda (x cont)
      (cond ((eq? x 0) 
              (zero-error))
          (else
          (cont x)))))

(define zero-error
  (lambda ()
    "Arithmetic-error -- Division by zero\n"))

(define <term>
  (lambda (inp cont)
    (next-sym inp ;; verifier le premier symbole du <term>
              (lambda (inp2 sym)
                (cond ((string? sym) ;; identificateur?
                       (cont inp2 (list 'VAR sym)))
                      ((number? sym) ;; entier?
                       (cont inp2 (list 'INT sym)))
                      (else
                       (<paren_expr> inp cont)))))))

;; La fonction execute prend en parametre l'ASA du programme a
;; interpreter et retourne une chaine de caracteres qui contient
;; l'accumulation de tout ce qui est affiche par les enonces "print"
;; executes par le programme interprete.

(define execute
  (lambda (ast)
    (exec-stat '() ;; etat des variables globales
               ""  ;; sortie jusqu'a date
               ast ;; ASA du programme
               (lambda (env output)
                 output)))) ;; retourner l'output pour qu'il soit affiche

;; La fonction exec-stat fait l'interpretation d'un enonce du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'enonce a interpreter et une continuation.  La continuation sera
;; appelee avec deux parametres : une liste d'association donnant la
;; valeur de chaque variable du programme apres l'interpretation de
;; l'enonce et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'enonce.

(define exec-stat
  (lambda (env output ast cont)
    (case (car ast)

      ((PRINT)
       (exec-expr env ;; evaluer l'expression du print
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env 
                          (string-append output  ;; ajouter le resultat a la sortie
                                         (number->string val)
                                         "\n")))))

      ((SEQ)            ;;exécute une séquence de statement
        (exec-stat env 
                   output 
                   (cadr ast)  
                   (lambda (env output)
                           (exec-stat env 
                               output 
                               (caddr ast)  
                               (lambda (env output)
                                       (cont env output))))))

       ((IF)
       (exec-expr env 
              output  
                (cadr ast) 
                (lambda (env output val )
                     (if val                ;; si val vrai
                          (exec-stat env    ;; on execute ce statement sinon on passe à la suite
                                    output 
                                    (caddr ast)  
                                    (lambda (env output)
                                        (cont env output)))
                          (cont env output)))))

       ((ELIF)
        (exec-expr env 
              output  
                (cadr ast) 
                (lambda (env output val )
                     (if val                ;; si val vrai
                          (exec-stat env    ;; on execute ce statement
                                    output 
                                    (caddr ast)  
                                    (lambda (env output)
                                        (cont env output)))
                          (exec-stat env    ;; sinon celui-ci
                                    output 
                                    (cadddr ast)  
                                    (lambda (env output)
                                        (cont env output)))))))

      ((WHILE)
         (exec-expr env   ;;calcul de la valeur de condition de fin
              output  
                (cadr ast) 
                (lambda (env output val )
                     (if val                  ;; si val est est vrai
                          (exec-stat env      ;; on exécute le statement puis on recommence
                                    output    ;; sinon on passe à la suite
                                    (caddr ast)  
                                    (lambda (env output)
                                        (exec-stat env 
                                            output 
                                            ast 
                                            (lambda (env output)
                                                (cont env output)))))
                          (cont env output)))))

      ((DO)
        (exec-stat env  ;;on execute ce statement
            output 
            (cadr ast)  
            (lambda (env output)
                 (exec-expr env   ;;calcul de la valeur de condition de fin
                      output  
                        ( caddr ast)
                        (lambda (env output val )
                             (if val               ;; si val est est vrai, on réexécute le premier statement
                                  (exec-stat env   ;; sinon on passe à la suite
                                            output 
                                            (cadr ast)  
                                            (lambda (env output)
                                                (exec-stat env 
                                                    output 
                                                    ast 
                                                    (lambda (env output)
                                                        (cont env output)))))
                                  (cont env output)))))))

      ((EXPR)
       (exec-expr env ;; evaluer l'expression
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env output)))) ;; continuer en ignorant le resultat

      ((EMPTY)
        (cont env output))

      (else
       "internal error (unknown statement AST)\n"))))

;; La fonction exec-expr fait l'interpretation d'une expression du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'expression a interpreter et une continuation.  La continuation
;; sera appelee avec deux parametres : une liste d'association donnant
;; la valeur de chaque variable du programme apres l'interpretation de
;; l'expression et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'expression.

(define exec-expr
  (lambda (env output ast cont)
    (cond ((equal? (car ast) 'INT)
             (cont env
                   output
                   (cadr ast))) ;; retourner la valeur de la constante


            ((and (pair? ast) (equal?(car ast) 'ASSIGN))
             (exec-expr env 
                    output  
                      (caddr ast) 
                      (lambda (env output val )
                        (cont (cons (cons (cadr ast) val) env) output val))));;Ajoute à l'avant de l'environnement une liste contenant la variable et sa valeur
            

            ((equal? (car ast) 'VAR)
              (if (assoc (cadr ast) env)
                        (cont env output (cdr(assoc (cadr ast) env)));;Cherche la variable dans l'environnemet
                  (cont env output 0))) ;; Si la variable n'est pasdans l'environnement, renvoit 0.
                        
          

           ((and (pair? ast) (assoc (car ast) opers));; Calcul ou comparaison arithmétique
             (exec-expr env 
                    output  
                      (cadr ast) 
                      (lambda (env output val )
                           (exec-expr env 
                              output  
                                (caddr ast) 
                                (lambda (env output val2 )
                                  (if (or (equal? (car ast) 'DIV)(equal? (car ast) 'MOD))
                                    (zero val2 ;;Vérifie la division par zéro
                                      (lambda (val2)
                                        (cont env output (let ((fn (cdr (assoc (car ast) opers))))
                                         (fn val val2)))))
                                    (cont env output (let ((fn (cdr (assoc (car ast) opers))))
                                         (fn val val2)))))))))
      (else
       "internal error (unknown expression AST)\n"))))

   ;; pour associer le symbole à l'opération à effectuer.
  (define opers
  (list (cons 'ADD (lambda (x y) (+ x y)))
        (cons 'SUB (lambda (x y) (- x y)))
        (cons 'MULT (lambda (x y) (* x y)))
        (cons 'DIV (lambda (x y) (quotient x y)))
        (cons 'MOD (lambda (x y) (remainder x y)))
        (cons 'EQ (lambda (x y) (equal? x y)))
        (cons 'NEQ (lambda (x y) (not(equal? x y))))
        (cons 'LT (lambda (x y) (< x y)))
        (cons 'LTE (lambda (x y) (or (< x y) (equal? x y))))
        (cons 'GT (lambda (x y) (> x y)))
        (cons 'GTE (lambda (x y) (or (> x y) (equal? x y))))))
;;;----------------------------------------------------------------------------

;;; *** NE MODIFIEZ PAS CETTE SECTION ***

(define main
  (lambda ()
    (print (parse-and-execute (read-all (current-input-port) read-char)))))
    
;;;----------------------------------------------------------------------------
