;;;;;;;;;;;;;;;;;;;;;;;; Seletores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun linha (index tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   ((equal index 0) (car tabuleiro))
   (T (linha (- index 1) (cdr tabuleiro)))
   ))

(defun celula (x y tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   (T (labels ((atomo (i l)
       (cond
        ((NULL l) NIL)
        ((equal i 0) (car l))
        (T (atomo (- i 1) (cdr l)))
        ))) (atomo y (linha x tabuleiro))))
   ))

;;;;;;;;;;;;;;;;;;;;;;;; Construção tabuleiro ;;;;;;;;;;;;;;;;;;;;;;;;
(defun lista-numeros(&optional (n 100))
  "Recebe um número positivo n e cria uma lista com todos os números entre 0 e n"
  (cond
   ((< n 0) nil)
   ((equal n 1) ( cons (- n 1) NIL))
   (T (cons (- n 1) (lista-numeros (- n 1))))
   )
)

(defun baralhar(lista)
  "Baralha uma lista de números"
  (cond
   ((NULL lista) NIL)
   (T (let ((n (nth (random (length lista)) lista)))
        (cons n (baralhar (remover-se #'(lambda (x) (= x n)) lista)))
        )
   )))

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
  "Gera um tabuleiro aleatorio"
  (cond
   ((null lista) nil)
   (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;; Posicionamento ;;;;;;;;;;;;;;;;;;;;;;;;
(defun substituir-posicao(x lista &optional (valor NIL))
  "Substituir a posição x da lista pela variavel valor"
  (cond
   ((NULL lista) NIL)
   ((equal x 0) (cons valor (cdr lista)))
   (T (cons (car lista) (substituir-posicao (- x 1) (cdr lista) valor)))
   ))

(defun substituir(x y tabuleiro &optional (valor NIL))
  "Substituir a posição (x y) da matriz pela variavel valor"
  (cond 
   ((or (NULL tabuleiro)) NIL)
   ((equal x 0) (cons (substituir-posicao y (car tabuleiro) valor) 
                               (substituir (- x 1) y (cdr tabuleiro) valor)))
   (T (cons (car tabuleiro) (substituir (- x 1) y (cdr tabuleiro) valor)))
   ))

(defun obter-posicao(x y tabuleiro)
  "Verifica se uma posição está dentro dos limites do tabuleiro e depois retorna o seu valor."
  (if (AND (>= x 0) (<= x (1- (length tabuleiro))) (>= y 0) (<= y (1- (length tabuleiro))) (NOT (NULL tabuleiro)))
      (nth y (nth x tabuleiro))
    NIL))

(defun obter-valor-linha(valor linha &optional (y 0))
  (cond
   ((NULL linha) NIL)
   ((equal valor (car linha)) y)
   (T (obter-valor-linha valor (cdr linha) (1+ y)))
   )
)

(defun obter-valor(valor tabuleiro &optional (x 0))
  (let ((y (obter-valor-linha valor (car tabuleiro))))
    (cond
     ((NULL tabuleiro) NIL)
     ((NULL y) (obter-valor valor (cdr tabuleiro) (1+ x)))
     (T (cons x (cons y nil)))
     )
    )
)

(defun posicao-cavalo(tabuleiro)
  "Verifica se o cavalo está no tabuleiro e caso esteja retorna a posição do mesmo"
  (cond
   ((NULL tabuleiro) NIL)
   ((NULL (find T (car tabuleiro))) (posicao-cavalo (cdr tabuleiro)))
   (T (list (- (length (car tabuleiro)) (length tabuleiro)) (- (length (car tabuleiro)) (length (member T (car tabuleiro))))))
  ))

;;;;;;;;;;;;;;;;;;;;;;;; Funções auxiliares aos operadores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun numero-para-lista(n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun maior-duplo-linha(linha &optional (max-duplo NIL))
  (let ((number-list (numero-para-lista (car linha))))
    (cond
     ((NULL linha) max-duplo)
     ((AND (NULL max-duplo) (NOT (EQUAL T (car linha))) (equal (car number-list) (cadr number-list))) (maior-duplo-linha (cdr linha) (car linha)))
     ((AND (NOT ( NULL max-duplo)) (NOT ( NULL (car linha)))
           (NOT (EQUAL T max-duplo)) (NOT (EQUAL T (car linha)))
           (equal (car number-list) (cadr number-list)) (< max-duplo (car linha))) 
      (maior-duplo-linha (cdr linha) (car linha)))
     (T (maior-duplo-linha (cdr linha) max-duplo))
     )
    ))

(defun maior-duplo(tabela &optional (max-duplo NIL))
  (let ((max-linha (maior-duplo-linha (car tabela))))
    (cond 
     ((NULL tabela) max-duplo)
     ((AND (NULL max-duplo) (NOT (NULL max-linha)) (NOT (EQUAL T max-linha))) (maior-duplo (cdr tabela) max-linha))
     ((AND (NOT ( NULL max-duplo)) (NOT ( NULL max-linha)) 
           (NOT (EQUAL T max-duplo)) (NOT (EQUAL T max-linha)) (< max-duplo max-linha)) 
      (maior-duplo (cdr tabela) max-linha))
     (T (maior-duplo (cdr tabela) max-duplo))
     )
    )
)

(defun reverter-lista (l)
  (cond
   ((null l) '())
   (T (append (reverter-lista (cdr l)) (list (car l))))
   ))

(defun posicao-simetrico-linha(n linha &optional (y 0))
  (let* ((numero-lista (numero-para-lista n)) (car-linha-lista (numero-para-lista (car linha))))
    (cond
     ((NULL linha) NIL)
     ((equal 1 (length numero-lista)) (cond
                                       ((equal (cons n (cons 0 nil)) car-linha-lista) y)
                                       (T (posicao-simetrico-linha n (cdr linha) (1+ y)))
                                       ))
     ((and (equal (car numero-lista) (car linha)) (equal 0 (cadr numero-lista))) y)
     ((equal (reverter-lista numero-lista) car-linha-lista) y)
     (T (posicao-simetrico-linha n (cdr linha) (1+ y)))
     )
    ))

(defun posicao-simetrico(n tabuleiro &optional (x 0))
  (let ((simetrico (posicao-simetrico-linha n (car tabuleiro))))
    (cond
     ((NULL tabuleiro) NIL)
     ((or (equal (car (numero-para-lista n)) (cadr (numero-para-lista n))) (equal n 0)) (obter-valor (maior-duplo tabuleiro) tabuleiro))
     ((NOT (NULL simetrico)) (cons x (cons simetrico nil)))
     (T (posicao-simetrico n (cdr tabuleiro) (1+ x)))
     )
    )
  )

(defun colocar-cavalo(tabuleiro)
  (let ((pos-cavalo (posicao-cavalo tabuleiro)) 
        (pos-cavalo-y (random (length tabuleiro))))
    (cond
     ((and (NULL pos-cavalo) 
           (NOT (NULL (obter-posicao 0 pos-cavalo-y tabuleiro)))
           (NOT (equal 'T (obter-posicao 0 pos-cavalo-y tabuleiro))))
      (let* ((tabuleiro1 (substituir 0 pos-cavalo-y tabuleiro T))
             (simetrico (posicao-simetrico (celula 0 pos-cavalo-y tabuleiro) tabuleiro1)))
        (cond
         ((NULL simetrico) (substituir 0 pos-cavalo-y tabuleiro T))
         (T (substituir (car simetrico) (cadr simetrico) tabuleiro1 'NIL))
         )
        ))
     ((NULL pos-cavalo) (colocar-cavalo tabuleiro))
     (T tabuleiro)
     )
    ))

;;;;;;;;;;;;;;;;;;;;;;;; Operadores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun movimentos-possiveis-menu(tabuleiro)
  "Escreve no ecrã uma lista de operadores possiveis de executar tendo em conta a posição atual do cavalo"
  (let ((pos-cavalo (posicao-cavalo tabuleiro)))
    (cond
     ((NULL pos-cavalo) (format t "O cavalo ainda não está posicionado no tabuleiro."))
     (T (progn
         (if (obter-posicao (+ (car pos-cavalo) 2) (1- (cadr pos-cavalo)) tabuleiro) (format t "~%1- Operador-1"))
         (if (obter-posicao (+ (car pos-cavalo) 2) (1+ (cadr pos-cavalo)) tabuleiro) (format t "~%2- Operador-2"))
         (if (obter-posicao (1+ (car pos-cavalo)) (+ (cadr pos-cavalo) 2) tabuleiro) (format t "~%3- Operador-3"))
         (if (obter-posicao (1- (car pos-cavalo)) (+ (cadr pos-cavalo) 2) tabuleiro) (format t "~%4- Operador-4"))
         (if (obter-posicao (- (car pos-cavalo) 2) (1+ (cadr pos-cavalo)) tabuleiro) (format t "~%5- Operador-5"))
         (if (obter-posicao (- (car pos-cavalo) 2) (1- (cadr pos-cavalo)) tabuleiro) (format t "~%6- Operador-6"))
         (if (obter-posicao (1- (car pos-cavalo)) (- (cadr pos-cavalo) 2) tabuleiro) (format t "~%7- Operador-7"))
         (if (obter-posicao (1+ (car pos-cavalo)) (- (cadr pos-cavalo) 2) tabuleiro) (format t "~%8- Operador-8"))
         ))
     )
    ))

(defun movimentos-possiveis(tabuleiro operadores)
  (cond
   ((or (NULL (posicao-cavalo tabuleiro)) (NULL operadores)) nil)
   ((NULL (funcall (car operadores) tabuleiro)) (movimentos-possiveis tabuleiro (cdr operadores)))
   (T (cons (car operadores) (movimentos-possiveis tabuleiro (cdr operadores))))
   )
)

(defun operadores()
  (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))

(defun operador-aux(inc-x inc-y tabuleiro)
  (let ((pos-cavalo (posicao-cavalo tabuleiro))) 
    (cond
     ((null pos-cavalo) (colocar-cavalo tabuleiro))
     (T (let ((nova-posicao (obter-posicao (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro)))
         (cond
          ((null pos-cavalo) (colocar-cavalo tabuleiro))
          ((OR (NULL nova-posicao) (equal 'T nova-posicao)) NIL)
          (T (let* ((tabuleiro1 (substituir (car pos-cavalo) (cadr pos-cavalo) tabuleiro 'NIL))
                    (tabuleiro2 (substituir (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro1 'T))
                    (simetrico (posicao-simetrico (celula (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro) tabuleiro2)))
               (cond
                ((NULL simetrico) tabuleiro2)
                (T (substituir (car simetrico) (cadr simetrico) tabuleiro2 'NIL))
                )
               ))
          ))))))

(defun operador-1(tabuleiro) (operador-aux 2 -1 tabuleiro))

(defun operador-2(tabuleiro) (operador-aux 2 1 tabuleiro))

(defun operador-3(tabuleiro) (operador-aux 1 2 tabuleiro))

(defun operador-4(tabuleiro) (operador-aux -1 2 tabuleiro))

(defun operador-5(tabuleiro) (operador-aux -2 1 tabuleiro))

(defun operador-6(tabuleiro) (operador-aux -2 -1 tabuleiro))

(defun operador-7(tabuleiro) (operador-aux -1 -2 tabuleiro))

(defun operador-8(tabuleiro) (operador-aux 1 -2 tabuleiro))

;;;;;;;;;;;;;;;;;;;;;;;;;;; HEURISTICA ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun append-tabuleiro(tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   (T (append (car tabuleiro) (append-tabuleiro (cdr tabuleiro))))
   )
)

(defun media-pontos(tabuleiro &optional primeira-iteracao)
  (cond
   ((NULL tabuleiro) 0.0)
   ((NULL primeira-iteracao) (let ((linha (remover-se #'(lambda (x) (or (NULL x) (equal 'T x))) (append-tabuleiro tabuleiro))))
                               (cond
                                ((or (equal 0 (length linha)) (NULL linha)) 1)
                                (T (/ (+ (car linha) (media-pontos (cdr linha) 1)) (length linha)))
                                )
                               ))
   (T (+ (car tabuleiro) (media-pontos (cdr tabuleiro) 1)))
   )
  )

(defun heuristica-base(no g-sucessor objetivo)
  (cond
   ((or (NULL no) (NULL objetivo)) NIL)
   (T (/ (- objetivo g-sucessor) (media-pontos (no-estado no))))
   )
  )

(defun heuristica-melhor(no g-sucessor objetivo)
  (cond
   ((or (NULL no) (NULL objetivo)) NIL)
   (T (+ (* (- objetivo g-sucessor) 0.4) (* (length (movimentos-possiveis (no-estado no) (operadores))) 0.6)))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;; SUCESSORES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-posicao(no sucessor)
  (let ((pos-cavalo (posicao-cavalo sucessor)))
    (cond
     ((NULL pos-cavalo) nil)
     (T (obter-posicao (car pos-cavalo) (cadr pos-cavalo) (no-estado no)))
     )
    ))

(defun novo-sucessor(no op &optional objetivo f-heuristica)
  (let* ((sucessor (construir-no (funcall op (no-estado no)) no)) (g (g-posicao no (no-estado sucessor))))
    (cond 
     ((or (NULL no) (NULL sucessor) (NULL g)) NIL)
     ((NULL f-heuristica) (construir-no (no-estado sucessor) no (+ (no-g no) g) 0))
     (T (construir-no (no-estado sucessor) no (+ (no-g no) g) (funcall f-heuristica sucessor (+ (no-g no) g) objetivo)))
     )
    )
  )

(defun sucessores(no operadores alg &optional maxProf objetivo f-heuristica)
  (cond
   ((or (NULL no) (NULL operadores)) NIL)
   ((and (equal alg 'dfs) (equal (profundidade-no no) maxProf)) NIL)
   (T (remover-se #'(lambda (x) (NULL x)) (cons (novo-sucessor no (car operadores) objetivo f-heuristica) (sucessores no (cdr operadores) alg maxProf objetivo f-heuristica))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;; TABULEIROS ;;;;;;;;;;;;;;;;;;;;;;;;
(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posiÃ§Ã£o: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 NIL 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)



;;;;;;;;;;;;;;;;;;;;;;;; PROBLEMAS ;;;;;;;;;;;;;;;;;;;;;;;;

(defun tabuleiro-A()
  "Problema A"
  '(
    (2 20 44 NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL 3 30 NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    )
  )

(defun tabuleiro-B()
  "Problema B"
  '(
    (2 NIL 4 NIL 6 NIL 8 NIL 10 NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL 3 NIL 5 NIL 7 NIL 9 NIL 11) 
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    )
  )

(defun tabuleiro-C()
  "Problema C"
  '(
    (1 12 3 23 NIL 88 NIL NIL NIL NIL)
    (21 45 43 NIL NIL NIL NIL NIL NIL NIL)
    (NIL 56 NIL 78 NIL NIL NIL NIL NIL NIL) 
    (89 NIL 99 54 NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    )
  )


(defun tabuleiro-D()
  "Problema D"
  '(
    (98 97 96 95 94 93 92 91 90 89)
    (1 2 3 4 5 55 6 7 8 9)
    (NIL 66 NIL NIL NIL NIL NIL NIL NIL 11) 
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL 22 NIL NIL NIL NIL NIL 33 NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL 88 NIL NIL NIL 44 NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL 77 NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL 99 NIL NIL NIL)
    )
  )

(defun tabuleiro-E()
  "Problema E"
  '(
    (NIL 5 NIL NIL NIL 15 NIL NIL NIL 25)
    (NIL NIL NIL 6 NIL NIL NIL 16 NIL NIL)
    (NIL 4 NIL NIL NIL 14 NIL NIL NIL 24) 
    (NIL NIL NIL 7 NIL NIL NIL 17 NIL NIL)
    (NIL 3 NIL NIL NIL 13 NIL NIL NIL 23)
    (NIL NIL NIL 8 NIL NIL NIL 18 NIL NIL)
    (NIL 2 NIL NIL NIL 12 NIL NIL NIL 22)
    (NIL NIL NIL 9 NIL NIL NIL 19 NIL NIL)
    (NIL 1 NIL NIL NIL 11 NIL NIL NIL 21)
    (NIL NIL NIL 10 NIL NIL NIL 20 NIL NIL)
    )
  )