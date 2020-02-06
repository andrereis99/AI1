;;;;;;;;;;;;;;;;;;;;;;;;;;; NOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun construir-no (tabuleiro pai &optional (g 0) (h 0))
  "Constroi a estrutura de dados para o no do problema"
  (list tabuleiro pai g h)
  )

(defun no-estado (no)
  "Retorna o estado 'tabuleiro' de um no"
  (car no))

(defun no-pai (no)
  "Devolve o no pai de um no"
  (cadr no))

(defun no-g(no)
  "Devolve o valor g de um no"
  (caddr no))

(defun no-h(no)
  "Devolve o valor h de um no"
  (cadddr no))

(defun no-f(no)
  "Devolve o valor de f de um no"
  (+ (no-g no) (no-h no)))

(defun no-existep (no lista-no)
  "Verifica se um no existe numa determinada lista"
  (eval (cons 'or (mapcar #'(lambda(noaux) (if (equal noaux no) T NIL)) lista-no))))

(defun profundidade-no(no)
"Calcula a profundidade de um no"
  (cond
   ((null (no-pai no)) 0)
   (T (1+ (profundidade-no (no-pai no))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;; AUXILIARES ALGORITMOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

"exemplo: (remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))"
"resultado: (1 2 2 4)"
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun abertos-bfs(abertos s)
  (cond
   ((NULL abertos) s)
   (T (append abertos s))
   )
  
)

(defun abertos-dfs(abertos s)
  (cond
   ((NULL abertos) s)
   (T (append s abertos))
   )
)

(defun escrever-tabuleiro(tabuleiro &optional (stream t))
  "Mostra um tabuleiro bem formatado"
   (NOT (NULL (mapcar #'(lambda(a) (format stream "~%~t~t ~a" a)) tabuleiro))))

(defun escrever-caminho(no &optional n-abertos n-fechados (stream t))
  "Mostra o caminho até um no"
  (cond
   ((NULL no) NIL)
   ((NULL (no-pai no)) (NOT (NULL (mapcar #'(lambda(a) (format stream "~%~t~t ~a" a)) (no-estado no)))))
   (T (progn
        (escrever-caminho (no-pai no))
        (format stream "~%===========================================")
        (NOT (NULL (mapcar #'(lambda(a) (format stream "~%~t~t ~a" a)) (no-estado no))))
        (cond
         ((and (NOT (NULL n-abertos)) (NOT (NULL n-fechados)))
          (progn
            (format t "~%~t~t Numero de nos abertos: ~a ~t Numero de nos fechados: ~a" n-abertos n-fechados)
            (format t "~%~t~t Movimentos: ~a ~t Pontos: ~a" (profundidade-no no) (no-g no))
            ))
         )
        ))
   )
  )

(defun calcular-f(no)
  (cond
   ((NULL no) nil)
   (T (+ (no-g no) (no-h no)))
   )
)

(defun melhor-f(lista objetivo &optional melhor-no melhor-f)
  (let ((f (calcular-f (car lista))))
    (cond
     ((or (null lista) (null objetivo)) melhor-no)
     ((NULL melhor-no) (melhor-f (cdr lista) objetivo (car lista) f))
     ((> f melhor-f) (melhor-f (cdr lista) objetivo (car lista) f))
     (T (melhor-f (cdr lista) objetivo melhor-no melhor-f))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;; ALGORITMOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfs(f-sucessores ops abertos &optional (movimentos 8) solucao fechados)
  "BREADTH-FIRST"
  (let* ((no-escolhido (car abertos))
         (sucessores (funcall f-sucessores no-escolhido ops 'bfs))
         (novo-abertos (abertos-bfs (cdr abertos) sucessores))
         (novo-fechados (cons no-escolhido fechados)))
    (cond
     ((null abertos) nil)
     ((equal movimentos (profundidade-no no-escolhido)) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((null novo-abertos) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((and (not (null solucao)) (<= solucao (no-g no-escolhido)))
      (list no-escolhido (length novo-abertos) (length novo-fechados)))
     (T (bfs f-sucessores ops novo-abertos movimentos solucao novo-fechados))
     )
    ))

(defun dfs(f-sucessores ops abertos &optional (movimentos 8) solucao fechados)
  "DEPTH-FIRST"
  (let* ((no-escolhido (car abertos))
         (sucessores (funcall f-sucessores no-escolhido ops 'bfs))
         (novo-abertos (abertos-dfs (cdr abertos) sucessores))
         (novo-fechados (cons no-escolhido fechados)))
    (cond
     ((null abertos) nil)
     ((< movimentos (profundidade-no no-escolhido)) (dfs f-sucessores ops novo-abertos movimentos solucao novo-fechados))
     ((null novo-abertos) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((and (not (null solucao)) (<= solucao (no-g no-escolhido)))
      (list no-escolhido (length novo-abertos) (length novo-fechados)))
     (T (dfs f-sucessores ops novo-abertos movimentos solucao novo-fechados))
     )
    ))

(defun A*(f-sucessores f-heuristica ops abertos objetivo &optional (movimentos 8) fechados)
  (let* ((no-escolhido (melhor-f abertos objetivo))
         (sucessores (funcall f-sucessores no-escolhido ops 'A* movimentos objetivo f-heuristica))
         (novo-abertos (append (cdr abertos) sucessores))
         (novo-fechados (cons no-escolhido fechados))
         (melhor-fechado (melhor-f fechados objetivo)))
    (cond
     ((null abertos) nil)
     ((null novo-abertos) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((<= objetivo (no-g no-escolhido)) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((equal movimentos (profundidade-no no-escolhido)) (list no-escolhido (length novo-abertos) (length novo-fechados)))
     ((and (NOT (NULL melhor-fechado)) (> (calcular-f melhor-fechado) (calcular-f no-escolhido)))
      (A* f-sucessores f-heuristica ops (cons melhor-fechado novo-abertos) objetivo movimentos 
          (remover-se #'(lambda(x) (if(equal x melhor-fechado) x)) novo-fechados)))
     (T (A* f-sucessores f-heuristica ops novo-abertos objetivo movimentos novo-fechados))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;; RAMIFICAÇÂO E PENETRANCIA ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun penetrancia (list)
  (/ (profundidade-no (first list)) (+ (second list) (third list)))
)

(defun factor-ramificacao (list &optional (valor-L (profundidade-no (first list))) (valor-T (+ (second list) (third list))) (erro 0.1) (bmin 1) (bmax 10e11))
"Devolve o factor de ramificacao, executando o metodo da bisseccao"
  (let ((bmedio (/ (+ bmin bmax) 2)))
    (cond 
     ((< (- bmax bmin) erro) (/ (+ bmax bmin) 2))
     ((< (f-polinomial bmedio valor-L valor-T) 0) (factor-ramificacao list valor-L valor-T erro bmedio bmax))
     (t (factor-ramificacao list valor-L valor-T erro bmin bmedio))
     )
    )
)

(defun f-polinomial (B L valor-T)
 "B + B^2 + ... + B^L=T"
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (f-polinomial B (- L 1) valor-T)))
  )
)