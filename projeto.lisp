;;;;Carrega os outros ficheiros de c�digo, escreve e l� de ficheiros e trata da intera��o com o utilizador
;;;; Disciplina de IA - 2019 / 2020
;;;; 1� projeto
;;;; Autores: Andr� Reis 170221035 e Bruno Alves 170221041

;;; Menus
(defun menu-inicial()
"Mostra o menu inicial"
  (progn
    (format t "~% --------------------------------------------------------- ")
    (format t "~%|                    JOGO DO CAVALO                       |")
    (format t "~%|                                                         |")
    (format t "~%|                1 - Simular jogo                         |")
    (format t "~%|                s - Sair                                 |")
    (format t "~%|                                                         |")
    (format t "~% ---------------------------------------------------------~%~%> ")
  )
)

(defun start()
  "Inicia o programa"
  (progn
    (menu-inicial)
    (let ((opt (read)))
      (cond
       ((eq opt 's) (format t "At� �pr�xima!"))
       ((or (not (numberp opt)) (not (equal opt 1))) (progn (format t "Op��o inv�lida") (start)))
       ((eq opt '1) (menu-algoritmos))))))


(defun ler-algoritmo-mensagem()
  "Apresenta a mensagem para escolher o algoritmo"
  (progn
    (format t "    ~%---------------------------------------------------------")
    (format t "   ~%|           JOGO DO CAVALO - Escolha o algoritmo          |")
    (format t "   ~%|                                                         |")
    (format t "   ~%|                1 - Breadth-First Search                 |")
    (format t "   ~%|                2 - Depth-First Search                   |")
    (format t "   ~%|                3 - A*                                   |")
    (format t "   ~%|                0 - Voltar                               |")
    (format t "   ~%|                s - Sair                                 |")
    (format t "   ~%|                                                         |")
    (format t "   ~% ---------------------------------------------------------~%~%> ")
    )
  )

(defun menu-algoritmos()
  "Executa um algoritmo, dependendo da opcao escolhida"
  (progn (ler-algoritmo-mensagem)
    (let ((opt (read)))
      (cond 
       ((eq opt '0) (start))
       ((eq opt 's) (format t "At� �pr�xima!"))
       ((or (not (numberp opt)) (> opt 3) (< opt 1)) (progn (format t "Insira uma op��o v�lida") (menu-algoritmos)))
       (T (let* ((referencia-tabuleiro (ler-tabuleiro 'menu-algoritmos))
                 (tabuleiro (second referencia-tabuleiro))
                 (ntabuleiro (first referencia-tabuleiro))
                 (no (list (construir-no tabuleiro nil))))
            (ecase opt
              (1 
               (let* ((movimentos (ler-n-movimentos))
                      (objetivo (ler-objetivo))
                      (solucao (list (current-time) (bfs 'sucessores (operadores) no movimentos objetivo) (current-time) ntabuleiro 'BFS movimentos objetivo)))
                 (escrever-estatisticas-ficheiro solucao)
                 ))

              (2 
               (let* ((movimentos (ler-n-movimentos))
                      (objetivo (ler-objetivo))
                      (solucao (list (current-time) (dfs 'sucessores (operadores) no movimentos objetivo) (current-time) ntabuleiro 'DFS movimentos objetivo)))
                 (escrever-estatisticas-ficheiro solucao)
                 ))

              (3 
               (let* ((heuristica (ler-heuristica))
                      (objetivo (ler-objetivo))
                      (movimentos (ler-n-movimentos))
                      (solucao (list (current-time) (A* 'sucessores heuristica (operadores) no objetivo movimentos) (current-time) ntabuleiro 'A* movimentos objetivo heuristica)))
                 (escrever-estatisticas-ficheiro solucao)
                 )))
              ))
          ))
      ))


(defun ler-movimentos-messagem()
  "Apresenta a mensagem para escolher o numero maximo de movimentos"
  (format t "   ~% ------------------------------------------------------------")
  (format t "   ~%|   JOGO DO CAVALO - Defina um n�mero maximo de movimentos   |")
  (format t "   ~%|              n - N�o definir maximo de movimentos          |")
  (format t "   ~%|             -1 - Voltar                                    |")
  (format t "   ~%|              s - Sair                                      |")
  (format t "   ~% ------------------------------------------------------------~%~%> ")
  )


(defun ler-n-movimentos()
  "Le o numero maximo de movimentos recepido"
  (progn
    (ler-movimentos-messagem)
    (let ((opt (read)))
      (cond 
       ((eq opt '-1) (menu-algoritmos))
       ((eq opt 'n) nil)
       ((eq opt 's) (format t "At� � pr�xima!"))
       ((or (not (numberp opt)) (< opt -1)) (progn (format t "Insira uma op��o v�lida")) (ler-n-movimentos))
       (T opt)))
    ))

(defun ler-objetivo-messagem()
  "Apresenta a mensagem para escolher o numero maximo de movimentos"
  (format t "   ~% ------------------------------------------------------------")
  (format t "   ~%|             JOGO DO CAVALO - Defina um objetivo            |")
  (format t "   ~%|                 n - N�o definir objetivo                   |")
  (format t "   ~%|                -1 - Voltar                                 |")
  (format t "   ~%|                 s - Sair                                   |")
  (format t "   ~% ------------------------------------------------------------~%~%> ")
  )


(defun ler-objetivo()
  "Le o objetivo de pontos"
  (progn
    (ler-objetivo-messagem)
    (let ((opt (read)))
      (cond 
       ((eq opt '-1) (menu-algoritmos))
       ((eq opt 'n) nil)
       ((eq opt 's) (format t "At� � pr�xima!"))
       ((or (not (numberp opt)) (< opt -1)) (progn (format t "Insira uma op��o v�lida")) (ler-objetivo))
       (T opt)))
    ))

(defun ler-heuristica-messagem()
  "Apresenta a mensagem para escolher a heuristica"
  (format t "   ~% ------------------------------------------------------------")
  (format t "   ~%|            JOGO DO CAVALO - Escolha a heur�stica           |")
  (format t "   ~%|                 1 - Heur�stica Enunciado                   |")
  (format t "   ~%|                 2 - Heur�stica Criada                      |")
  (format t "   ~%|                 0 - Voltar                                 |")
  (format t "   ~%|                 s - Sair                                   |")
  (format t "   ~% ------------------------------------------------------------~%~%> ")
  )


(defun ler-heuristica()
  "Le a heuristica"
  (progn
    (ler-heuristica-messagem)
    (let ((opt (read)))
      (cond
       ((eq opt '0) (menu-algoritmos))
       ((eq opt 'n) nil)
       ((eq opt 's) (format t "At� �pr�xima!"))
       ((or (not (numberp opt)) (< opt 0) (> opt 2)) (progn (format t "Insira uma op��o v�lida")) (ler-heuristica))
       ((eq opt 1) 'heuristica-base)
       (T 'heuristica-melhor)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;; TABULEIROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caminhos-ficheiro-problemas()
  "Devolve o path para o ficheiro problemas.dat (C:\lisp\problemas.dat)"
  (make-pathname :host "c" :directory '(:absolute "lisp") :name "problemas" :type "dat"))

(defun ler-tabuleiros()
  "Retorna os tabuleiros no ficheiro problemas.dat"
  (with-open-file (file (caminhos-ficheiro-problemas) :if-does-not-exist nil)
    (do ((result nil (cons next result)) (next (read file nil 'eof) (read file nil 'eof)))
        ((equal next 'eof) (reverse result))
      )
    ))

(defun tabuleiros(&optional(i 1) (problems (ler-tabuleiros)))	
  "Ilustra as boards no ficheiro problemas.dat"
  (cond
   ((null problems) (progn   
                      (format t "~%|                     7 - Tabuleiro Aleatorio               |")
                      (format t "~%|                     0 - Voltar                            |")
                      (format t "~% -----------------------------------------------------------~%~%>"))
    )
   (T (progn 
        (if (= i 1) 
            (progn 
              (format t "~% -----------------------------------------------------------")
              (format t "~%|             JOGO DO CAVALO - Escolha o Tabuleiro              |")
              ))
        (format t "~%|                     ~a - Tabuleiro ~a                       |" i i) 
        (tabuleiros (+ i 1) (cdr problems))
        ))
   ))

(defun ler-tabuleiro(menu)
  "Selecciona uma board das opcoes do menu"
  (progn 
    (tabuleiros)
    (let ((opt (read)))
      (cond ((eq opt '0) (funcall menu))
            ((not (numberp opt)) (progn 
                                   (format t "Insira uma op��o v�lida") 
                                   (ler-tabuleiro menu)
                                   ))
            (T (let ((lista-tabuleiros (ler-tabuleiros)))
                 (cond
                  ((equal opt 7) (list opt (tabuleiro-aleatorio)))
                  ((or (< opt 0) (> opt (length lista-tabuleiros))) (progn (format t "Insira uma op��o v�lida") (ler-tabuleiro menu)))
                  (T (list opt (nth(1- opt) lista-tabuleiros)))
                  )
                 ))
            )
      )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;; RESULTADOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun caminhos-ficheiro-resultados()
"Devolve o path para o ficheiro (C:\lisp\resultados.dat)"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "resultados" :type "dat"))

(defun escrever-estatisticas-ficheiro (solucao)
  "Escreve, no ficheiro de resultados, a solucao e medidas de desempenho de um determinado problema"
  (let* ((tempo-inicial (first solucao))
         (caminho-solucao (second solucao))
         (tempo-fim (third solucao))
         (ntabuleiro (fourth solucao))
         (alg (fifth solucao))
         (movimentos (sixth solucao))
         (objetivo (seventh solucao))
         (heuristica (eighth solucao)))
    (with-open-file (file (caminhos-ficheiro-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
      (progn
        (format file "~%* Resolu��o do Tabuleiro ~a *" ntabuleiro)
        (format file "~%~t> Algoritmo: ~a " alg)
        (format file "~%~t> In�cio: ~a:~a:~a" (first tempo-inicial) (second tempo-inicial) (third tempo-inicial))
        (format file "~%~t> Fim: ~a:~a:~a" (first tempo-fim) (second tempo-fim) (third tempo-fim))
        (format file "~%~t> N�mero de n�s gerados: ~a" (+ (second caminho-solucao) (third caminho-solucao)))
        (format file "~%~t> N�mero de n�s expandidos: ~a" (third caminho-solucao))
        (format file "~%~t> Penetr�ncia: ~F" (penetrancia caminho-solucao))
        (format file "~%~t> Fator de ramifica��o m�dia ~F" (factor-ramificacao caminho-solucao))
        (format file "~%~t> Profundidade m�xima: ~a" movimentos)
        (format file "~%~t> Comprimento da solu��o ~a" (profundidade-no (first caminho-solucao)))
        (format file "~%~t> Objetivo pretendido: ~a" objetivo)
        (format file "~%~t> Pontos totais: ~a" (no-g (first caminho-solucao)))
        (if (eq alg 'A*)
            (format file "~%~t> Heuristica utilizada: ~a" heuristica))
        (escrever-caminho (first caminho-solucao) (second caminho-solucao) (third caminho-solucao))
        ))
    )
  )

(defun current-time()
  "Retorna o tempo actual com o formato (h m s)"
  ;;HORAS-MINUTOS-SEGUNDOS
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
    )
  )
