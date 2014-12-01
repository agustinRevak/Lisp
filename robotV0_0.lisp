(defun muestra-tablero (board)
  (format t "~%")
  (loop for i below (car (array-dimensions board)) do
        (loop for j below (cadr (array-dimensions board)) do
          (let ((cell (aref board i j)))
            (format t "~a " cell)))
        (format t "~%")))


(defun mapa1 ()
	(make-array '(3 3)  
              :initial-contents '((Z M Z) (Z _ Z) (Z R Z))))


(defun mapa2 ()
	(make-array '(6 6)  
              :initial-contents '(
(Z M Z Z Z Z) 
(Z _ Z Z Z Z) 
(Z _ Z Z Z Z)
(Z _ _ _ _ Z)
(Z _ _ _ Z Z)
(Z Z Z R Z Z)


)))



(defun mapa3 ()
	(make-array '(9 9)  
              :initial-contents '(
(Z Z Z Z Z Z Z Z Z) 
(Z _ Z M _ _ Z Z Z) 
(Z _ Z Z Z _ Z Z Z)
(Z _ _ _ _ _ _ _ Z)
(Z _ _ _ _ Z Z _ Z)
(Z Z Z Z Z Z Z _ Z)
(Z Z Z _ _ _ _ _ Z)
(Z Z Z Z R Z Z Z Z)
(Z Z Z Z Z Z Z Z Z)


)))


(defun mapa4 ()
	(make-array '(10 8)  
              :initial-contents '(
(Z Z Z Z Z Z Z Z) 
(Z Z Z _ _ _ R Z) 
(Z Z _ _ Z Z Z Z)
(Z Z _ Z Z Z Z Z)
(Z Z _ _ _ _ _ Z)
(Z Z Z _ Z Z _ Z)
(Z _ _ _ M Z _ Z)
(Z _ Z Z Z Z _ Z)
(Z _ _ _ _ _ _ Z)
(Z Z Z Z Z Z Z Z)


)))


(defun mapa5 ()
	(make-array '(5 12)  
              :initial-contents '(
(Z Z Z Z Z Z Z Z Z Z Z Z) 
(Z Z _ _ _ _ _ _ _ _ Z Z) 
(Z R _ Z Z Z Z Z Z _ M Z)
(Z Z _ _ _ _ _ _ _ _ Z Z)
(Z Z Z Z Z Z Z Z Z Z Z Z)



)))

(defun iniciajuego ()
	(print "Saludos humano, bienvenido!!")
	(print "Estos son los pasos")
	(print "Me muevo arriba = I")
	(print "Mue muevo abajo = K")
	(print "Me muevo a la izq = J")
	(print "Me muevo a la der = L")
	(print "Termino el juego = M")
	(print "Al terminar un mapa, tendras 2 opciones")
	(print "Seguir = N")
	(print "Repite los pasos = R")
	(print "Empecemos!!!")
	(print " ")
	(setq yanolesigo `y)
	(setq contador 1)
	(setq mapas (list (mapa1) (mapa2) (mapa3) (mapa4) (mapa5)))

	(setq valorx1 2)
	(setq valory1 1)
	(setq metax1 0)
	(setq metay1 1)

        (setq reproduceX valorx1)
        (setq reproduceY valory1)

	(setq valorx2 5)
	(setq valory2 3)
	(setq metax2 0)
	(setq metay2 1)
	
	(setq valorx3 7)
	(setq valory3 4)
	(setq metax3 1)
	(setq metay3 3)

	(setq valorx4 1)
	(setq valory4 6)
	(setq metax4 6)
	(setq metay4 4)

	(setq valorx5 2)
	(setq valory5 1)
	(setq metax5 2)
	(setq metay5 10)

	(setq robotx valorx1)
	(setq roboty valory1)
	(setq listaDemovimientos ())

	(loop while (and (not(equal yanolesigo `n)) (not (equal mapas nil))) do
		(print "Mapa")
		(print contador)
                  (format t "~%")
		(funcall #' muestra-tablero (first mapas))
		(setf yanolesigo (read))
		(cond ((equal yanolesigo `i)
			(setq movimiento #'mueve-arriba))

		      ((equal yanolesigo `k)
			 (setq movimiento #'mueve-abajo))

		      ((equal yanolesigo `j)
			  (setq movimiento #'mueve-izq))

		      ((equal yanolesigo `l)
			  (setq movimiento #'mueve-der))
		)
		
		(setq resultado (funcall  movimiento robotx roboty (first mapas)))
		(setq listaDemovimientos (agrega-movimientos listaDemovimientos movimiento))
		(cond ((and (listp resultado) (not (equal resultado nil)))
                        (print "resultado")
                        (car resultado)
			(setq mapas   (append (list (first resultado)) (cdr mapas)))
			(setq robotx (second resultado))
			(setq roboty (third resultado))
			)
			((eql resultado nil) 
			 (print "Movimiento invalido!!")
			)
			((eql resultado T)
			     (print "Felicidades, pasa al siguiente mapa")
                             (funcall #' muestra-tablero (first mapas))
              
                             (print "Gustas reproducir tus movimientos o sigues con el siguiente mapa? (Q) si, otra tecla para entenderlo como un no")
                             (setf opcion (read))
                             (cond ((equal opcion `q)
                                                      (cond ((= contador 1)
                                                              (setq mapaRepeticion (mapa1)))
                                                            ((= contador 2)
                                                              (setq mapaRepeticion (mapa2)))
                                                            ((= contador 3)
                                                              (setq mapaRepeticion (mapa3)))
                                                            ((= contador 4)
                                                              (setq mapaRepeticion (mapa4)))
                                                            ((= contador 5)
                                                              (setq mapaRepeticion (mapa5)))
                                                       )

                                                      (loop while (equal opcion `q) do
                                                            (reproduceMovimientos mapaRepeticion  reproduceX reproduceY listaDemovimientos)
                                                            (print "Ya lo reproduje, quieres que lo vuelva a reproducir (Q) o pasas al siguiente mapa?")
                                                            (setf opcion (read))
                                                      )
                                                      )
                              )

                             (setq listaDemovimientos ())

 			     (setq contador (+ contador 1))
			     (setq mapas (cdr mapas))
			     (cond ((= contador 2) 
				(setq robotx valorx2)
				(setq roboty valory2)

                                (setq reproduceX valorx2)
                                (setq reproduceY valory2)
				)
			      ((= contador 3)
				(setq robotx valorx3)
				(setq roboty valory3)

                                (setq reproduceX valorx3)
                                (setq reproduceY valory3)
				)
			      ((= contador 4) 
				(setq robotx valorx4)
				(setq roboty valory4)

                                (setq reproduceX valorx4)
                                (setq reproduceY valory4)
				)
			      ((= contador 5)
				(setq robotx valorx5)
				(setq roboty valory5)

                                (setq reproduceX valorx5)
                                (setq reproduceY valory5)
				)
			      )			
			 )
                        )
              )
		
	
	(cond ((equal yanolesigo `n) (print "Usted salio del juego"))
	      ((equal mapas nil) (print "Ya no quedan mapas"))
         )
)
		

(defun reproduceMovimientos (tablero x y listademovimientos)
  (setq muevox x)
  (setq muevoy y)
  (loop while (> (list-length listademovimientos) 0)  do
        	(funcall #' muestra-tablero tablero)
        	(setq resultado (funcall  (car listaDemovimientos) muevox muevoy tablero))
		(cond ((and (listp resultado) (not (equal resultado nil)))
			(setq tablero (first resultado))
			(setq muevox (second resultado))
			(setq muevoy (third resultado))
			)
			((eql resultado nil) 
			 (print "Movimiento invalido!!")
			)
			((eql resultado T)
			     (print "Felicidade pasa al siguiente mapa")
                             (funcall #' muestra-tablero tablero)
                             )
                        )
                
        (setq listademovimientos (cdr listademovimientos))
   )

)

(defun mueve-arriba(x y tablero)
	(cond ((< (- x 1) 0) nil)
	 ((equal `Z (aref tablero (- x 1) y)) nil)
	 ((equal `M (aref tablero (- x 1) y)) T)
	 ((equal `_ (aref tablero (- x 1) y)) 
	     (setf (aref tablero  (- x 1) y) `R)
             (setf (aref tablero x y) `_ )
	     (setq x (- x 1))
	     (setq lista (list tablero x y))
              lista
	)
        )
) 



(defun mueve-abajo(x y tablero)
	(setq limite (array-dimension tablero 1))
	(cond ((>= (+ x 1) limite) nil)
	      ((equal `Z (aref tablero (+ x 1) y)) nil)
	      ((equal `M (aref tablero (+ x 1) y)) T)
	      ((equal `_ (aref tablero (+ x 1) y)) 
	       (setf (aref tablero    (+ x 1) y) `R)
               (setf (aref tablero x y) `_ )
	       (setq x (+ x 1))
	       (setq lista (list tablero x y))
               lista
	     )
         )
) 	


(defun mueve-izq(x y tablero)
	(cond ((< (- y 1) 0) nil)
	      ((equal `Z (aref tablero x (- y 1) )) nil)
	      ((equal `M (aref tablero x (- y 1) )) T)
	       ((equal `_ (aref tablero x (- y 1) )) 
	        (setf (aref tablero  x (- y 1) ) `R)
                (setf (aref tablero x y) `_ )
	        (setq y (- y 1))
	        (setq lista (list tablero x y))
                 lista
	       )
         )
) 


(defun mueve-der(x y tablero)
	(setq limite (array-dimension tablero 1))
	(cond ((>= (+ y 1) limite) nil)
	      ((equal `Z (aref tablero x (+ y 1) )) nil)
	       ((equal `M (aref tablero x (+ y 1))) T)
	       ((equal `_ (aref tablero x (+ y 1) )) 
	           (setf (aref tablero x (+ y 1) ) `R)
                   (setf (aref tablero x y) `_ )
	           (setq y (+ y 1))
	        (setq lista (list tablero x y))
                 lista
	       )
         )
) 

(defun escribe-movimiento(mov)
	(with-open-file (stream "C:\\movimientosrobot.txt"  :direction :output
							    :if-exists :overwrite
							    :if-does-not-exist :create)
  (print mov stream)))



(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
	

(defun agrega-movimientos(lista p)
	(append lista (list p)))
