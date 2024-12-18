#lang racket

#|

- Autor. Ing(c) David Vasquez Naranjo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado: 8.11.1
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Un descriptivo de que hace el programa: es un programa que permite jugar ajedrez a dos(2) jugadores
- Un descriptivo de cada identificador utilizado en cada función: el descriptivo de cada parametro esta a lo largo del codigo

|#

( require graphics/graphics ) ;se requiere la libreria a utilizar
(open-graphics )
(define tablero (open-viewport "Ajedrez" 900 600)) ;se define el viewport en el que se va a jugar

( define ( Board counter p x1 y1 x2 y2 )
   ( define ( Lines counter x y )                                                                                   ; inicio funcion para el tablero
      ( if ( < counter 4 )
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn x y ) 75 75 "gray" )
              ( Lines ( + 1 counter ) ( + 150 x ) y ) ) 
           ( void )))
   ( if ( < counter 8 )
        ( begin
           ( if ( = p 1 )
                ( Lines 0 x1 y1 )
                ( Lines 0 x2 y2 ))
           ( Board ( + counter 1 ) ( if ( = p 1 ) 0 1 ) x1 ( + 75 y1 ) x2 ( + 75 y1 )))
        ( void ))
   ) #| fin funcion Board                                                                                      ; fin funcion para el tablero
counter: es el contador que le indica a la funcion cuando parar
p: es un parametro que indica el numero de linea y se empieza dejando espacio o haciendo un cuadro gris
"x1" y "y1": representan las coordenadas de las filas impares empezando desde 1
"x2" y "y2": representan las coordenadas de las filas pares empezando desde 1 |#

( define ( Fichas counterFichas counterX counterY )                                                             ; inicio funcon de las fichas
   ( if ( = counterX 8 )
        ( Fichas counterFichas 0 ( + 1 counterY ) )
        ;else
   ( begin
      ( if ( = counterFichas 65 )
        ( void )
        ( begin
                  ( if ( or ( = counterFichas 1 ) ( = counterFichas 8 ))
                       ((( draw-pixmap-posn "torreNegra.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 2 ) ( = counterFichas 7 ))
                       ((( draw-pixmap-posn "caballoNegro.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 3 ) ( = counterFichas 6 ))
                       ((( draw-pixmap-posn "alfilNegro.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( = counterFichas 4 )
                       ((( draw-pixmap-posn "reinaNegra.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( = counterFichas 5 )
                       ((( draw-pixmap-posn "reyNegro.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 9 ) ( = counterFichas 10 ) ( = counterFichas 11 ) ( = counterFichas 12 ) ( = counterFichas 13 ) ( = counterFichas 14 ) ( = counterFichas 15 ) ( = counterFichas 16 ))
                       ((( draw-pixmap-posn "peonNegro.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 49 ) ( = counterFichas 50 ) ( = counterFichas 51 ) ( = counterFichas 52 ) ( = counterFichas 53 ) ( = counterFichas 54 ) ( = counterFichas 55 ) ( = counterFichas 56 )) 
                       ((( draw-pixmap-posn "peonBlanco.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 57 ) ( = counterFichas 64 ))
                       ((( draw-pixmap-posn "torreBlanca.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 58 ) ( = counterFichas 63 ))
                       ((( draw-pixmap-posn "caballoBlanco.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( or ( = counterFichas 59 ) ( = counterFichas 62 ))
                       ((( draw-pixmap-posn "alfilBlanco.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( = counterFichas 60 )
                       ((( draw-pixmap-posn "reinaBlanca.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                  ( if ( = counterFichas 61 )
                       ((( draw-pixmap-posn "reyBlanco.png" ) tablero ) ( make-posn ( * 75 counterX ) ( * 75 counterY )))
                       ( void )))))))))))))
              ( Fichas ( + 1 counterFichas ) ( + 1 counterX ) counterY )))))
   ) #| fin funcion Fichas                                                                                                 ;fin funcion para las fichas
counterFichas: sirve como ancla para que la funcion pare
counterX: sirve para indicarle a la funcion cuando hacer "salto de linea" y indicar la coordenada en la que esta
counterY: sirve para indicar la coordenada en "y" en la que esta |#

( define strTable "T1C1A1DNRNA2C2T2P1P2P3P4P5P6P7P8&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&p1p2p3p4p5p6p7p8t1c1a1dbrba2c2t2$$$$$$$$" ) ;string para las fichas

( define ( ChangeChar str1 pos char )                                                                         ;inicio parte para cambiar posiciones del string
( string-append ( substring str1 0 ( * 2 pos )) char ( substring str1 (+ 2 ( * 2 pos ))))
   ) #| fin funcion ChangeChar
str1: es el string en el que se va a cambiar el char
pos: es la posicion en la que se va a cambiar
char: es por lo que se va a cambiar |#

( define ( ChangePosition str pos1 pos2 )
   ( define c1 ( ChangeChar str ( - pos2 1 ) ( string-append ( string ( string-ref str ( * ( - pos1 1 ) 2 ))) ( string ( string-ref str ( + 1 ( * ( - pos1 1 ) 2 )))))))
   ( ChangeChar c1 ( - pos1 1 ) "&&" )
) #| fin funcion ChangePositions                                                                              ;fin parte para cambiar posiciones del string
str: es el string en el cual se va a hacer el cambio
pos1 y pos2: son las posiciones en las cuales se va a hacer el cambio |#

( define ( FindString str x y )                                                                               
   ( string-append ( string ( string-ref str ( * 2 ( + x ( * y 8 )))))                    ;funcion para encontrar el string en una coordenada
                   ( string ( string-ref str ( + 1 ( * 2 ( + x ( * y 8 )))))))
   ) ;fin funcion FindString

( define ( Blocks casilla x y )
   ( if ( = ( remainder y 2 ) 0 )
        ( if ( = ( remainder casilla 2 ) 0 )                      ;funcion para poner un bloque de un color dependiendo de la casilla
            "gray" 
             ;else
            "white" )
        ;else
        ( if ( = ( remainder casilla 2 ) 1 )
             "gray"
             ;else
             "white" ))
   ) #| fin funcion Blocks
casilla: sirve paar saber si el color se esa casilla es blanco o negro
"x" y "y": representan las coordenadas de la posicion |#

( define ( BadPlay x )
   (( draw-solid-rectangle tablero ) ( make-posn 650 50 ) 200 70 "white" )
   ((draw-string tablero ) (make-posn 700 90 ) ( if ( = x 1 )
                                                    "Casilla invalida"
                                                    ( if ( = x 2 )
                                                         "No es tu turno"
                                                         "ESTA EN JAQUE")))               ;funcion que dice los errores
   ( sleep 1 )
   (( draw-solid-rectangle tablero ) ( make-posn 650 50 ) 200 70 "sienna" )
   ) #| fin funcion BadPlay
x: sirve para definir el mensaje que se mostrara |#

( define ( StrCasilla str x )
   ( string-append ( string ( string-ref str ( - ( * 2 x ) 2 ))) ( string ( string-ref str ( - ( * 2 x ) 1 ))))  ;funcion para encontrar el string con una casilla
   ) #| fin funcion para encontrar el string con una casilla
str: es el string en el que se va a evaluarla casilla
x: es la casilla que se va a evaluar |#

( define ( CaminoTorre str counter x1 x2 y1 y2 )                                                  ;inicio restricciones fichas
   ( if ( and ( = y1 y2 )
              ( > x2 x1 ))
        ( if ( = counter ( - x2 x1 ))
             #t
             ( if ( string=? ( FindString str ( + counter x1 ) y2 ) "&&" )
                  ( CaminoTorre str ( + counter 1 ) x1 x2 y1 y2 )
                  #f ))
    ( if ( and ( = y1 y2 )
               ( > x1 x2 ))
         ( if ( = counter ( - x1 x2 ))
              #t
              ( if ( string=? ( FindString str ( + counter x2 ) y2 ) "&&" )
                   ( CaminoTorre str ( + counter 1 ) x1 x2 y1 y2 )
                   #f ))
    ( if ( and ( > y1 y2 )
               ( = x1 x2 ))
         ( if ( = counter ( - y1 y2 ))
              #t
              ( if ( string=? ( FindString str x2 ( + counter y2 )) "&&" )
                   ( CaminoTorre str ( + counter 1 ) x1 x2 y1 y2 )
                   #f ))
    ( if ( and ( > y2 y1 )
               ( = x1 x2 ))
         ( if ( = counter ( - y2 y1 ))
              #t
              ( if ( string=? ( FindString str x2 ( + counter y1 )) "&&" )
                   ( CaminoTorre str ( + counter 1 ) x1 x2 y1 y2 )
                   #f ))
         void ))))
   ) #| fin funcion CaminoTorre
str: es el string en el que se va a evaluar el camino
counter: representa la posicion que va evaluando cada vez
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( CaminoAlfil str counter x1 x2 y1 y2 )
   ( if ( and ( = ( - y2 y1 ) ( - x2 x1 ))
              ( > y2 y1 ))
        ( if ( = counter ( - y2 y1 ))
             #t
             ( if ( string=? ( FindString str ( + counter x1 ) ( + counter y1 )) "&&" )
                  ( CaminoAlfil str ( + counter 1 ) x1 x2 y1 y2 )
                  #f ))
   ( if ( and ( = ( - y2 y1 ) ( - x2 x1 ))
              ( > y1 y2 ))
         ( if ( = counter ( - y1 y2 ))
             #t
             ( if ( string=? ( FindString str ( - x1 counter ) ( - y1 counter )) "&&" )
                  ( CaminoAlfil str ( + counter 1 ) x1 x2 y1 y2 )
                  #f ))
   ( if ( and ( = ( - y2 y1 ) ( - x1 x2 ))
              { > y2 y1 })
        ( if ( = counter ( - y2 y1 ))
             #t
             ( if ( string=? ( FindString str ( - x1 counter ) ( + y1 counter )) "&&" )
                  ( CaminoAlfil str ( + counter 1 ) x1 x2 y1 y2 )
                  #f ))
   ( if ( and ( = ( - y2 y1 ) ( - x1 x2 ))
              { > y1 y2 })
        ( if ( = counter ( - y1 y2 ))
             #t
             ( if ( string=? ( FindString str ( + x1 counter ) ( - y1 counter )) "&&" )
                  ( CaminoAlfil str ( + counter 1 ) x1 x2 y1 y2 )
                  #f ))
        void ))))
   ) #| fin funcion CaminoAlfil
str: es el string en el que se va a evaluar el camino
counter: representa la posicion que va evaluando cada vez
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( PeonWhiteRestriction str x1 x2 y1 y2 )  ;restriccion peones
   ( if ( or ( and ( = y2 ( - y1 1 )) ;jugada diagonal
                   ( or ( = x2 ( + x1 1 ))
                        ( = x2 ( - x1 1 )))
                   ( not ( string=? "&&" ( FindString str x2 y2 ))))
             
             ( and ( = y1 6 ) ;jugada de 2 casillas en el inicio
                   ( or ( and ( = y2 ( - y1 2 ))
                              ( = x1 x2 ))
                        ( and ( = y2 ( - y1 1 ))
                              ( = x1 x2 )))
                   ( string=? ( FindString str x2 y2 ) "&&" ))
             
             ( and ( = y2 ( - y1 1 )) ;movimiento normal del peon
                   ( = x1 x2 )
                   ( string=? ( FindString str x2 y2 ) "&&" )))
        #t             
        #f )
) #| fin funcion PeonWhiteRestriction
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( PeonBlackRestriction str x1 x2 y1 y2 )
   ( if ( or ( and ( = y2 ( + y1 1 )) ;jugada diagonal
                   ( or ( = x2 ( + x1 1 ))
                        ( = x2 ( - x1 1 )))
                   ( not ( string=? "&&" ( FindString str x2 y2 ))))
             
             ( and ( = y1 1 ) ;jugada de 2 casillas
                   ( or ( and ( = y2 ( + y1 2 ))
                              ( = x1 x2 ))
                        ( and ( = y2 ( + y1 1 ))
                              ( = x1 x2 )))
                   ( string=? ( FindString str x2 y2 ) "&&" ))
             ( and ( = y2 ( + y1 1 )) ;jugada normal peon
                   ( = x1 x2 )
                   ( string=? ( FindString str x2 y2 ) "&&" )))
        #t
        #f )
   ) #| fin restriccion peon negro
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( TorreRestriction str x1 x2 y1 y2 )    ;restriccion torres
   ( if ( and ( or ( = y1 y2 )
                   ( = x1 x2 ))
              ( CaminoTorre str 1 x1 x2 y1 y2 ))
        #t 
        #f )
   ) #| fin funcion TorreRestriction
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( CaballoRestriction x1 x2 y1 y2 )     ;restriccion caballos
   ( if ( or ( and ( or ( = y2 ( - y1 1 )) ( = y2 ( + y1 1 )))
                   ( or ( = x2 ( + x1 2 )) ( = x2 ( - x1 2 ))))
             ( and ( or ( = y2 ( - y1 2 )) ( = y2 ( + y1 2 )))
                   ( or ( = x2 ( + x1 1 )) ( = x2 ( - x1 1 )))))    
        #t
        #f )
   ) #| fin funcion CaballoRestriction
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( AlfilRestriction str x1 x2 y1 y2 )        ;restriccion alfil
   ( if ( and ( or ( = ( - y2 y1 ) ( - x2 x1 ))
                   ( = ( - y2 y1 ) ( - x1 x2 )))
              ( CaminoAlfil str 1 x1 x2 y1 y2 ))
        #t
        #f )
   ) #| fin funcion AlfilRestriction
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( ReinaRestriction str x1 x2 y1 y2 )          ;restriccion reina
   ( if ( and ( or ( = y1 y2 )
                   ( = x1 x2 )
                   ( = ( - y2 y1 ) ( - x2 x1 ))
                   ( = ( - y2 y1 ) ( - x1 x2 )))
              ( CaminoTorre str 1 x1 x2 y1 y2 )
              ( CaminoAlfil str 1 x1 x2 y1 y2 ))
        #t
        #f )
   ) #| fin funcion ReinaRestriction
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#

( define ( ReyRestriction x1 x2 y1 y2 )         ;restriccion rey
   ( if ( or ( and ( = y1 y2 )
                   ( or ( = x2 ( + 1 x1 )) ( = x2 ( - x1 1 ))))
             ( and ( or ( = y2 ( - y1 1 )) ( = y2 ( + y1 1 )))
                   ( or ( = x2 x1 ) ( = x2 ( - x1 1 )) ( = x2 ( + 1 x1 )))))
        #t
        #f )
   ) #| fin funcion ReyRestriction
str: es el string en el que se va a evaluar la restriccion
"x1" y "y1" representan las coordenadas de la primera posicion
"x2" y "y2" representan las coordenadas de la segunda posicion |#                                   ;fin restricciones fichas

( define ( DecorationCoronation )                                                                      ;incicio funciones para coronacion de peones
   (( draw-solid-rectangle tablero ) ( make-posn 650 50 ) 200 50 "white" )
   (( draw-solid-rectangle tablero ) ( make-posn 650 100 ) 200 50 "white" )
   (( draw-solid-rectangle tablero ) ( make-posn 650 150 ) 200 50 "white" )
   (( draw-solid-rectangle tablero ) ( make-posn 650 200 ) 200 50 "white" )
   (( draw-rectangle tablero ) ( make-posn 650 50 ) 200 50 )
   (( draw-rectangle tablero ) ( make-posn 650 100 ) 200 50 )
   (( draw-rectangle tablero ) ( make-posn 650 150 ) 200 50 )
   (( draw-rectangle tablero ) ( make-posn 650 200 ) 200 50 )
   ((draw-string tablero ) (make-posn 720 80 ) "Caballo" )
   ((draw-string tablero ) (make-posn 720 130 ) "Reina" )
   ((draw-string tablero ) (make-posn 720 180 ) "Torre" )
   ((draw-string tablero ) (make-posn 720 230 ) "Alfil" )
   ) ;define DecorationCoronation

( define ( PeonCoronation str x y casilla click counter turn ) 
   ( define y1 ( quotient ( - ( posn-y ( mouse-click-posn click )) 50 )  50 ))
   ( if ( = counter 1 )
        str
        ( if ( or ( > ( posn-x ( mouse-click-posn click )) 850 ) ( < ( posn-x ( mouse-click-posn click )) 650 )
                  ( > ( posn-y ( mouse-click-posn click )) 250 ) ( < ( posn-x ( mouse-click-posn click )) 50 ))
             ( PeonCoronation str x y casilla ( get-mouse-click tablero ) 0 turn )
             ( begin
                (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x )( * 75 y )) 75 75 ( Blocks casilla ( * 75 x )( * 75 y )))
                (( draw-solid-rectangle tablero ) ( make-posn 650 50 ) 200 200 "sienna" )
                (( draw-solid-rectangle tablero ) ( make-posn 650 375 ) 200 70 "white" )
                (( draw-string tablero ) (make-posn 720 415 ) ( if ( = turn 1 ) "Turno: 2" "Turno 1" ))
                ( if ( = turn 1 )
                     ( if ( = y1 0 )
                          ( begin
                             ((( draw-pixmap-posn "caballoBlanco.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                             ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "c1" ) x y casilla click ( + counter 1 ) turn ))
                          ( if ( = y1 1 )
                               ( begin
                                  ((( draw-pixmap-posn "reinaBlanca.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                  ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "db" ) x y casilla click ( + counter 1 ) turn ))
                               ( if ( = y1 2 )
                                    ( begin
                                       ((( draw-pixmap-posn "torreBlanca.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                       ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "t1" ) x y casilla click ( + counter 1 ) turn ))
                                    ( begin
                                       ((( draw-pixmap-posn "alfilBlanco.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                       ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "a1" ) x y casilla click ( + counter 1 ) turn )))))
                     ( if ( = y1 0 )
                          ( begin
                             ((( draw-pixmap-posn "caballoNegro.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                             ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "C1" ) x y casilla click ( + counter 1 ) turn ))
                          ( if ( = y1 1 )
                               ( begin
                                  ((( draw-pixmap-posn "reinaNegra.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                  ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "DN" ) x y casilla click ( + counter 1 ) turn ))
                               ( if ( = y1 2 )
                                    ( begin
                                       ((( draw-pixmap-posn "torreNegra.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                       ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "T1" ) x y casilla click ( + counter 1 ) turn ))
                                    ( begin
                                       ((( draw-pixmap-posn "alfilNegro.png" ) tablero ) ( make-posn ( * 75 x ) ( * 75 y )))
                                       ( PeonCoronation ( ChangeChar str ( - casilla 1 ) "A1" ) x y casilla click ( + counter 1 ) turn )))))))))
        ) #| fin funcion PeonCoronation
str: es el string en el que se va a evaluar si hay un peon coronado
"x" y "y": son las coordenadas del peon coronado
casilla: es la casilla del peon a cambiar
click: es el click en el que se selecciona la ficha que el usuario quiere
counter: indica cuando debe de devolver el string
turn: define el turno para saber el color de peon que va a coronar |#

( define ( StrY str y )
   ( substring str ( * 2 ( * y 8 )) ( + 16 ( * 2 ( * y 8 ))))
   ) #|fin funcion para encontar el string de una coordenada en y
str: es el string en el que se va a buscar el sring que va a devolver
y: es la cordenada de "y" en la cual se haya el string |# 


( define ( BuscarPeon counter str )
   ( if ( = counter ( string-length str ))
        #f
        ( if ( or ( string=? ( string ( string-ref str counter )) "p" )
                  ( string=? ( string ( string-ref str counter )) "P" ))
             ( begin
                #t
                counter )
             ( BuscarPeon ( + 2 counter ) str ))) 
   ) #| fin funcion BuscarPeon
counter: sirve para ver la posicion del peon o para ver si no esta
str: es el string en el que se va a buscar el peon |#                                             ;fin funciones para coronacion de peones

( define ( PositionFichas str index contadorFichas numeroFicha ficha )           ;inicio funciones para los jaques
        ( if ( = index ( string-length str ))
             -1
             ( if ( and ( string=? ( string ( string-ref str index )) ficha )         ; funcion que sirve para encontrar una ficha especifica
                        ( = contadorFichas numeroFicha ))
                  ( / index 2 )
                  ( if ( string=? ( string ( string-ref str index )) ficha )
                       ( PositionFichas str ( + 1 index ) ( + 1 contadorFichas ) numeroFicha ficha )
                       ( PositionFichas str ( + 1 index ) contadorFichas numeroFicha ficha ))))
   ) #| fin funcion PositionFichas
str: es el string en el que se va a buscar la ficha
index: va a devolver la posicion de la ficha si existe, sino devolvera -1
contadorFichas: va a contar el numero de fichas que han pasado
numeroFichas: es el numero de la ficha que se quiere encontar
ficha: es la ficha que se quiere encontarr |#


( define ( JaqueReyBlanco numberFicha tipoFicha str )

   ( define casilla1 ( PositionFichas str 0 1 numberFicha tipoFicha ))
   ( define casillaR ( PositionFichas str 0 1 1 "r" ))
   ( define rbx ( remainder casillaR 8 )) ( define rby ( quotient casillaR 8 ))
   ( define fichax ( remainder casilla1 8 )) ( define fichay ( quotient casilla1 8 ))
   
   ( if ( string=? tipoFicha "T" )
        ( if ( = casilla1 -1 )
             ( JaqueReyBlanco 1 "C" str )
       ( if ( TorreRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "C" )
        ( if ( = casilla1 -1 )
             ( JaqueReyBlanco 1 "A" str )
       ( if ( CaballoRestriction fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))   
   ( if ( string=? tipoFicha "A" )
        ( if ( = casilla1 -1 )
             ( JaqueReyBlanco 1 "D" str )
       ( if ( AlfilRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))             ;funcion que determina si el rey blanco esta en jaque
   ( if ( string=? tipoFicha "D" )
        ( if ( = casilla1 -1 )
             ( JaqueReyBlanco 1 "R" str )
       ( if ( ReinaRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "R" )
        ( if ( = casilla1 -1 )
             ( JaqueReyBlanco 1 "P" str )
       ( if ( ReyRestriction fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "P" )
        ( if ( = casilla1 -1 )
             #f
       ( if ( PeonBlackRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyBlanco ( + 1 numberFicha ) tipoFicha str )))
        void ))))))
   ) #| fin funcion jaque rey blanco
numberFicha: es el numero de ficha que se va a evaluar
tipoFicha: es el tipo de ficha que se va a evaluar
str: es el string en el que se va a evaluar si el rey esta en jaque |#

( define ( JaqueReyNegro numberFicha tipoFicha str )

   ( define casilla1 ( PositionFichas str 0 1 numberFicha tipoFicha ))
   ( define casillaR ( PositionFichas str 0 1 1 "R" ))
   ( define rbx ( remainder casillaR 8 )) ( define rby ( quotient casillaR 8 ))
   ( define fichax ( remainder casilla1 8 )) ( define fichay ( quotient casilla1 8 ))
   
   ( if ( string=? tipoFicha "t" )
        ( if ( = casilla1 -1 )
             ( JaqueReyNegro 1 "c" str )
       ( if ( TorreRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "c" )
        ( if ( = casilla1 -1 )
             ( JaqueReyNegro 1 "a" str )
       ( if ( CaballoRestriction fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))   
   ( if ( string=? tipoFicha "a" )
        ( if ( = casilla1 -1 )
             ( JaqueReyNegro 1 "d" str )
       ( if ( AlfilRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))                  ;funcion que determina si el rey negro esta en jaque
   ( if ( string=? tipoFicha "d" )
        ( if ( = casilla1 -1 )
             ( JaqueReyNegro 1 "r" str )
       ( if ( ReinaRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "r" )
        ( if ( = casilla1 -1 )
             ( JaqueReyNegro 1 "p" str )
       ( if ( ReyRestriction fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))
   ( if ( string=? tipoFicha "p" )
        ( if ( = casilla1 -1 )
             #f
       ( if ( PeonWhiteRestriction str fichax rbx fichay rby )
            #t
            ( JaqueReyNegro ( + 1 numberFicha ) tipoFicha str )))
        void ))))))
   ) #| fin funcion jaque rey negro
numberFicha: es el numero de ficha que se va a evaluar
tipoFicha: es el tipo de ficha que se va a evaluar
str: es el string en el que se va a evaluar si el rey esta en jaque |#

( define ( MateNegro str numberFicha tipoFicha x2 y2 )
   ( define casilla2 ( + x2 ( * y2 8 )))
   ( define casilla1 ( PositionFichas str 0 1 numberFicha tipoFicha ))
   ( define x1 ( remainder casilla1 8 ))
   ( define y1 ( quotient casilla1 8 ))
   ( if ( = x2 8 )
        ( MateNegro str numberFicha tipoFicha 0 ( + 1 y2 ))
        ( if ( = y2 8 )
             ( MateNegro str ( + 1 numberFicha ) tipoFicha 0 0 )
             ( if ( or ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "T" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "C" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "A" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "D" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "R" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "P" ))
                  ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )
                  
                  ( if ( string=? tipoFicha "T" )
                       ( if ( = casilla1 -1 )
                            ( MateNegro str 1 "C" 0 0 )
                            ( if ( and ( TorreRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "C" )
                       ( if ( = casilla1 -1 )
                            ( MateNegro str 1 "A" 0 0 )
                            ( if ( and ( CaballoRestriction x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "A" )
                       ( if ( = casilla1 -1 )
                            ( MateNegro str 1 "D" 0 0 )
                            ( if ( and ( AlfilRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "D" )
                         ( if ( = casilla1 -1 )
                            ( MateNegro str 1 "R" 0 0 )
                            ( if ( and ( ReinaRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "R" )
                       ( if ( = casilla1 -1 )
                            ( MateNegro str 1 "P" 0 0 )
                            ( if ( and ( ReyRestriction x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "P" )
                       ( if ( = casilla1 -1 )
                            #t
                            ( if ( and ( PeonBlackRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyNegro 1 "t" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateNegro str numberFicha tipoFicha ( + 1 x2 ) y2 )))void)))))))))
   ) #| fin funcion MateNegro
str: es el string en el que se va a evaluar
"x" y "y" son las coordenadas de la casilla que se esta evaluando
"contadorX" y "contadorY": define los limites para evaluar las casillas adecuadas
index: sirve para que la primera casilla a evaluar sea la superior izquierda |#

( define ( MateBlanco str numberFicha tipoFicha x2 y2 )
   ( define casilla2 ( + x2 ( * y2 8 )))
   ( define casilla1 ( PositionFichas str 0 1 numberFicha tipoFicha ))
   ( define x1 ( remainder casilla1 8 ))
   ( define y1 ( quotient casilla1 8 ))
   ( if ( = x2 8 )
        ( MateBlanco str numberFicha tipoFicha 0 ( + 1 y2 ))
        ( if ( = y2 8 )
             ( MateBlanco str ( + 1 numberFicha ) tipoFicha 0 0 )
             ( if ( or ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "t" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "c" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "a" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "d" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "r" )
                       ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "p" ))
                  ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )
                  
                  ( if ( string=? tipoFicha "t" )
                       ( if ( = casilla1 -1 )
                            ( MateBlanco str 1 "c" 0 0 )
                            ( if ( and ( TorreRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "c" )
                       ( if ( = casilla1 -1 )
                            ( MateBlanco str 1 "a" 0 0 )
                            ( if ( and ( CaballoRestriction x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "a" )
                       ( if ( = casilla1 -1 )
                            ( MateBlanco str 1 "d" 0 0 )
                            ( if ( and ( AlfilRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "d" )
                         ( if ( = casilla1 -1 )
                            ( MateBlanco str 1 "r" 0 0 )
                            ( if ( and ( ReinaRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "r" )
                       ( if ( = casilla1 -1 )
                            ( MateBlanco str 1 "p" 0 0 )
                            ( if ( and ( ReyRestriction x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))
                  ( if ( string=? tipoFicha "p" )
                       ( if ( = casilla1 -1 )
                            #t
                            ( if ( and ( PeonWhiteRestriction str x1 x2 y1 y2 )
                                       ( not ( JaqueReyBlanco 1 "T" ( ChangePosition str ( + 1 casilla1 ) ( + 1 casilla2 )))))
                                 #f
                                 ( MateBlanco str numberFicha tipoFicha ( + 1 x2 ) y2 )))void)))))))))
   ) #| fin funcion MateBlanco
str: es el string en el que se va a evaluar
"x" y "y" son las coordenadas de la casilla que se esta evaluando
"contadorX" y "contadorY": define los limites para evaluar las casillas adecuadas
index: sirve para que la primera casilla a evaluar sea la superior izquierda |#

( define ( Game turn str mouse1 mouse2 )                                                     ;inicio funcion principal
             
   ( define x1 ( quotient ( posn-x ( mouse-click-posn mouse1 )) 75 ))
   ( define y1 ( quotient ( posn-y ( mouse-click-posn mouse1 )) 75 ))    ;se separan las coordenadas de los 2 clicks
   ( define x2 ( quotient ( posn-x ( mouse-click-posn mouse2 )) 75 ))
   ( define y2 ( quotient ( posn-y ( mouse-click-posn mouse2 )) 75 ))
   
   ( define casilla1 ( + ( * y1 8 ) ( + x1 1 )))    ;se definen las casillas de cada click
   ( define casilla2 ( + ( * y2 8 ) ( + x2 1 )))
   
   ( define actualTable ( ChangePosition str casilla1 casilla2 )) ;se define la tabla con el string cambiado
   ( display actualTable )
   ( newline )
   (newline)

   (( draw-solid-rectangle tablero ) ( make-posn 650 375 ) 200 70 "white" )
   (( draw-string tablero ) (make-posn 720 415 ) ( if ( = turn 1 ) "Turno: 1" "Turno: 2" ) "black" )
   
   ( if ( and ( >= ( posn-x ( mouse-click-posn mouse1 )) 650 ) ( <= ( posn-x ( mouse-click-posn mouse1 )) 850 )             
              ( >= ( posn-y ( mouse-click-posn mouse1 )) 475 ) ( <= ( posn-y ( mouse-click-posn mouse1 )) 545 ))
        ( begin
           (( draw-solid-rectangle tablero ) ( make-posn 0 0 ) 900 600 "black" )
           ((draw-string tablero ) (make-posn 415 280 ) "FIN DEL JUEGO" "yellow" )
            ( sleep 2 )
            ( exit ))
   ( if ( or ( > x2 7 )
             ( > x1 7 ))       ;restriccion de tablero
        ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero ))
        
   ( begin
      ( if ( = turn  1 )
           ( if ( JaqueReyBlanco 1 "T" actualTable )
                ( begin
                   ( BadPlay 3 )
                   ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))
                   
        ( if ( or ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "p" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "t" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "c" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "a" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "d" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "r" ))
             ( if ( and ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "p" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "t" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "c" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "a" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "d" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "r" )))
   ( begin
      ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "p" )
                 ( PeonWhiteRestriction str x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ( if ( BuscarPeon 0 ( StrY actualTable 0 ))
                   ( begin
                      ((( draw-pixmap-posn "peonBlanco.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 )))
                      ( DecorationCoronation )
                      ( Game ( if ( = turn 1 ) 2 1 )
                             ( PeonCoronation actualTable x2 y2 casilla2 ( get-mouse-click tablero ) 0 1 )
                             ( get-mouse-click tablero ) ( get-mouse-click tablero )))
                   ((( draw-pixmap-posn "peonBlanco.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 )))))
      ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "t" )
                 ( TorreRestriction str x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ((( draw-pixmap-posn "torreBlanca.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
      ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "c" )
              ( CaballoRestriction x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ((( draw-pixmap-posn "caballoBlanco.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
      ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "a" )
                 ( AlfilRestriction str x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ((( draw-pixmap-posn "alfilBlanco.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
      ( if ( and ( string=? ( FindString str x1 y1 ) "rb" )
                 ( ReyRestriction x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ((( draw-pixmap-posn "reyBlanco.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
      ( if ( and ( string=? ( FindString str x1 y1 ) "db" )
                 ( ReinaRestriction str x1 x2 y1 y2 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
              (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
              ((( draw-pixmap-posn "reinaBlanca.png" ) tablero ) ( make-posn (* 75 x2 ) ( * 75 y2 ))))
           ;else
           ( begin
              ( BadPlay 1 )
              ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))))))))
      (( draw-solid-rectangle tablero ) ( make-posn 650 375 ) 200 70 "white" )
      ((draw-string tablero ) (make-posn 720 415 ) ( if ( = turn 1 ) "Turno: 2" "Turno: 1") "black" ))
   ;else ( if ( not ( string=? 
   ( begin
      ( BadPlay 1 )
      ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))) ;fin ( if ( not ( string=? 
   ;else ( if ( string=? 
    ( begin
      ( BadPlay 2 )
      ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero ))))) ;fin ( if ( string=? 
        
        ;else ( if ( = turn  1 )

           ( if ( JaqueReyNegro 1 "t" actualTable )
                ( begin
                   ( BadPlay 3 )
                   ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))
        ( if ( or ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "P" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "T" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "C" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "A" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "D" )
                  ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "R" ))
             ( if ( and ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "P" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "T" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "C" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "A" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "D" ))
                        ( not ( string=? ( string ( string-ref ( FindString str x2 y2 ) 0 )) "R" )))
                  ( begin         
                     ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "P" )
                                ( PeonBlackRestriction str x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ( if ( BuscarPeon 0 ( StrY actualTable 7 ))
                                  ( begin
                                     ((( draw-pixmap-posn "peonNegro.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 )))
                                     ( DecorationCoronation )
                                     ( Game ( if ( = turn 1 ) 2 1 )
                                            ( PeonCoronation actualTable x2 y2 casilla2 ( get-mouse-click tablero ) 0 2 )
                                            ( get-mouse-click tablero ) ( get-mouse-click tablero )))
                                  ((( draw-pixmap-posn "peonNegro.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 )))))
                     ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "T" )
                                ( TorreRestriction str x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ((( draw-pixmap-posn "torreNegra.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
                     ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "C" )
                                ( CaballoRestriction x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ((( draw-pixmap-posn "caballoNegro.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
                     ( if ( and ( string=? ( string ( string-ref ( FindString str x1 y1 ) 0 )) "A" )
                                ( AlfilRestriction str x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ((( draw-pixmap-posn "alfilNegro.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
                     ( if ( and ( string=? ( FindString str x1 y1 ) "RN" )
                                ( ReyRestriction x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ((( draw-pixmap-posn "reyNegro.png" ) tablero ) ( make-posn ( * 75 x2 ) ( * 75 y2 ))))
                     ( if ( and ( string=? ( FindString str x1 y1 ) "DN" )
                                ( ReinaRestriction str x1 x2 y1 y2 ))
                          ( begin
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x1 )( * 75 y1 )) 75 75 ( Blocks casilla1 ( * 75 x1 )( * 75 y1 )))
                             (( draw-solid-rectangle tablero ) ( make-posn ( * 75 x2 )( * 75 y2 )) 75 75 ( Blocks casilla2 ( * 75 x2 )( * 75 y2 )))
                             ((( draw-pixmap-posn "reinaNegra.png" ) tablero ) ( make-posn (* 75 x2 ) ( * 75 y2 ))))
                          ;else
                          ( begin
                             ( BadPlay 1 )
                             ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))))))))
                     (( draw-solid-rectangle tablero ) ( make-posn 650 375 ) 200 70 "white" )
                     ((draw-string tablero ) (make-posn 720 415 ) ( if ( = turn 1 ) "Turno: 2" "Turno: 1") "black" )) ;fin begin
                  ;else ( if ( not ( string=? 
                  ( begin
                     ( BadPlay 1 )
                     ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))) ;fin ( if ( not ( string=? 
             ;else ( if ( string=? 
             ( begin
                ( BadPlay 2 )
                ( Game turn str ( get-mouse-click tablero ) ( get-mouse-click tablero )))
             ) ;fin ( if ( string=?
        )
        ) ;fin ( = turn  1 )

      ( if ( or ( MateNegro actualTable 1 "T" 0 0 )
                ( MateBlanco actualTable 1 "t" 0 0 ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn 650 150 )  200 100 "Black" )
              ((draw-string tablero ) (make-posn 690 210 ) "Jaque Mate" "red" )
              ( sleep 2 )
              )
        ( if ( or ( JaqueReyBlanco 1 "T" actualTable )
                ( JaqueReyNegro 1 "t" actualTable ))
           ( begin
              (( draw-solid-rectangle tablero ) ( make-posn 650 200 ) 200 100 "white" )
              ((draw-string tablero ) (make-posn 725 260 ) "Jaque" "black" )
              ( sleep 1 )
              (( draw-solid-rectangle tablero ) ( make-posn 650 200 ) 200 100 "sienna" ))
           void ))
       
      ( Game ( if ( = turn 1 ) 2 1 ) actualTable ( get-mouse-click tablero ) ( get-mouse-click tablero ))))
   ) ;final fin del juego
   ) ;fin funcion Game
#| turn: representa el turno que esta jugando
str: es el string que va cambiando durante el juego
mouse1 y mouse2: representan los clicks que da el usuario |#

( printf "EL PROGRAMA FUNCIONA UTILIZANDO 2 CLICKS EN ORDEN.\n\nEL PRIMER TURNO LE CORRESPONDE AL JUGADOR QUE
ESTE USANDO LAS FICHAS BLANCAS.\n\nQUE SE DIVIERTA." )
(( draw-solid-rectangle tablero ) ( make-posn 600 0 ) 300 600 "sienna" )
(( draw-solid-rectangle tablero ) ( make-posn 650 375 ) 200 70 "white" )
(( draw-solid-rectangle tablero ) ( make-posn 650 475 ) 200 70 "white" )
((draw-string tablero ) (make-posn 710 515 ) "TERMINAR" "black" )
((draw-string tablero ) (make-posn 720 415 ) "Turno:1" "black" )
( Board 0 1 75 0 0 75 )
( Fichas 1 0 0 )
( Game 1 strTable ( get-mouse-click tablero ) ( get-mouse-click tablero )) ;se llama el juego