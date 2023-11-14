(ns interprete-racket.core
      (:import (java.io FileNotFoundException PushbackReader)))

(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])

(defn spy
      ([x] (do (prn x) x))
      ([msg x] (do (print msg) (print ": ") (prn x) x))
      )

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-enter!)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)

; Funciones auxiliares propias
(declare restaurar-bool-aux)
(declare fnc-append-aux)
(declare bool-a-simbolo)
(declare simbolo-a-bool)
(declare es-igual?)
(declare fnc-equal-wrapper)
(declare sumar-parentesis)
(declare buscar-indice)
(declare fnc-sumar-aux)
(declare fnc-restar-aux)
(declare fnc-comparar-aux)
(declare fnc-comparar-reduce)
(declare crear-lambda)
(declare evaluar-or-aux)


(defn -main []
      (repl))


(defn repl
      "Inicia el REPL de Racket."
      ([]
       (println "Interprete de Racket en Clojure")
       (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2023") (prn) (flush)
       (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
                   'display 'display 'enter! 'enter! 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
                   'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list?
                   'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
                   'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
                   (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>=) ""))
      ([amb ns]
       (if (empty? ns) (print ns) (pr ns)) (print "> ") (flush)
       (try
             (let [renglon (leer-entrada)]                       ; READ
                   (if (= renglon "")
                         (repl amb ns)
                         (let [str-corregida (proteger-bool-en-str renglon),
                               cod-en-str (read-string str-corregida),
                               cod-corregido (restaurar-bool cod-en-str),
                               res (evaluar cod-corregido amb),     ; EVAL
                               res1 (first res),
                               res2 (second res)]
                               (cond
                                     (nil? res2) 'Goodbye!              ; Si el ambiente del resultado es 'nil', es porque se ha evaluado (exit)
                                     (and (list? res1)                  ; En tal caso, sale del REPL devolviendo Goodbye!.
                                          (= (first res1) 'ns))
                                     (repl res2 (second res1))
                                     :else
                                     (do (imprimir res1)             ; PRINT
                                         (repl res2 ns))))))         ; LOOP (Se llama a si misma con el nuevo ambiente)
             (catch Exception e                                  ; PRINT (si se lanza una excepcion)
                   (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
                   (repl amb ns)))))                     ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
      "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
      [expre amb]
      (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
            (list expre amb)                                      ; de lo contrario, evaluarla
            (cond
                  (not (seq? expre)) (evaluar-escalar expre amb)
                  (= (first expre) 'define) (evaluar-define expre amb)
                  (= (first expre) 'if) (evaluar-if expre amb)
                  (= (first expre) 'or) (evaluar-or expre amb)
                  (= (first expre) 'set!) (evaluar-set! expre amb)
                  (= (first expre) 'eval) (evaluar-eval expre amb)
                  (= (first expre) 'exit) (evaluar-exit expre amb)
                  (= (first expre) 'cond) (evaluar-cond expre amb)
                  (= (first expre) 'quote) (evaluar-quote expre amb)
                  (= (first expre) 'lambda) (evaluar-lambda expre amb)
                  (= (first expre) 'enter!) (evaluar-enter! expre amb)
                  ;
                  ;
                  ;
                  ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada
                  ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
                  ;
                  ;
                  ;

                  :else (let [res-eval-1 (evaluar (first expre) amb),
                              res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
                              (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))


(defn aplicar
      "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
      ([fnc lae amb]
       (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
      ([resu1 resu2 fnc lae amb]
       (cond
             (error? resu1) (list resu1 amb)
             (error? resu2) (list resu2 amb)
             (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
             :else (aplicar-lambda fnc lae amb))))


(defn aplicar-lambda
      "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
      [fnc lae amb]
      (cond
            (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
            (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
            :else (aplicar-lambda-multiple fnc lae amb)))


(defn aplicar-lambda-simple
      "Evalua un lambda `fnc` con un cuerpo simple"
      [fnc lae amb]
      (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (= (first %) 'lambda)))
                                       %
                                       (list 'quote %)) lae),
            nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes)),
            mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares)))),
            cuerpo (first (nnext fnc)),
            expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (= (ffirst cuerpo) 'lambda))
                        (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                        (postwalk-replace mapa cuerpo))]
            (evaluar expre amb)))


(defn aplicar-lambda-multiple
      "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
      [fnc lae amb]
      (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
               lae
               (second (aplicar-lambda-simple fnc lae amb))))


(defn aplicar-funcion-primitiva
      "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
      [fnc lae amb]
      (cond
            (= fnc '<) (fnc-menor lae)
            (= fnc '>) (fnc-mayor lae)
            (= fnc '>=) (fnc-mayor-o-igual lae)
            (= fnc '+) (fnc-sumar lae)
            (= fnc '-) (fnc-restar lae)

            (= fnc 'equal?) (fnc-equal? lae)
            (= fnc 'append) (fnc-append lae)
            (= fnc 'car) (fnc-car lae)
            (= fnc 'cdr) (fnc-cdr lae)
            (= fnc 'cons) (fnc-cons lae)
            (= fnc 'display) (fnc-display lae)
            (= fnc 'env) (fnc-env lae amb)
            (= fnc 'length) (fnc-length lae)
            (= fnc 'list) (fnc-list lae)
            (= fnc 'list?) (fnc-list? lae)
            (= fnc 'newline) (fnc-newline lae)
            (= fnc 'not) (fnc-not lae)
            (= fnc 'null?) (fnc-null? lae)
            (= fnc 'read) (fnc-read lae)
            (= fnc 'reverse) (fnc-reverse lae)

            :else (generar-mensaje-error :wrong-type-apply fnc)))


(defn fnc-car
      "Devuelve el primer elemento de una lista."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
            (cond
                  (error? ari) ari
                  (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
                  :else (first arg1))))


(defn fnc-cdr
      "Devuelve una lista sin su 1ra. posicion."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
            (cond
                  (error? ari) ari
                  (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
                  :else (rest arg1))))


(defn fnc-cons
      "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
      [lae]
      (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
            (cond
                  (error? ari) ari
                  (not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
                  :else (cons arg1 arg2))))


(defn fnc-display
      "Imprime un elemento por la termina/consola y devuelve #<void>."
      [lae]
      (let [cant-args (count lae), arg1 (first lae)]
            (case cant-args
                  1 (do (print arg1) (flush) (symbol "#<void>"))
                  2 (generar-mensaje-error :io-ports-not-implemented 'display)
                  (generar-mensaje-error :wrong-number-args-prim-proc 'display))))


(defn fnc-env
      "Devuelve el ambiente."
      [lae amb]
      (let [ari (controlar-aridad-fnc lae 0 'env)]
            (if (error? ari)
                  ari
                  amb)))


(defn fnc-length
      "Devuelve la longitud de una lista."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
            (cond
                  (error? ari) ari
                  (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
                  :else (count arg1))))


(defn fnc-list
      "Devuelve una lista formada por los args."
      [lae]
      (if (< (count lae) 1)
            ()
            lae))


(defn fnc-list?
      "Devuelve #t si un elemento es una lista. Si no, #f."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
            (if (error? ari)
                  ari
                  (if (seq? arg1)
                        (symbol "#t")
                        (symbol "#f")))))


(defn fnc-newline
      "Imprime un salto de linea y devuelve #<void>."
      [lae]
      (let [cant-args (count lae)]
            (case cant-args
                  0 (do (newline) (flush) (symbol "#<void>"))
                  1 (generar-mensaje-error :io-ports-not-implemented 'newline)
                  (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))


(defn fnc-not
      "Niega el argumento."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'not)]
            (if (error? ari)
                  ari
                  (if (= (first lae) (symbol "#f"))
                        (symbol "#t")
                        (symbol "#f")))))


(defn fnc-null?
      "Devuelve #t si un elemento es ()."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'null?)]
            (if (error? ari)
                  ari
                  (if (= (first lae) ())
                        (symbol "#t")
                        (symbol "#f")))))


(defn fnc-reverse
      "Devuelve una lista con los elementos de `lae` en orden inverso."
      [lae]
      (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
            (cond
                  (error? ari) ari
                  (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
                  :else (reverse arg1))))


(defn controlar-aridad-fnc
      "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
       Si no, devuelve una lista con un mensaje de error."
      [lae val-esperado fnc]
      (if (= val-esperado (count lae))
            val-esperado
            (generar-mensaje-error :wrong-number-args-prim-proc fnc)))


(defn imprimir
      "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
      con comillas) y devuelve su valor. Muestra errores sin parentesis."
      ([elem]
       (cond
             (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
             (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
             :else (do (prn elem) (flush) elem)))
      ([lis orig]
       (cond
             (nil? lis) (do (prn) (flush) orig)
             :else (do (pr (first lis))
                       (print " ")
                       (imprimir (next lis) orig)))))


(defn revisar-fnc
      "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
      [lis] (if (error? lis) lis nil))


(defn revisar-lae
      "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
      [lis] (first (remove nil? (map revisar-fnc (filter seq? lis)))))


(defn evaluar-cond
      "Evalua una expresion `cond`."
      [expre amb]
      (if (= (count expre) 1) ; si es el operador solo
            (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
            (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
                  (if (empty? res)
                        (evaluar-clausulas-de-cond expre (next expre) amb)
                        (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))


(defn evaluar-clausulas-de-cond
      "Evalua las clausulas de cond."
      [expre lis amb]
      (if (nil? lis)
            (list (symbol "#<void>") amb) ; cuando ninguna fue distinta de #f
            (let [res-eval (if (not= (ffirst lis) 'else)
                                 (evaluar (ffirst lis) amb)
                                 (if (nil? (next lis))
                                       (list (symbol "#t") amb)
                                       (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
                  (cond
                        (error? (first res-eval)) res-eval
                        (= (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval))
                        :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))


(defn evaluar-secuencia-en-cond
      "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
      [lis amb]
      (if (nil? (next lis))
            (evaluar (first lis) amb)
            (let [res-eval (evaluar (first lis) amb)]
                  (if (error? (first res-eval))
                        res-eval
                        (recur (next lis) (second res-eval))))))


(defn evaluar-eval
      "Evalua una expresion `eval`."
      [expre amb]
      (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
            (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
            (let [arg (second expre)]
                  (if (and (seq? arg) (= (first arg) 'quote))
                        (evaluar (second arg) amb)
                        (evaluar arg amb)))))


(defn evaluar-exit
      "Sale del interprete de Racket."
      [expre amb]
      (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
            (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
            (list nil nil)))


(defn evaluar-lambda
      "Evalua una expresion `lambda`."
      [expre amb]
      (cond
            (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
            (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
            (not (seq? (second expre)))
            (list (generar-mensaje-error :bad-params 'lambda expre) amb)
            :else (list expre amb)))


(defn evaluar-enter!
      "Carga en el ambiente un archivo 'expre' de código Racket"
      [expre amb]
      (cond
            (= (count expre) 1) ; si es el operador solo
            (list (generar-mensaje-error :wrong-number-args (symbol "enter!: bad syntax...")) amb)
            (= (second expre) (symbol "#f"))
            (list (list 'ns "") amb)
            :else (list (list 'ns (second expre)) (cargar-arch amb (second expre)))))


(defn cargar-arch
      "Carga el contenido de un archivo"
      ([amb arch]
       (let [res (evaluar arch amb),
             nom-original (first res),
             nuevo-amb (second res)]
             (if (error? nom-original)
                   (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
                   (let [nom-a-usar (generar-nombre-arch nom-original)]
                         (if (error? nom-a-usar)
                               (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
                               (let [tmp (try
                                               (slurp nom-a-usar)
                                               (catch FileNotFoundException _
                                                     (generar-mensaje-error :file-not-found)))]
                                     (if (error? tmp)
                                           (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
                                           (do (spit "rkt-temp" (proteger-bool-en-str (clojure.string/replace tmp #"#lang racket" "")))
                                               (let [ret (with-open [in (PushbackReader. (reader "rkt-temp"))]
                                                               (binding [*read-eval* false]
                                                                     (try
                                                                           (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                                                           (catch Exception e
                                                                                 (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                                                     (do (delete-file "rkt-temp" true) ret))))))))))
      ([amb in nom-orig nom-usado]
       (try
             (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
             (catch Exception _
                   amb))))


(defn generar-nombre-arch
      "Dada una entrada la convierte en un nombre de archivo de Racket valido."
      [nom]
      (if (not (string? nom))
            (generar-mensaje-error :wrong-type-arg1 'string-length nom)
            (let [n (lower-case nom)]
                  (if (nombre-arch-valido? n)
                        n
                        (str n ".rkt")))))    ; Agrega '.rkt' al final


(defn nombre-arch-valido?
      "Chequea que el string sea un nombre de archivo .rkt valido."
      [nombre] (and (> (count nombre) 4) (ends-with? nombre ".rkt")))


(defn evaluar-quote
      "Evalua una expresion `quote`."
      [expre amb]
      (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
            (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
            (list (second expre) amb)))


(defn generar-mensaje-error
      "Devuelve un mensaje de error expresado como lista."
      ([cod]
       (case cod
             :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
             :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
             ()))
      ([cod fnc]
       (cons (symbol ";ERROR:")
             (case cod
                   :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
                   :error (list (symbol (str fnc)))
                   :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
                   :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
                   :unbound-variable (list 'unbound (symbol "variable:") fnc)
                   :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
                   :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
                   :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
                   :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
                   ())))
      ([cod fnc nom-arg]
       (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
                                      (case cod
                                            :bad-body (list 'bad 'body nom-arg)
                                            :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
                                            :bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
                                            :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
                                            :bad-variable (list 'bad 'variable nom-arg)
                                            :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
                                            :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
                                            :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
                                            :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
                                            ())))))


; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE RACKET (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):

; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"
; user=> (leer-entrada)
; (+ 1 3) 3)
; ;WARNING: unexpected ")"#<input-port 0>
; "(+ 1 3) 3)"
(defn leer-entrada
      "Lee una cadena desde la terminal/consola. Si contiene parentesis de menos al presionar Enter/Intro,
      se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, se la
      devuelve completa (si corresponde, advirtiendo previamente que hay parentesis de mas)."
      ([] (leer-entrada 0))
      ([parentesis]
       (
             let [cadena (st/trim (read-line))
                  cant-parentesis (verificar-parentesis cadena)
                  diferencia (+ cant-parentesis parentesis)
                  ]
             (cond
                   (= 0 diferencia) cadena
                   (= 1 diferencia) (str cadena " " (leer-entrada diferencia))
                   :else (do (imprimir (generar-mensaje-error :warning-paren)) cadena)
                   )
             )
       )
      )

; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0
(defn verificar-parentesis
      "Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."
      [cadena]
      (reduce sumar-parentesis 0 cadena)
      )

(defn sumar-parentesis [suma valor]
      (if (< suma 0)
            (reduced -1)
            (case valor
                  \) (dec suma)
                  \( (inc suma)
                  suma
                  )
            )
      )

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(defn actualizar-amb
      "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor.
      Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva informacion."
      [ambiente clave valor]
      (cond
        (error? valor) ambiente
        (error? (buscar ambiente clave)) (concat ambiente (list clave valor))
        :else (let [posicion (buscar-indice clave ambiente)]
                  (concat (take (dec posicion) ambiente) (list clave valor) (drop (inc posicion) ambiente)))
        )
      )

(defn buscar-indice [clave ambiente]
      (inc (first (keep-indexed #(if (and (even? %1) (= clave %2)) %1) ambiente)))
  )


; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)
(defn buscar
      "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
       y devuelve el valor asociado. Devuelve un error: unbound-variable si no la encuentra."
      [ambiente clave]
       (cond
            (empty? ambiente) (generar-mensaje-error :unbound-variable clave)
            (= (first ambiente) clave) (second ambiente)
            :else (buscar (drop 2 ambiente) clave)
            )
      )



; user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true
(defn error?
      "Devuelve true o false, segun sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."
      [valor]
      (and (seq? valor) (or (= (first valor) (symbol ";ERROR:")) (= (first valor) (symbol ";WARNING:"))))
      )

; user=> (proteger-bool-en-str "(or #f #t)")
; "(or %f %t)"
; user=> (proteger-bool-en-str "(and (or #f #t) #t)")
; "(and (or %f %t) %t)"
; user=> (proteger-bool-en-str "")
; ""
(defn proteger-bool-en-str
      "Cambia, en una cadena, #t por %t y #f por %f, para poder aplicarle read-string."
      [cadena] (st/replace (st/replace cadena #"#t" "%t") #"#f" "%f")
      )

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
(defn restaurar-bool
      "Cambia, en un codigo leido con read-string, %t por #t y %f por #f."
      [codigo] (map restaurar-bool-aux codigo)
      )

(defn restaurar-bool-aux [elemento]
      (cond
            (seq? elemento) (restaurar-bool elemento)
            (symbol? elemento) (symbol (st/replace (st/replace (str elemento) #"%t" "#t") #"%f" "#f"))
            :else elemento
            )
      )


; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
(defn fnc-append
      "Devuelve el resultado de fusionar listas."
      [lista]
      (fnc-append-aux lista ())
      )

(defn fnc-append-aux [lista resultado]
      (cond
            (empty? lista) resultado
            (not (seq? (first lista))) (generar-mensaje-error :wrong-type-arg 'append (first lista))
            :else (fnc-append-aux (rest lista) (concat resultado (first lista)))
            )
      )

; user=> (fnc-equal? ())
; #t
; user=> (fnc-equal? '(A))
; #t
; user=> (fnc-equal? '(A a))
; #t
; user=> (fnc-equal? '(A a A))
; #t
; user=> (fnc-equal? '(A a A a))
; #t
; user=> (fnc-equal? '(A a A B))
; #f
; user=> (fnc-equal? '(1 1 1 1))
; #t
; user=> (fnc-equal? '(1 1 2 1))
; #f
(defn fnc-equal?
      "Compara elementos. Si son iguales, devuelve #t. Si no, #f."
      [lista]
      (cond
            (or (empty? lista) (= (count lista) 1)) (bool-a-simbolo true)
            :else (bool-a-simbolo (reduce fnc-equal-wrapper lista))
            )
      )

(defn fnc-equal-wrapper [x y]
      (if (es-igual? x y) x (reduced false))
      )

(defn es-igual? [x y]
      (cond
            (and (number? x) (number? y)) (= x y)
            (or (= (type x) (type y)) (and (seq? x) (seq? y))) (= (lower-case (str x)) (lower-case (str y)))
            :else false
            )
      )

(defn bool-a-simbolo [bool]
      (if bool (symbol "#t") (symbol "#f"))
      )

(defn simbolo-a-bool [simbolo]
      (cond
            (= simbolo (symbol "#t")) true
            (or (seq? simbolo) (number? simbolo)) simbolo
            :else false
            )
      )

; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
(defn fnc-read
      "Devuelve la lectura de un elemento de Racket desde la terminal/consola."
      [lista]
      (cond
            (empty? lista) (read-string (leer-entrada))
            (= 1 (count lista)) (generar-mensaje-error :io-ports-not-implemented 'read)
            :else (generar-mensaje-error :wrong-number-args-prim-proc 'read)
            )
      )

; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)
(defn fnc-sumar
      "Suma los elementos de una lista."
      [lista]
      (cond
            (empty? lista) 0
            (= 1 (count lista)) (first lista)
            :else (reduce fnc-sumar-aux lista)
            )
      )

(defn fnc-sumar-aux [x y]
      (cond
            (not (number? x)) (reduced (generar-mensaje-error :wrong-type-arg1 '+ x))
            (not (number? y)) (reduced (generar-mensaje-error :wrong-type-arg2 '+ y))
            :else (+ x y)
            )
      )


; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)
(defn fnc-restar
      "Resta los elementos de un lista."
      [lista]
      (cond
            (empty? lista) (generar-mensaje-error :wrong-number-args-oper '-)
            (= 1 (count lista)) (- (first lista))
            :else (reduce fnc-restar-aux lista)
            )
      )

(defn fnc-restar-aux [x y]
      (cond
            (not (number? x)) (reduced (generar-mensaje-error :wrong-type-arg1 '- x))
            (not (number? y)) (reduced (generar-mensaje-error :wrong-type-arg2 '- y))
            :else (- x y)
            )
      )

; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)
(defn fnc-menor
      "Devuelve #t si los numeros de una lista estan en orden estrictamente creciente; si no, #f."
      [lista]
      (fnc-comparar-aux lista < '<)
      )

(defn fnc-comparar-aux [lista fun-comparacion literal-comparacion]
      (let [resultado (cond
                            (empty? lista) true
                            :else (reduce (partial fnc-comparar-reduce fun-comparacion literal-comparacion) lista)
                            )]
            (cond
                  (seq? resultado) resultado
                  :else (bool-a-simbolo resultado)
                  )
            )
      )

(defn fnc-comparar-reduce [fun-comparacion literal-comparacion x y]
      (cond
            (not (number? x)) (reduced (generar-mensaje-error :wrong-type-arg1 literal-comparacion x))
            (not (number? y)) (reduced (generar-mensaje-error :wrong-type-arg2 literal-comparacion y))
            (not (fun-comparacion x y)) (reduced false)
            :else y
            )
      )

; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)
(defn fnc-mayor
      "Devuelve #t si los numeros de una lista estan en orden estrictamente decreciente; si no, #f."
      [lista]
      (fnc-comparar-aux lista > '>)
      )

; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: >=: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: >=: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: >=: Wrong type in arg2 A)
(defn fnc-mayor-o-igual
      "Devuelve #t si los numeros de una lista estan en orden decreciente; si no, #f."
      [lista]
      (fnc-comparar-aux lista >= '>=)
      )

; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "chau" '(x 6 y 11 z "hola"))
; ("chau" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))
(defn evaluar-escalar
      "Evalua una expresion escalar. Devuelve una lista con el resultado y un ambiente."
      [escalar ambiente]
      (cond
            (and (symbol? escalar) (not (= escalar (symbol "#<void>")))) (list (buscar ambiente escalar) ambiente)
            :else (list escalar ambiente)
            )
      )

; user=> (evaluar-define '(define x 2) '(x 1))
; (#<void> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<void> (x 1 f (lambda (x) (+ x 1))))
; user=> (evaluar-define '(define) '(x 1))
; ((;ERROR: define: missing or extra expression (define)) (x 1))
; user=> (evaluar-define '(define x) '(x 1))
; ((;ERROR: define: missing or extra expression (define x)) (x 1))
; user=> (evaluar-define '(define x 2 3) '(x 1))
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))
; user=> (evaluar-define '(define ()) '(x 1))
; ((;ERROR: define: missing or extra expression (define ())) (x 1))
; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
(defn evaluar-define
      "Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
      [expresion ambiente]
      (cond
            (<= (count expresion) 2) (list (generar-mensaje-error :missing-or-extra 'define expresion) ambiente)
            (symbol? (second expresion)) (cond
                                               (= 3 (count expresion)) (list (symbol "#<void>") (actualizar-amb ambiente (second expresion) (first (evaluar (last expresion) ambiente))))
                                               :else (list (generar-mensaje-error :missing-or-extra 'define expresion) ambiente))
            (and (seq? (second expresion)) (not (empty? (second expresion)))) (cond
                                                                                    (and (>= (count expresion) 3) (seq? (nth expresion 2)))
                                                                                    (list (symbol "#<void>") (actualizar-amb ambiente (first (second expresion)) (crear-lambda (rest (second expresion)) (drop 2 expresion))))
                                                                                    )
            :else (list (generar-mensaje-error :bad-variable 'define expresion) ambiente))
      )

(defn crear-lambda
      [parametros expresion]
      (cons 'lambda (cons parametros expresion))
      )

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
(defn evaluar-if
      "Evalua una expresion `if`. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
      [expresion ambiente]
      (let [cant-elementos (count expresion)]
        (cond
          (<= cant-elementos 2) (list (generar-mensaje-error :missing-or-extra 'if expresion) ambiente)
          (>= cant-elementos 5) (list (generar-mensaje-error :missing-or-extra 'if expresion) ambiente)
          (= cant-elementos 3) (if (simbolo-a-bool (first (evaluar (second expresion) ambiente))) (evaluar (nth expresion 2) ambiente) (list (symbol "#<void>") ambiente))
          (= cant-elementos 4) (evaluar (if (simbolo-a-bool (first (evaluar (second expresion) ambiente))) (nth expresion 2) (nth expresion 3)) ambiente)
          )
        )
  )

; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
(defn evaluar-or
      "Evalua una expresion `or`.  Devuelve una lista con el resultado y un ambiente."
      [expresion ambiente]
      (let [lista (rest expresion)]
            (if (empty? lista) (list (symbol "#f") ambiente)
                               (list (reduce (partial evaluar-or-aux ambiente) (symbol "#f") lista) ambiente)
                               )
            )
      )

(defn evaluar-or-aux [ambiente resultado valor]
      (let [evaluado (first (evaluar valor ambiente))]
            (cond
                  (not (= (symbol "#f") evaluado)) (reduced evaluado)
                  :else resultado
                  )
            )
      )

; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<void> (x 1))
; user=> (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())
; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))
; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))
; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))
(defn evaluar-set!
      "Evalua una expresion `set!`. Devuelve una lista con el resultado y un ambiente actualizado con la redefinicion."
      [expresion ambiente]
      (cond
            (not (= 3 (count expresion))) (list (generar-mensaje-error :missing-or-extra 'set! expresion) ambiente)
            (not (symbol? (second expresion))) (list (generar-mensaje-error :bad-variable 'set! (second expresion)) ambiente)
            (error? (buscar ambiente (second expresion))) (list (generar-mensaje-error :unbound-variable (second expresion)) ambiente)
            :else (list (symbol "#<void>") (actualizar-amb ambiente (second expresion) (first (evaluar (last expresion) ambiente))))
            )
      )

; Al terminar de cargar el archivo en el REPL de Clojure, se debe devolver true.
true
