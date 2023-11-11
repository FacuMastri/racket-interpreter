(ns interprete-racket.core-test
  (:require [clojure.test :refer :all]
            [interprete-racket.core :refer :all]))


(deftest leer-entrada-test
  (testing "Test de leer-entrada"
    ; Observacion: puede ser que las pruebas fallen si se ejecutan en Windows. Cambiar los \n por \r\n
    (is (= (with-in-str "(hola\nmundo)" (leer-entrada)) "(hola mundo)"))
    (is (= (with-in-str "123" (leer-entrada)) "123"))
    (is (= (with-in-str "123\n" (leer-entrada)) "123"))
    (is (= (with-in-str "(+ 1 3) 3)" (leer-entrada)) "(+ 1 3) 3)"))
    )
  (testing "Test de efectos colaterales"
    (is (= (with-out-str (with-in-str "(+ 1 3) 3)" (leer-entrada))) ";WARNING: unexpected \")\"#<input-port 0> \n"))
    )
  )

(deftest verificar-parentesis-test
  (testing "Test de verificar-parentesis"
    (is (= (verificar-parentesis "(hola 'mundo") '1))
    (is (= (verificar-parentesis "(hola '(mundo)))"), '-1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7)"), '-1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7) 9)"), '-1))
    (is (= (verificar-parentesis "(hola '(mundo) )"), '0))
    )
  )

(deftest error?-test
  (testing "Test de error?"
    (is (error? (list (symbol ";ERROR:") 'mal 'hecho)))
    (is (not (error? (list 'mal 'hecho)))
    (is (error? (list (symbol ";WARNING:") 'mal 'hecho))))
    (is (not (error? (list 'no 'error))))
    (is (not (error? 'no-es-lista)))
    (is (not (error? nil)))
    ))

(deftest buscar-test
  (testing "Test de buscar"
    (is (= (buscar '(a 1 b 2 c 3 d 4 e 5) 'c) 3))
    (is (= (buscar '(a 1 b 2 c 3 d 4 e 5) 'f) (list (symbol ";ERROR:") (symbol "unbound") (symbol "variable:") 'f)))
    (is (= (buscar '(a 1 b 2 c 3 d 4 e 5) 'a) 1))
    (is (= (buscar '(a 1 b 2 c 3 d 4 e 5) 'z) (list (symbol ";ERROR:") (symbol "unbound") (symbol "variable:") 'z)))
    (is (= (buscar '() 'a) (list (symbol ";ERROR:") (symbol "unbound" ) (symbol "variable:") 'a)))
    ))

(deftest actualizar-amb-test
  (testing "Test for actualizar-amb function"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)) '(a 1 b 2 c 3)))
    (is (= (actualizar-amb '() 'b 7) '(b 7)))
    ))


(deftest proteger-bool-en-str-test
  (testing "Test de proteger-bool-en-str solo con #t"
    (is (= "%t" (proteger-bool-en-str "#t")))
    (is (= "(or %t %t)" (proteger-bool-en-str "(or #t #t)")))
    (is (= "(and %t %t)" (proteger-bool-en-str "(and #t #t)")))
    (is (= "(not %t)" (proteger-bool-en-str "(not #t)")))
    (is (= "(if %t %t %t)" (proteger-bool-en-str "(if #t #t #t)")))
    (is (= "(cond [%t %t] [%t %t])" (proteger-bool-en-str "(cond [#t #t] [#t #t])")))
    (is (= "(and (or %t %t) %t)" (proteger-bool-en-str "(and (or #t #t) #t)")))
    )
  (testing "Test de proteger-bool-en-str solo con #f"
    (is (= "%f" (proteger-bool-en-str "#f")))
    (is (= "(or %f %f)" (proteger-bool-en-str "(or #f #f)")))
    (is (= "(and %f %f)" (proteger-bool-en-str "(and #f #f)")))
    (is (= "(not %f)" (proteger-bool-en-str "(not #f)")))
    (is (= "(if %f %f %f)" (proteger-bool-en-str "(if #f #f #f)")))
    (is (= "(cond [%f %f] [%f %f])" (proteger-bool-en-str "(cond [#f #f] [#f #f])")))
    (is (= "(and (or %f %f) %f)" (proteger-bool-en-str "(and (or #f #f) #f)")))
    )
  (testing "Test de proteger-bool-en-str combinado"
    (is (= "(and (or %t %f) %t)" (proteger-bool-en-str "(and (or #t #f) #t)")))
    (is (= "(and (or %f %t) %f)" (proteger-bool-en-str "(and (or #f #t) #f)")))
    (is (= "(and (or %t %f) %f)" (proteger-bool-en-str "(and (or #t #f) #f)")))
    (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)")))
    (is (= "(and (or %t %t) %t)" (proteger-bool-en-str "(and (or #t #t) #t)")))
    (is (= "" (proteger-bool-en-str "")))
    )
  )

(deftest restaurar-bool-test
  (testing "Test de restaurar-bool"
    (let [esperado (list (symbol "and") (list (symbol "or") (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) (symbol "#t"))]
      (is (= esperado (restaurar-bool (read-string "(and (or %f %f %t %t) %t)"))))
      (is (= esperado (restaurar-bool (read-string (proteger-bool-en-str "(and (or #f #f #t #t) #t)")))))
      )
    )
  )

(deftest fnc-append-test
  (testing "Test de fnc-append"
    (is (= (fnc-append '((1 2) (3) (4 5) (6 7))) '(1 2 3 4 5 6 7)))
    (is (= (str (fnc-append '((1 2) 3 (4 5) (6 7)))) "(;ERROR: append: Wrong type in arg 3)"))
    (is (= (str (fnc-append '( (1 2) A (4 5) (6 7)))) "(;ERROR: append: Wrong type in arg A)"))
    )
  )

(deftest fnc-equal?-test
  (testing "Test de fnc-equal?"
    (is (= (fnc-equal? ()) (symbol "#t")))
    (is (= (fnc-equal? '(A)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A a)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A B)) (symbol "#f")))
    (is (= (fnc-equal? '(1 1 1 1)) (symbol "#t")))
    (is (= (fnc-equal? '(1 1 2 1)) (symbol "#f")))
    )
  )

(deftest fnc-read-test
  (testing "Test de fnc-read"
    (is (= (str (with-in-str "(hola\nmundo)" (fnc-read ()))) "(hola mundo)"))
    (is (= (str (fnc-read '(1))) "(;ERROR: read: Use of I/O ports not implemented)"))
    (is (= (str (fnc-read '(1 2))) "(;ERROR: Wrong number of args given #<primitive-procedure read>)"))
    (is (= (str (fnc-read '(1 2 3))) "(;ERROR: Wrong number of args given #<primitive-procedure read>)")))
  )
