(ns interprete-racket.core-test
  (:require [clojure.test :refer :all]
            [interprete-racket.core :refer :all]))

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
    )
  )
