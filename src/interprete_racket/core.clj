(ns interprete-racket.core)

(declare ejemplo)
(declare es-el-doble?)
(declare spy)
(defn -main
      "Ejemplo de Proyecto en Clojure"
      [& args]
      (ejemplo))
(defn ejemplo []
      (let [mensaje-1 (do (print "Ingrese un numero: ") (flush)),
            n1 (read),
            mensaje-2 (do (print "Ingrese el doble del numero anterior: ") (flush)),
            n2 (read),
            salida (println (str "Ud." (if (es-el-doble? n1 n2) " " " no ") "sabe calcular!"))]
           (do (print "Presione Enter...") (flush) (read-line) (read-line) 'Chau!)))
(defn es-el-doble? [a b]
      (= (* 2 a) b))
