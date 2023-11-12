# Racket Interpreter

Interprete de Racket realizado en Clojure

## Uso

Para ejecutar el proyecto, se necesita tener instalado [Leiningen](https://leiningen.org/).

- `lein run`: Ejecuta el proyecto.
- `lein test`: ejecuta los tests del proyecto.

### Leyendo archivos de Racket

Para un archivo de Racket, se debe ejecutar el comando `lein run` y luego escribir en el REPL:

Si queremos leer el archivo `demo.rkt`:

```
(enter! "demo.rkt")
```

Si queremos leer el archivo `jarras.rkt`:

```
(enter! "jarras.rkt")
```