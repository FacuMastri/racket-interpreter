# Racket Interpreter

Racket interpreter implemented in Clojure

## Usage

To run the project, you need to have [Leiningen](https://leiningen.org/) installed.

- `lein run`: Executes the project.
- `lein test`: Runs the project tests.

### Reading Racket Files

To read a Racket file, you should execute the command `lein run` and then type in the REPL:

If you want to read the file `demo.rkt`:

```clojure
(enter! "demo.rkt")
```

If you want to read the file `jarras.rkt`:

```clojure
(enter! "jarras.rkt")
```

Then, you can proceed with:

```clojure
(breadth-first bc)
```