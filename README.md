# Langage PF23
> Auteurs : Achouri Sira Lina et Bonboire Marie
> Projet realise dans le cadre du module LU2IN119 (Programmation fonctionnelle, niveau L2), parcours double licence mathematiques-informatique.


## Version Ocaml requise : ocaml.5.0.0


## I. Bibliotheques utilisees

```ocaml

# pour realiser la saisie d'un programme par l'utilisateur

core.v0.16.0
async.v0.15.0

```


## II. Structure du code

### 1. Manipulation des instructions du langage :

```ocaml
type element
fun to_string
fun of_string
fun split
fun parse
fun text
```

### 2. Calcul d'un programme :

```ocaml
type prog
type stack
fun eval_binop
fun eval_stackop
fun step
fun calc
```

### 3. Ajout de definitions :

```ocaml
type name
type dico
fun add
fun remove
fun lookup
```

### 4. Evaluation :

```ocaml
fun eval
```

### 5. Tests :

```ocaml
fun tester
fun carre
fun cube
fun fib
fun fact
fun if_then_else
fun fun u
fun sum_n
fun val_abs
fun f91
fun syracuse
```

### 6. Programme interactif :
```ocaml
fun programme
```


## III. Compilation et Execution

+ Compiler les fichiers Projet.ml et Main.ml puis executer et suivre les instructions affichees ou utiliser les fonctions fournies.


## IV. Bibliographie

+ [Documentation Ocaml](https://v2.ocaml.org/api/)
+ [Printing Ocaml Programming](https://cs3110.github.io/textbook/chapters/basics/printing.html)
+ [Input-Output](https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora027.html)
+ [Using Async Reader to read user input](https://discuss.ocaml.org/t/using-async-reader-to-read-user-input/4943)
+ [Concurrent programming with Async](https://dev.realworldocaml.org/concurrent-programming.html)
