# README

Some useful Common Lisp code

Using ASDF system definition.

## Usage

```lisp
(push {{this path}} asdf:*central-registry*)
(asdf:compile-system :ccQ-toolbox)
(asdf:load-system :ccQ-toolbox)

;; Example
(math-tool:standard-deviation '(1 1 1))
```


## Catalog

+ general-tool
    - with-gensyms
    - aappend
    - combine
    - range
    - ->>
    - ->
+ math-tool
    - standard-deviation
    - gen-random-num
    - sigma
+ matrix-tool
    - *list-to-array
    - *array-to-list
    - array-slice
    - array-slice-col
    - array-multiply
    - matrix-norm-2
    - point-distance
    - points-average
