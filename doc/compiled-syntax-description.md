# Syntax definition of our Compiled Scheme implementation

This document defines the compiled syntax of a scheme script [Augmented Backus-Naur form](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form) (also [ABNF](https://www.rfc-editor.org/rfc/rfc5234)).

## Table of Contents

- [Syntax definition of our Compiled Scheme implementation](#syntax-definition-of-our-compiled-scheme-implementation)
  - [Table of Contents](#table-of-contents)
  - [ABNF](#abnf)
  - [Keywords](#keywords)
  - [Various Examples](#various-examples)

## ABNF

The following is the ABNF for our "assembly language".

```ABNF

character             = ALPHA / DIGIT / %x20-2F / %x3A-40 / %x5B-60 / %x7B-7E

identifier            = 1*ALPHA

text                  = *character

keyword               = "push" / "pop" / "call" / "move" / "init"

boolean               = "#t" / "#f"

list-start            = "("

list-end              = ")"

list-item-del         = ","

eol                   = LF / CR / CRLF

number                = 1*DIGIT

float                 = number "." 1*DIGIT

literal               = DQUOTE text DQUOTE

instruction-argument  = literal / number / float / boolean / identifier / atomic-list

atomic-list           = list-start instruction-argument *(list-item-del instruction-argument) list-end

instruction           = keyword *1(" " instruction-argument) eol

instruction-list      = 1*instruction

function-decl         = "func" identifier eol

main-function-decl    = "main" eol

function-end          = "end" eol

function              = function-decl instruction-list function-end eol

main-function         = main-function-decl instruction-list function-end eol

script                = main-function *function

```

## Keywords

The following are the keywords used by our "assembly" language.

| Keyword      | Arguments                                           | Description                                                             |
| ------------ | --------------------------------------------------- | ----------------------------------------------------------------------- |
| ```main```   | /                                                   | Starts the block containing the main instructions of the program        |
| ```func```   | ```name```                                          | Starts a function definition                                            |
| ```end```    | /                                                   | Ends a function definition                                              |
| ```push```   | ```symbole```                                       | Pushes a value on the **argument queue**                                    |
| ```pop```    | ```symbole```                                       | Pops a value from the **argument queue** and binds to given local variable  |
| ```init```   | ```name```                                          | Declares variable. **Does not** assign a value to it |
| ```move```   | ```symbole, value```                                | Copies a value into a variable or register                              |
| ```call```   | ```function name```                                 | Calls a function                                                        |
| ```return``` | ```value```                                         | Assigns value to **#RET** and resume execution in previous function     |
| ```if```     | ```condition, instruction list, instruction list``` | Ends an if block                                                        |
| ```enif```   | /                                                   | Ends an if block                                                        |

## Various Examples

The following is old brainstorming material. This will *eventually* be changed out for proper examples.

```
main
push 5
push 3
call add
end

func add
pop a
pop b
push a
push b
call +
end

(define (fact x)
    (if (eq? x 1)
        1
        (* x (fact (- x 1)))))
(print (fact 3))

main
pushArg 10
call fact
pushArg #RET
call print
end

func fact
pop x
push x
push 1
call eq?
if #RET
return 1
else
push x
push 1
call -
push #RET
call fact
push x
push #RET
call *
endif
end
```