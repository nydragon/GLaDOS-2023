# Syntax definition of our Scheme implementation
This document defines the syntax of this specific implementation of Scheme using the [Augmented Backus-Naur form](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form) (also [ABNF](https://www.rfc-editor.org/rfc/rfc5234)).


```ABNF
; Every visible special character that isn't a double quote
symbol                = %x20-2F / %x3A-40 / %x5B-60 / %x7B-7E

character             = ALPHA | DIGIT | symbol

identifier            = 1*ALPHA

text                  = *character

literal               = DQUOTE text DQUOTE

number                = 1*DIGIT

expression-start      = "("

expression-end        = ")"

sep                   = *(SP / HTAB / EOL)

boolean               = "#t" / "#f"

array-start           = "["

array-end             = "]"

array-item-delimiter  = ","

infix-sign            = "`"

array-content         = expression-argument 1*(*sep array-item-delimiter *sep expression-argument)

array                 = array-start *sep *array-content *sep array-end

expression-argument   = literal / number / expression / boolean / array

expression-arguments  = expression-argument 1*(sep expression-argument)

postfix-expression    = identifier sep *expression-arguments

infix-identifier      = identifier / infix-sign identifier infix-sign

infix-expression      = expression-argument sep infix-identifier sep expression-argument *sep *(sep infix-identifier sep expression-argument)

expression            = expression-start *sep *(postfix-expression / infix-expression) *sep expression-end sep

expressions           = *expression
```

[Return to the main page](../README.md)