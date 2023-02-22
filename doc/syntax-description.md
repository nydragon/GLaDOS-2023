# Syntax definition of our Scheme implementation
This document defines the syntax of this specific implementation of Scheme using the [Augmented Backus-Naur form](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form) (also [ABNF](https://www.rfc-editor.org/rfc/rfc5234)).


```ABNF
; Every visible special character that isn't a double quote
symbol                = %x20–21 / %x22–2F / %x3A-40 / %x5B-60 / %x7B-7E

character             = ALPHA | DIGIT | symbol

identifier            = 1*ALPHA

text                  = *character

literal               = DQUOTE text DQUOTE

number                = 1*DIGIT

expression-start      = "("

expression-end        = ")"

newline               = CR / LF / CRLF 

separator             = *(SP / newline)

boolean               = "#t" / "#f"

expression-argument   = literal / number / expression / boolean

expression-arguments  = *(separator expression-argument)

expression            = expression-start identifier expression-arguments expression-end EOL

expressions           = *expression
```

[Return to the main page](../README.md)