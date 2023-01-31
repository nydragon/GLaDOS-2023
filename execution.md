# Execution

This document will serve the process of documenting thoughts regarding the execution of the AST

## Questions

- How to know when to execute lambda ?
- Local scope variables ?
- Should boolean literals be parsed by tokenization / cpt / ast ?

## Established

- Function and variable definition will be tracked using tuple of Data.Map