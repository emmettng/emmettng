---
title: "Category Theory: Basic Terms"
date: 2019-11-14T15:56:14+08:00
draft: true
---

#### online resources
- [wiki haskell](https://wiki.haskell.org/Lambda_abstraction)
- [stackexchange](https://math.stackexchange.com/questions/65622/whats-the-point-of-eta-conversion-in-lambda-calculus)

### Basic terms 
- `alpha conversion`     
  function variables renaming
  
- `eta conversion`   
  **1.**  `\x -> abs x`

  **2.**  `abs`
  - from 1 to 2 is eta reduction (point free)
  - from 2 to 1 is eta abstraction 

  #### references:
    - [what's the motivation for eta rules](https://mail.haskell.org/pipermail/haskell-cafe/2010-December/087850.html)
- `beta reduction`   
  function evaluation