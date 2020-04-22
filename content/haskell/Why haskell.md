---
title: "Why Haskell"
date: 2019-11-03T19:38:50+08:00
draft: false
---

### Programming 
- Engineers decompose a complex problem into many factors. Hopefully, a recomposition of these factors would work as we expected and satisfy the requirements. 

    Usually, people managed to design and implement these factors through analyzing and developing upon limited examples/requirements . The problem is when trying to reuse these factors :
    - The behaviour of novel recomposition could be unexpected. 
    - Factor upgrade for satisfying new requirement would break the consistency of other existing composition.

    These problems are particularly common and troublesome in software engineering. Tradition methodologies , such as OOP, could help us confine the scale of exceptions within a particular domain. However, we need a paradigm which could help us regulate the consequence of factorization process. 

- Being able to do doesn't necessarily means will do.
    -  The way we reasoning and planing is far from being well structured or long-term effective. 
    - The wish of introducing benefit of functional paradigm to cooperate with our unsustainable habit is great. However, as long as there are **Simple** alternatives, we will inevitably regress to who we are.
    - **Simple** usually implies **Complexities / Difficulties in Installments**.

 We need to be forced to do **abstraction** the **Right** way.

### Principles
- A learning system should be able to express great diversities based on compositions of extremely small number of rules.
  <p style="text-align: center;"> <b> Anything out of Nothing </b> </p>

- Every piece of Information matters, eg:
  <p style="text-align: center;"> <b> data Nat = Zero | Succ Nat</b> </p>
    
    `Succ n` carries two pieces of information 
    - `n` : the number **n**
    - `Succ n`: the number **n + 1**

There are many different paradigms and patterns being used to decompose and explain the mechanism of how this world works. Intuitively, most people agree that complexity originated from simplicity. These are seemingly trivial features of Haskell and Lambda Calculus, I believe these are the representation of valid simplicities that we need to get a better understanding of the world.
