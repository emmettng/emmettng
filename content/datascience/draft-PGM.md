---
title: "Draft PGM"
date: 2020-05-05T22:13:51+08:00
draft: true
---


## Terms:
   1. [Probabilistic Proposition](https://en.wikipedia.org/wiki/Probabilistic_proposition): A probabilistic proposition is a proposition with a measured probability of being true for an arbitrary person at an arbitrary time.
   1. [Propositional Variable](https://en.wikipedia.org/wiki/Propositional_variable): A propositional variable (also called a sentential variable or sentential letter) is a variable which can either be true or false. Propositional variables are the basic building-blocks of propositional formulas, used in propositional logic and higher logics.
   1. **Upper case** represents propositional variable when being used in a formula to describe the relation between two or three propositions.
       > $P(A) + P(\neg{A}) = 1$
   1. **Upper case with subscript** represents a certain (value)observation of a **propositional variable**.
       > $P(A) = \sum_{i}P(A|B_i)P(B_i)$
   1. **lower case** represents propositional variable when being used in a formula to describe the relation among a set of propositions, many propositions will be denoted with same letter and different subscript.
       > $P(x_1,x_2,...,x_n)$
   1. **Upper case** and **Lower case** can be used together to represent certain semantic.
       > $P(H|e) \propto P(e|H) P(H)$
   $e$ represents `evidence`; $H$ represents `Hypothesis`.
   1. All probabilistic variable in following sections are **Propositional variables**.

## 1.Probability 



1. $P(A) + P(\neg{A}) = 1$

   - Usually, $P(A)$ refers to the probability of the **Variable** $A$ being different values.
   - In this document, $A$ refers to proposition variable (**An observation of event A**).
   - The probability of a proposition and all its mutual exclusive propositions added up to 1.
   -$P(A | Z) + P(\neg{A | Z}) = 1$. $Z$ is the context knowledge what would affect the distribution of $A$
   - Actually, all observation on the right side of notation ' **|** ' can be treated as a description of certain context knowledge.

1. $P(A) = \sum_{i}P(A|B_i)P(B_i)$
    
    It is also:
    - $P(A) = \sum_{i}P(A,B_i)$
    
    The probability of one propositions(**Obeservation**) $P(A)$ equals to the sum of all other propositions(**Obeservation**).

1. Marginal probability $P(x_i)$
    
    A joint probability distribution of encoding n propositional variables would be:
    - $P(x_1,x_2,...,x_n)$

    - $P(x_i)$ is a marginal probability that require summing $P(x_1,x_2,...,x_{i-1},x_{i+1},...,x_n)$ over all $2^{n-1}$ combinations. 
    



2. $P(A|B) = \frac{P(A,B)}{P(B)}$

    This is the Bayesian interpretation of conditional probability:
    - $P(A|B)$
    
    It is the joint probability 
    - $P(A,B)$ 
     
    normalized by the chance that the condition 
    - $P(B)$ \
    would happen.

    If the knowledge is a joint distribution of n variables $P(x_1,x_2,...,x_n)$, so this formula 
    - $P(x_i|x_j)=\frac{P(x_i,x_j)}{P(x_j)}$
    entail dividing two marginal probabilities
    - $P(x_j)$ and $P(x_i,x_j)$
   
   


## 2.Classical Bayesian 

$P(H|e) = \frac{P(e|H) P(H)}{P(e)}$

**Semantics:** 

>    The believe of our **priori knowledge** about $H$:
>   - $P(H)$ 
> 
>    will be updated to get the **posterior knowledge**: 
>    - $P(H|e)$  
>>    means given the observation of evidence $e$

>    by introducing the likelihood of observing $e$ on the condition of $H$ is happened for sure:
>    - $P(e|H)$

In other words, the prior knowledge of $P(H)$ updated by the known likelihood $P(e|H)$ to get the posterior knowledge $P(H|e)$.
> $P(H|e) \propto P(e|H) P(H)$
>> $P(e)$ usually don't need to be compute explicitly.

**Common usage** 

Usually we know:
- $P(H)$ and also $P(\neg{H})$
- $P(e|H)$ and also $P(e|\neg{H})$
- So that:
    >>
    >$P(H|e) = \frac{P(e|H) P(H)}{P(e)}$
    > 
    > can be rewritten as:
    > 
    > 1. $P(H|e) = \frac{P(e|H)P(H)}{P(e|H)P(H)+P(e|\neg{H})P(\neg{H})}$
    >>This is the naive usage of bayesian formula
    >
    > or
    >
    > 2. $\frac{P(H|e)}{P(\neg{H}|e)} = \frac{P(e|H)P(H)}{P(e|\neg{H})P(\neg{H})}$

## 3.Simple Belief updating
$\frac{P(H|e)}{P(\neg{H}|e)} = \frac{P(e|H)P(H)}{P(e|\neg{H})P(\neg{H})}$

The above formula could be reorganized as:
- **prior odds**: $\frac{P(H)}{P(\neg{H})}=O(H)$

- **likelihood**:$\frac{P(e|H)}{P(e|\neg{H})}= L(e|H)$

- **posterior odds**:$\frac{P(H|e)}{\neg{P(H|e)}}=O(H|e)$ 

So we have:
> $O(H|e) = L(e|H) O(H)$
> $O(H|e) = \frac{P(H|e)}{P(\neg{H}|e)} \implies P(H|e) = \frac{O(H|e)}{1+O(H|e)}$

### 3.1 Pooling of Evidence

If $e_k$ are independent from each other given condition $H$, then:
- $P(e_1,e_2,...,e_k|H) = \prod_{k=1}^{N}P(e^k|H)$

Therefore:
-  $O(H|e_1,e_2,...,e_k)= O(H) \prod_{k=1}^{N}P(e^k|H)$


### 3.2 Recursive updating Belief 

From previous `Pooling of Evidence` we have the belief of:
- $P(H|e_n)$

what if the new evidence, or observation $e$ shows up.
- $P(e|e_n)$

in this case $e_n$ is the same as $Z$ in Probability section. It is the context information. So we have:
- $P(H|e,e_n) \propto P(e|H,e_n) P(H|e_n)$ 

if we remove the context information $e_n$ this is just a regular Bayesian formula.

Now, let's focus one this:
- $P(e|H,e_n)$ 

This is the likelihood of observing $e$ given condition of $H$ and $e_n$. 

**Condition ONE**: If knowing $H$ or $\neg{H}$ renders past observations $e_n$ totally irrelevant with regard to future observations $e$, then:
- $P(e|H,e_n) = P(e|H)$ 

So we could use a simple likelihood ratio 
- $L(e|H) = \frac{P(e|H)}{P(e|\neg{H})}$

to update our new posterior knowledge such as:
- $O(H|e_{n+1})= L(e|H) O(H|e_n)$

#### Summaries:
- **Condition ONE** is a very restrict condition.
- We could use `logarithm` to make this equation more intuitive:
>>$logO(H|e_{n+1})=  log O(H|e_n) + logL(e|H)$

## 4 Multi-Valued Hypotheses

- $P(e_1,e_2,...,e_k|H) = \prod_{k=1}^{N}P(e^k|H)$

Assuming conditional independence with respect to each context knowledge $H_i$

We know:
- $P(H|e,e_n) \propto P(e|H,e_n) P(H|e_n)$ 
  

#### 4.1 likelihood matrix and likelihood vector of $e_i$

- $P(e_i|H_j)$

#### 4.2 prior knowledge vector

#### 4.3 posterior knowledge update

## TODO

Identifying 
1. An important tool in such organization is the identification of intermediate variables that introduce conditional independence among observables. 

1. causal link to a graph .
    The graph is constructed by known causality.
    -- causal link has a great property that it is really easy to identifying intermediate variables that introduce conditional independence.
    -- PGM can always update from leaf node ?
2. Virtual Evidence ==> read again!

<<<<<<< HEAD
2. Virtual Evidence ==> read again!


## Markov and Bayesian Network 

### Main topics
   - Markov network: undirected graph, represents **symmetrical probabilistic dependencies**.
   - Bayesian network: directed acyclic graph , represents **causal influences**.

### Interested in 
  - X is independent of Y, given Z 
  - information dependency 
  - knowledge representation schemes in inference systems.
  - construct independencies like human (by using graph)

- the elementary building blocks of human knowledge are not entires of a joint-distribution table. Rather, they are low-order marginal and conditional probabilities defined over small clusters of propositions.

- Graph
    - node represents propositional variables
    - arc represents local dependencies among conceptually related propositions. 

- Section 3.1.2 is a great tutorial for deriving theorem.
    - Actually the entire idea behind section 3.1 is worth thinking 

- $P(X|Y,Z) = P(X|Z)$ : given the knowledge of $Z$ the knowing the truth of $Y$ would not affect our belief of $X$. $X$ is independent from $Y$ given $Z$.

- we interested in conditional probability because, the inference starts from the joint probability of all proposition is inapplicable.

### Markov network as knowledge Base

- in a undirected graph:
    - vertex represents propositional variable 
    - arc represents local dependencies 
    - a variable is proclaimed independent of all its non-neighbors once we know the values of its neighbors.

**TODO** needs more attention 

### Bayesian network
    
    Based on these two information we know:
    - $P(C|B) P(B|A) = P(C|B,A) P(B|A)$
    - $P(C|B,A)P(B|A) = [P(B) P(C|B)]^A \propto P(B|A,C)$
    - Notation $[...]^A$ means temporarily extract context information so that computation with no common context would be more clear.
    - Now: $P(B|A,C) \propto P(C|B) P(B|A)$