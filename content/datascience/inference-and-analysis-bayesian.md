---
title: "Inference and Analysis Bayesian"
date: 2020-02-21T20:09:17+08:00
draft: true
---

## 1.Probability 

1. $P(A) + P(\neg{A}) = 1$
   
   - The probability of a proposition and all its mutual exclusive propositions added up to 1.
   -$P(A | Z) + P(\neg{A | Z}) = 1$. $Z$ is the context knowledge what would affect the distribution of $A$
   - In this case $A$ is a certain proposition (**Oberservation**).
   
2. $P(A|B) = \frac{P(A,B)}{P(B)}$

    This is the Bayesian interpretation of conditional probability:
    - $P(A|B)$
    
    It is the joint probability 
    - $P(A,B)$ 
     
    normalized by the chance that the condition 
    - $P(B)$ \
    would happen.
   
   
3. $P(A) = \sum_{i}P(A|B_i)P(B_i)$
    
    It is also:
    - $P(A) = \sum_{i}P(A,B_i)$
    
    The probability of one propositions(**Obeservation**) $P(A)$ equals to the sum of all other propositions(**Obeservation**).

## 2.Classical Bayesian 

$P(H|e) = \frac{P(e|H) P(H)}{P(e)}$

**Semantics:** 

>    The believe of our **priori knowledge** about $H$:
>   - $P(H)$ 
> 
>    will be updated to our **posterior knowledge**: 
>    - $P(H|e)$  
>>    means given the observation of evidence $e$

>    if we know the likelihood of observing $e$ when $H$ is happened for sure:
>    - $P(e|H)$

In other words, the prior knowledge of $P(H)$ updated with the known likelihood $P(e|H)$ to get the posterior knowledge $P(H|e)$.
> $P(H|e) \propto P(e|H) P(H)$
>> $P(e)$ usually don't need to be compute explicitly.

**Common usage** 

Usually we know:
- $P(H)$ and also $P(\neg{H})$
- $P(e|H)$ and also $P(e|\neg{H})$
- So that we know 
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
- **posterior odds**:$\frac{H|e}{\neg{H|e}}=O(H|e)$ 

- **likelihood**:$\frac{e|H}{P(e|\neg{H})}= L(e|H)$

- **prior odds**: $\frac{H}{\neg{H}}=O(H)$

So we have:
> $O(H|e) = L(e|H) O(H)$