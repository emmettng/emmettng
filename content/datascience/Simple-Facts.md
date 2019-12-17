---
title: "Simple Facts"
date: 2019-12-17T17:59:08+08:00
draft: true
---
## A. Typical data analysis process includes:  
> 1. Analyse business requirement and design data sampling scheme.   
> 2. Scheme implementation.    
> 3. ETL.   
> 4. Distributional test & anomaly detection  
> 5. Based on the primitive analysis of `c` and `d`, adjust process `a` to `d` if necessary.    
> 6. Task-related analysis.   
> 7. Discriminative model or generative model development and test.     
> 8. Scheduled task development and test.

## B. Problems of **A**

- It never happen in that order.
- The requirements that our end-users keep thinking about could be paradoxical and the first version narrative has never been the same as what actually need to be done eventually. 
- Data contamination will happen(ageing equipment, nonstandard operation, season, local culture, etc). So `Empirical Reference` need to be adopted very carefully. 
- Data is valid `iff` wrong value will cause problem and someone will be punished because this problem. Many upstream data from sales or manufacture department could be wrong or intentionally `adjusted` for their own interest (imbalance or invalid normalization factor).
- System bias could be not political correct, however significantly cost effective. `(System bias implies the existence of certain effective domain. Only, it is not clear how this domain could be strictly identified with other features.)`

## C. `Discriminative` and `Generative`
1. It is extremely hard for managers to resist the temptation of possible causal relation. `It is a major part of our job to actively help downstream clients to sharpen their requirement and guarantee that our works being integrated with other business logic correctly`.

    A simple analogy that can be used to suppress end-users' desire for causality.
    > `Assuming eating salad is highly related to a healthy body shape. However, it doesn't necessarily mean eating 10 times more salad than regular people will guarantee someone's health`
    
## D. Role of Machine Learning 
- Out of domain prediction is an illusion brought by low dimensional problem. 
- Machine Learning approach is necessary for helping us understand more about the high-dimensional sampling set rather than focusing on optimizing some certain interested loss function. 
- Dimensionality reduction is always welcome. Because for end-user Explainable `==` Simple. Certain lost of accuracy is acceptable.