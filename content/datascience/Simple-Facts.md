---
title: "Simple Facts"
date: 2019-12-17T17:59:08+08:00
draft: false 
---
> ` A, B, C & D`

## A. Typical data analysis process includes:  
> 1. `Business requirements analysis`, `domain expertise mapping`, `data sampling scheme`.
> 2. Scheme implementation.    
> 3. ETL architecture implementation.   
> 4. Distributional test , anomaly detection , exception identification, revise previous process. 
> 6. Descriptive analysis and provide utility functions.
> 7. Scheduled tasks modeling and evaluation.     
> 8. Ad-hoc tasks development and evaluation.

## B. Problems of A  

- Things never happened in that order.
- The requirements of our end-users could be paradoxical and the first version narrative has never been the same as what actually need to be done eventually. 
- Data contamination happens(ageing equipment, nonstandard operation, seasons, local convention, etc). So `Empirical Reference` need to be examined very carefully. 
- Data is valid `iff` wrong value will cause problem and someone will be punished due to this problem. Upstream data from sales or manufacture department could be wrong or intentionally `adjusted` in their own interests (imbalanced or invalid normalization).
-  `System bias implies the existence of certain effective domain. Only, it is not clear how this domain could be strictly identified by other features.` (***It is significantly cost effective as a baseline***.)

## C. `Discriminative` and `Generative`
1. It is extremely hard for managers to resist the temptation of possible causal relation. `It is necessary to proactively help downstream clients to sharpen their requirement in order to guarantee that our works being integrated with other business logic correctly`. 

>Simple analogies that can be used to suppress end-users' desire for causality:
    > - `There is such a place, people who goes to there would take higher risk of not coming back alive. We should just avoid of being there.But what if that place is called 'Hospital'? `
    > - `People who eat salad usually has a healthy body shape. How about eating TEN times more? Presumably, it would make you TEM times more healthier. `
    
## D. Machine Learning 
- Trying modeling with large number of parameters would confine the validity of this model within an extremely small domain. Out of domain prediction is an illusion brought by low dimensional problem. 
- Machine Learning approach is necessary for helping us understand the high-dimensional sampling set rather than focusing on optimizing some certain loss function. 
- Dimension reduction is always welcome. For end-users, `Explainable` **means** `Simple`, a low dimensional statistical indicator is more desirable. Most of the time, certain lost of accuracy is acceptable.