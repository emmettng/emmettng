---
title: "Submodule in Haskell"
date: 2020-04-08T14:30:45+08:00
draft: true
---

- There is a haskell module in folder "/testmodule"
- An other haskell project "SuperApp" want to use testmodule as a submodule. 

1. `stack new SuperApp`.
1. creat folder `lib` in project `SuperApp` .
   ```
   cd SuperApp
   mkdir lib
   ```
1. In project directory `SuperApp`, use this command:  
```SSH: git submodule add git@<gitpath>:/testmodule.git```  
**replace <gitpath> with real path**

1. If any update was taken placed in `testmodule`, go into **the corresponding directory:** `lib/testmodule`, then run the following command:
    ```
    git fetch
    git merge origin/master
    ```
1. update `stack.yaml` file with the information of `testmodule`
    ```
       packages:
       - .
       - lib/testmodule
    ```
1. `stack build`
