---
title: "remind myself: Design Pattern - Handle"
date: 2019-10-30T11:37:24+08:00
draft: true
---

# **Handle** pattern

### Online resource 
1. [haskell in production](http://felixmulder.com/writing/2019/10/05/Designing-testable-components.html)
2. [Jasper's Blog](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)


### Notes

1. `handle pattern` defines a collection of function types which usually being used to handle resouces in the IO monad.  
   
    ```
    data Handle = Handle 
    { csvFileList :: IO [FilePath]
    , csvSaverPath :: FilePath -> FilePath
    }
    ```

2. `handle` usually being used togehter with another data type `Config`. 
   - `Config` contains information of context.    
   - Together with `Config`, it would be easy to `handle` different context.
   - `Config` could be created by using [configrator library](https://hackage.haskell.org/package/configurator) or reading information from JSON, yaml or other files.
   - `Config` datatype and its initialization function `new :: Config -> ... -> Handle` are usually being defined in the implementation module to provide more flexibility.