---
title: "Package: Configurator"
date: 2019-11-06T17:32:49+08:00
draft: true
---
> There are many great libraries. But usually it will take equally great many effort for people to get familiar with them.    
> Some example seems  stop people from using it deliberately.   
> `Package` section records examples to remind myself for future use.

#### online resouces:
0. [stackoverflow example](https://stackoverflow.com/questions/14340976/how-to-use-configurator)
1. [24 days haskell example](https://ocharles.org.uk/posts/2012-12-21-24-days-of-hackage-configurator.html) 
2. [schoole of haskell example](https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller/episode-3-configurator)
3. [hackage doc](http://hackage.haskell.org/package/configurator)


### Examples:

1. Config file `some.cfg` in dirction `Config`
```
SomeInfo
{
    onlineCSV = "/home/MachineLearning/Datasets/DataInspector/data_out_sku_20190816.csv"
    testCSV= "./testData/data_test.csv"  
}
```

2. This is the from [stackoverflow example](https://stackoverflow.com/questions/14340976/how-to-use-configurator)
```
  1   {-# LANGUAGE OverloadedStrings #-}
    1 
    2 module TestGround where
    3                                               
    4 import Data.Configurator
    5 import Data.Configurator.Types (Value)
    6    
    7 tryConfig :: IO ()
    8 tryConfig = do
    9   cfg <- load [Required "Config/some.cfg"]
   10   lst <- require cfg "SomeInfo.testCSV" :: IO Value
   11   print lst
```