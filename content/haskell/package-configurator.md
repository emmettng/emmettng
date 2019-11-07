---
title: "Package: Configurator"
date: 2019-11-06T17:32:49+08:00
draft: true
---

## Minimum usable: configurator
> There are many great Haskell libraries. But usually it will take equally great amount of effort for people to get familiar with them.    
> Worstly, it seems like some examples were designed to bring more obstracle deliberately. 
> `Package` section follows the `minimum usable` principle. Examples of this section is clear enough for reminding myself in future. Hopefully, it will also offer some help to others.
>  Issue in 

#### online resouces:
0. [stackoverflow example](https://stackoverflow.com/questions/14340976/how-to-use-configurator)
1. [24 days haskell example](https://ocharles.org.uk/posts/2012-12-21-24-days-of-hackage-configurator.html) 
2. [hackage doc](http://hackage.haskell.org/package/configurator)
3. [schoole of haskell example](https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller/episode-3-configurator)


### Notes

1. Config file `DemoOneInfo.cfg` in dirction `data`
```
DemoOneInfo                                                               
{                                                                        
        stringField = "This is demo ONE"     
        intField = 1
        floatField = 1.3                                                                                   
        homoListNum = [10,20,30,40,50]                                      
        homoListString = ["wuhao_1","emmettng_1"]                           
        homoListBool = [true,false,false,false]
        heteList = [1, "string", false]
}          
```
2. Simple data types
```
{-# LANGUAGE OverloadedStrings #-}
          
import Data.Configurator
import Data.Configurator.Types

stringField :: IO String
stringField = do
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.stringField" :: IO String
                                                                                                                                                   
intField :: IO Int                                                                                                                              intField = do
        cfg <- load [Required "data/configDemoOne.cfg"]                                                                    
        require cfg "DemoOneInfo.intField" :: IO Int
                                                                                                                 
floatField :: IO Float                                              
floatField = do
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.floatField" :: IO Float               
                                                  


heteListField :: IO [Value]
heteListField = do
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.heteList" :: IO [Value]

```

    Output:
```
Prelude Packages.Configurator> stringField 
"This is demo ONE"
Prelude Packages.Configurator> :info stringField 
stringField :: IO String

Prelude Packages.Configurator> intField 
1
Prelude Packages.Configurator> :info intField 
intField :: IO Int

Prelude Packages.Configurator> floatField 
1.3
Prelude Packages.Configurator> :info floatField 
floatField :: IO Float
```

3. Homogeneous List
```
homoIntListField :: IO [Int]                                          
homoIntListField = do
        fg <- load [Required "data/configDemoOne.cfg"]
        equire cfg "DemoOneInfo.homoListNum" :: IO [Int]
                                                                 
homoStringListField :: IO [String]                
homoStringListField = do                                              
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.homoListString" :: IO [String]
                   
homoBoolListField :: IO [Bool]
homoBoolListField = do
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.homoListBool" :: IO [Bool]

```

    Output:

```
Prelude Packages.Configurator> homo
homoBoolListField    homoIntListField     homoStringListField
Prelude Packages.Configurator> homoBoolListField 
[True,False,False,False]
Prelude Packages.Configurator> :info homoBoolListField 
homoBoolListField :: IO [Bool]

Prelude Packages.Configurator> homoIntListField 
[10,20,30,40,50]
Prelude Packages.Configurator> :info homoIntListField 
homoIntListField :: IO [Int]

Prelude Packages.Configurator> homoStringListField 
["wuhao_1","emmettng_1"]
Prelude Packages.Configurator> :info homoStringListField 
homoStringListField :: IO [String]

```

4. Heterogeneous

```
heteValueList :: IO [Value]                                                                                                                                                                      
heteValueList = do 
        cfg <- load [Required "data/configDemoOne.cfg"]
        require cfg "DemoOneInfo.heteList" :: IO [Value]
 
heteListField :: IO Value 
heteListField = do                 
        cfg <- load [Required "data/configDemoOne.cfg"]         
        require cfg "DemoOneInfo.heteList" :: IO Value 

```

`Value` is the internal data type being used to bridge desire type and input in `.cfg` file.  The example above usually indicate the use of customer data type rather than a list of `Value`.
```
Prelude Packages.Configurator Data.Configurator.Types> heteValueList 
[Number (1 % 1),String "string",Bool False]
Prelude Packages.Configurator Data.Configurator.Types> :info heteValueList 
heteValueList :: IO [Value]

Prelude Packages.Configurator Data.Configurator.Types> heteListField 
List [Number (1 % 1),String "string",Bool False]
Prelude Packages.Configurator Data.Configurator.Types> :info heteListField 
heteListField :: IO Value

```
5. Nested section

```
nestDemo :: IO String
nestDemo = do
        cfg <- load [Required "data/configDemo2.cfg"]
        require cfg "Demo2Info.demo2_nest.nest-name" :: IO String
```
    Output:
```
*Main Packages.Configurator> nestDemo 
"dorothy"

*Main Packages.Configurator> :info nestDemo 
nestDemo :: IO String
```
6. Import other config files     
demo file `data/configDemoThree.cfg`
```
DemoThreeInfo
 {         
        stringField = "This is Demo 3"                                                                                                             
        numberField = 3                                                                                                           
        floatField = 3.3                                                                                                           
        homoListNum = [1000,2000,3000,4000,5000]                                                                                                                                       
        homoListString = ["wuhao_3","emmettng_3"]                                                                                                                                      
        homoListBool = [true,true,true,false]
        heteList = [3, "3string", true]
        import "configDemoOne.cfg"
        import "configDemo2.cfg"
}
```

```
importDemo :: IO String                                                                             
importDemo = do                                                                                     
  cfg <- load [Required "data/configDemoThree.cfg"]                                                 
  require cfg "DemoThreeInfo.Demo2Info.demo2_nest.nest-name" :: IO String 
```

```
Prelude Packages.Configurator> importDemo 
"dorothy"
Prelude Packages.Configurator> :info importDemo 
importDemo :: IO String
```
>**Note**   
>
> - `configDemoThree.cfg` is in the same directory with `configDemoOne.cfg` an `configDemo2.cfg`. So just import the file name is ok!
> - The domain name of imported config are all being nested in the domain name `DemoThreeInfo`.

7. Customize Data type
demo file `data/ConfigDemoCustomize.cfg`
```
DemoCustomizeInfo
{
        stringField = "This is Demo 3"
        numberField = 3
        floatField = 3.3
        homoListNum = [1000,2000,3000,4000,5000]
        homoListString = ["wuhao_3","emmettng_3"]
        homoListBool = [true,true,true,false]
        heteList = [3, "3string", true]
        custDemo = ["wh", 25, true,100]
}
```
```
-- | customize data type                                                                                         
--                                                                  
--                      
data DemoFour = DemoFour                         
  { sName :: String                                              
  , iAge :: Int                                   
  , bAdult :: Bool                                                    
  , fWeight :: Float
  } deriving (Show)                              
                                              
instance Configured DemoFour where                               
  convert :: Value -> Maybe DemoFour              
  convert (List [sV, nV, bV, fV]) = do                                
        s <- convert sV :: Maybe String              
        n <- convert nV :: Maybe Int                  
        b <- convert bV :: Maybe Bool 
        f <- convert fV :: Maybe Float                               
        return $ DemoFour s n b f                     
  convert _ = Nothing                                                 
                                                   
customizeDemo :: IO DemoFour                           
customizeDemo = do                                       
        cfg <- load [Required "data/configDemoCustomize.cfg"]
        require cfg "DemoCustomizeInfo.custDemo" :: IO DemoFour

```
    Output:
```
*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> :info customizeDemo 
customizeDemo :: IO DemoFour

*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> customizeDemo 
DemoFour {sName = "wh", iAge = 25, bAdult = True, fWeight = 100.0}

*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> :t sName <$> customizeDemo 
sName <$> customizeDemo :: IO String

*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> :t iAge <$> customizeDemo 
iAge <$> customizeDemo :: IO Int

*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> :t bAdult <$> customizeDemo 
bAdult <$> customizeDemo :: IO Bool

*Main Packages.Configurator Data.Configurator Data.Configurator.Types Data.Maybe> :t fWeight <$> customizeDemo 
fWeight <$> customizeDemo :: IO Float
``` 

8. Hot reloading
   
   TODO 