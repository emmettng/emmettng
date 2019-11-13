---
title: "Package: Serialization"
date: 2019-11-13T19:33:49+08:00
draft: false 
---


## Minimum usable: configurator
> There are many great Haskell libraries. But usually it will take equally great amount of effort for people to get familiar with them.    
> Worstly, it seems like some examples were designed to bring more obstracle deliberately. 
> `Package` section follows the `minimum usable` principle. Examples of this section is clear enough for reminding myself in future. Hopefully, it will also offer some help to others.


#### online resouces:
0. [store](http://hackage.haskell.org/package/store-0.7.0/docs/Data-Store.html)
1. [cereal](http://hackage.haskell.org/package/cereal-0.5.8.1/docs/Data-Serialize.html)
   

### Notes 
Both packages can be used the same ways
1. GHC extension 
   
    >  {-# LANGUAGE DeriveGeneric #-}            
    >  {-# LANGUAGE DefaultSignatures #-}                                     
2. Import corresponding libraries:
   
   `sore` package 
   ```
   import Data.Sort
   ```
   `cereal` package 
   ```
   import Data.Serialize
   ``` 
3. Rely on `GHC.Generics`
   ```
        import GHC.Generics
        ...
        data DemoMap = DemoMap    
          { toMapList :: [([String], [Float])]    
          } deriving (Generic, Show)
    ```
4. Declare as Instance


    `store` package
   ```
       instance Store DemoMap
   ``` 
    `cereal` package
   ```
       instance Serialization DemoMap
   ``` 
5. Write to / Read from file by `Data.ByteString`


   `store` pakcage
   ```
        import qualified Data.ByteString as BL
        ...
        serializeDemoMap:: FilePath -> StandardMap -> IO ()
        serializeDemoMap filePath sdmap = BL.writeFile filePath (encode sdmap)
                                             
        deserializeDemoMap :: FilePath -> IO (Either PeekException StandardMap)
        deserializeDemoMap filePath = do          
            bs <- BL.readFile filePath              
            return $ decode bs        

   ```
   `cereal` package
   ```
        import qualified Data.ByteString.Lazy as BL
        ...
        serializeDemoMap :: FilePath -> StandardMap -> IO ()
        serializeDemoMap filePath sdmap = BL.writeFile filePath (encodeLazy sdmap)

        deserializeDemoMap :: FilePath -> IO (Either String StandardMap)
        deserializeDemoMap filePath = do          
          bs <- BL.readFile filePath              
          return $ decodeLazy bs
   ``` 
6. The file produced by `store` is about `1/3` bigger than file produced by `cereal`. 
7. Performance test:
    **TODO**