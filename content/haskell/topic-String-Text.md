---
title: "Topic String Text"
date: 2019-11-20T18:22:44+08:00
draft: true
---

#### online resources 

- [Hackage Text](http://hackage.haskell.org/package/text-1.2.4.0)
- [stackoverflow 1](https://stackoverflow.com/questions/7357775/text-or-bytestring)
- [Monday Morning Haskell](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)
- [blog 1](http://www.alexeyshmalko.com/2015/haskell-string-types/)


```
> :info printf
 String -> r 	-- Defined in ‘Text.Printf’
> let s = printf " %s is %d" "today" 2019
> s
 today is 2019
```