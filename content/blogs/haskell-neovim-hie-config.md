---
title: "Haskell Neovim"
date: 2019-10-28T18:24:53+08:00
draft: false 
---

> May God bless this thing

## neovim 
- nodejs   
  Could be older version which cannot support coc.nvim correctly.

  [stackoverflow solution](https://askubuntu.com/questions/426750/how-can-i-update-my-nodejs-to-the-latest-version)
  ``` 
  sudo npm cache clean -f
  sudo npm install -g n
  sudo n stable
  sudo n latest
  ```

- neovim
  
  install 
  ```
  sudo add-apt-repository ppa:neovim-ppa/stable
  sudo apt-get update
  sudo apt-get install neovim
  ```

## ghcide 
`ghcide` is way much better than `hie`

- [ghcide instruction](https://github.com/digital-asset/ghcide): The instruction works fine till now. 
  - set up `~/.config/nvim/coc-setting.json`
  - set up `~/.config/nvim/init.json`
  ([neovim setup instruction](https://jdhao.github.io/2018/12/24/centos_nvim_install_use_guide_en/#builtin-terminal))

- select correct ghc version to cooperate with `ghcide`.


## vscode config 
[this blog works for simple project](https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf)
**TODO** need to figure out how it could works for multi main project
