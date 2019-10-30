---
title: "Haskell Neovim Hie Config"
date: 2019-10-28T18:24:53+08:00
draft: true
---

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

  set up coc.nvim
  ```
  :CocConfig
  ```
  [setup tutorial 0](http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/)   
  [setup tutorial 1](https://chrispenner.ca/posts/hie-core)  
  Example:
  ```
  {
	"haskell": {
  	"command": "hie-wrapper",
  	"rootPatterns": [".stack.yaml", "cabal.config", "package.yaml"],
  	"filetypes": ["hs", "lhs", "haskell"],
  	"initializationOptions": {},
  	"settings": {
    	"languageServerHaskell": {
    	  "hlintOn": true,
    	  "maxNumberOfProblems": 10,
    	  "completionSnippetsOn": true
    	}
  	}
	}
  }
  ```
    - in `.config/nvim/ini.vim`
  ```
  
  ```

  common operation:  
  1. Plug 'scrooloose/nerdtree'  

      in init.vim
      ```
      map <C-f> :NERDTreeToggle<CR>
      ```
      in nvim use ctrl-f to open fold tree and use ctrl-w w to move around.
      ```
      Control+W followed by W
      ```

```
code
```

### Others sets

- use system clipboard, copy and paste between different vim windows.
  
  ```
  set clipboard=unnamed
  ```