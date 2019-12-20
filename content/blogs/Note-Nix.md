---
title: "Note: Nix"
date: 2019-12-20T11:06:23+08:00
draft: true
---

### Learning Path 
- [all resources entrance](https://nixos.org/nixos/learn.html)
- [0. Nix pill](https://nixos.org/nixos/nix-pills/why-you-should-give-it-a-try.html)
- [1. NixOS to EC2](https://typeclasses.com/nixos-on-aws)

## Quich Recap 

### common query 

- Install man `: nix-env -i man`
- Query all applications  `: nix-env -q`
- Check generations `: nix-env --list-generations`
- Rollback `: nix-env --rollback`
- Goback to generation 3 `: nix-env -G 3`
- Show runtime dependencies ``: nix-store -q --references `which hello` ``
- Show reverse dependencies ``: nix-store -q --referrers `which hello` ``
- `Closures` is a list of all dependencies ``: nix-store -qR `which man` ``
  - `Closures`(tree view)  ``: nix-store -q --tree `which man` ``

### package channel
- list package channel `:nix-channel --list`
- channel update `:nix-channel --update` **(This will take a long time)**