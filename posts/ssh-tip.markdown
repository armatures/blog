---
title: "SSH Authentication with Multiple Accounts"
date: "July 30, 2020"
tags: software, ssh, git
---

When working for different organizations, or using different accounts, you might find yourself needing to use different credentials to push to different repositories. Intuitively, I thought it might involve using some sort of user alias, but the actual solution uses aliases for different SSH hosts (even if the hosts resolve to the same web address). Let me explain.

On a linux machine, you have a configuration for the keys you used to authenticate with different posts in `~/.ssh/config`. That has contents like:
```
# Default GitHub
Host github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile ~/.ssh/id_rsa

# Work GitHub
Host work.github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile ~/.ssh/id_rsa_work
```
where you can configure different keys to authenticate with different hosts. But notice that the `HostName` lines for the two different configurations are the same! The `Host` line serves as an alias you can specify in your git remote configuration for a given repository, and it cashes out with the host and key specified in the rest of the config. So executing `git remote -v` in your repo might read something like this:
```
origin	git@work.github.com:work-co/basic.git (fetch)
origin	git@work.github.com:work-co/basic.git (push)
```
It's a little disorienting, because there's no actual host at `work.github.com`: that resolves to `github.com`. It's very convenient, though, once you know what's going on.

Hat tip to [this stackoverflow answer](https://stackoverflow.com/a/17158985/781421) for giving a good step-by-step walkthrough of how to set this up.
