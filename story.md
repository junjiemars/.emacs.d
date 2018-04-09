Story
=======

I'm a nerd with Emacs, it's awesome if I can [Learn Emacs in less then Ten Years](http://edward.oconnor.cx/2009/07/learn-emacs-in-ten-years). So, I decided to start from Level One not the Level Zero after read Emacs' tutorial (C-h t). Emacs is the most powerful editor on the world there is no **one of**, it's programmable, elegant and **self-documenting**, so you can start from Level Zero if you had time may be less than 10 years to read and try Emacs.

After falling in love with [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), I know it's the time to make Emacs being a part of my body and mind, not just open a file, navigating, editing, and save it. The Level One of mine start from [this is a Clojure-friendly emacs config](https://github.com/flyingmachine/emacs-for-clojure), and [A reasonable Emacs config](https://github.com/purcell/emacs.d). But, those ones neither can suit for my daily use on different machines.

The adaptive Emacs which I need is more stable, more smaller, more faster and more flexible which can be run on anywhere where Emacs run. So, I decide to build a new one.


* [Features](#features)
* [Requirements](#requirements)
* [Where](#where)
* [What](#what)
* [Install](#install)
* [Cooking](#cooking)
* [Mnemonic](mnemonic.md)

Now, let's start from the Level Two. And one more thing: teach youself some little Elisp (M-x info).

## Highlights
* __Carefully designed__: just load what's your need.
* __Adaptive__: Can run on any OS, GUI or Terminal, from most recent Emacs's versions to ancient ones, and one Emacs's configuration suit for all coexisting Emacs on the same machine.
* __Consistent__: Whereever and whenever you can play with Emacs always behaves in the same way.
* __Awesome style__: See it yourself.
* __stable__
* supper __faster__


## Requirements
* Emacs 22.1.1+
* Any keyboard


## Where
* Linux, whatever GUI or Terminal
* OSX, whatever GUI or Terminal
* Windows, whatever GUI or Terminal
* One machine run multiple Emacs instances in same time


## What
* You are the master of your Emacs, ...
* Navigating, finding, grepping in the same way whereever, ...
* Org Mode that's your new life, ...
* Reliable programming expirence, C, Lisp and Python, ...
* Bash on Windows, ...


## Install
Just one thing you need to do, clone it (just <400KB) to your HOME directory:
```sh
$ cd
$ git clone --depth=1 https://github.com/junjiemars/.emacs.d.git
```


On Windows, if you'd [Git-Bash](https://git-scm.com/downloads) installed but no Emacs, you are lucky, a one line code will do it all for you and more: fix some Emacs' issue for you, and you don't need run ```git clone ...```,

instead, 


```sh
$ HAS_EMACS=1 bash <(curl https://raw.githubusercontent.com/junjiemars/kit/master/win/install-win-kits.sh)
```


* First run: start Emacs then exit it, take a while if your Emacs 24.0+.
* Non-First run: start Emacs then run ```(clean-compiled-files)```, then exit.

Now, start Emacs again, it will works perfectly, but if you want to more control and more features, such as themes, packages, and fonts etc., see next section: [cooking](#cooking)


## Cooking

Suppose your Emacs HOME at ```~/.emacs.d``` and annotated it as ```emacs-home``` .

Your private configuration directory at ```emacs-home/private/``` 
and I abbreviate it as ```(emacs-home* "private/")```. All things under it you can rename, move, delete.

A magic function ```(clean-compiled-files)```, all Elisp files be compiled,
so if meet some trouble, run it and reopen the Emacs.


Directory orgnization:
```
emacs-home
├── LICENSE
├── README.md
├── appetizer.md
├── config (comment "all stuff here")
├── elpa (comment "installed packages")
├── init.el (comment "Emacs init file")
├── mnemonic.md
├── private (comment "your configuration can put here")
│   ├── self-env-spec.el
│   ├── self-epilogue.el
│   ├── self-package-spec.el
│   ├── self-path.el
│   ├── self-prologue.el
├── story.md
└── theme (comment "themes directory")

```


Locate your sepc via ```(emacs-home* "private/self-path.el")```.

```lisp
(def-self-path-ref
  :env-spec (emacs-home* "private/self-env-spec.el")
  :prologue (comment (emacs-home* "private/self-prologue.el"))
  :package-spec (comment (emacs-home* "private/self-package-spec.el"))
  :epilogue (comment (emacs-home* "private/self-epilogue.el")))
```


### Theme

Easy to switch themes, or try a new one.

The theme's spec locate in ```(emacs-home* "private/self-env-spec.el")```

```lisp
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t))
```


### Font and Language

Easy to swtich fonts and [CJK](https://en.wikipedia.org/wiki/CJK_characters) characters, or try a new one. The default encoding is [UTF-8](https://en.wikipedia.org/wiki/UTF-8). 

The default font's spec locate in ```(emacs-home* "private/self-env-spec.el")```

```lisp
(def-self-env-spec
  :font (list :name (platform-supported-if darwin
                        "Monaco-14"
                      (platform-supported-if windows-nt
                          "Consolas-13"
                        "DejaVu Sans Mono-12")) 
              :allowed t))

```



### Shell

### Package management

* associate ```private/self-package-spec.el``` with ```private/self-path.el```
```lisp
(setq self-def-paths
      (list
       ;; ...
       :package-spec (emacs-home* "private/self-package-spec.el")
       ;; ...
       )
)
```

* ```private/self-package-spec.el``` looks like:
```lisp
(def-self-package-spec
  (list
   :cond (lambda ()
           (and (version-supported-p <= 24.0)
                (bin-exists-p "virtualenv")))
   :packages '(elpy)
   :compile `(,(emacs-home* "config/setup-python.el"))))
```
this _package-spec_ used to support _Python_ programming, 
[default package spec](config/sample-self-package-spec.el)

### Common Lisp programming

### Clojure programming

### Python programming


