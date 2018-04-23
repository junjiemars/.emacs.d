More Reasonable Emacs
=======

[![build status](https://api.travis-ci.org/junjiemars/.emacs.d.svg?branch=master)](https://api.travis-ci.org/junjiemars/.emacs.d)


It's works on Emacs version 22.1.1+ , and on any platform that Emacs supported, and multiple Emacs instances with different version or different mode can run on the same machine at the same time.

* clone More Reasonable Emacs from github
```sh
git clone --depth=1 https://github.com/junjiemars/.emacs.d.git ~/.emacs.d
```
* open Emacs, after Emacs compile all elisp files, then reopen Emacs
* want more functionalities see [Cooking](#cooking) section


One more thing, it's best to remind you: take care, it's super fast.


Story
=======

I'm a nerd with Emacs, it's awesome if I can [Learn Emacs in less then Ten Years](http://edward.oconnor.cx/2009/07/learn-emacs-in-ten-years). So, I decided to start from Level One not the Level Zero after read Emacs' tutorial (C-h t). Emacs is the most powerful editor on the world there is no **one of**, it's programmable, elegant and **self-documenting**, so you can start from Level Zero if you had time may be less than 10 years to read and try Emacs.

After falling in love with [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), I know it's the time to make Emacs being a part of my body and mind, not just open a file, navigating, editing, and save it. The Level One of mine start from [this is a Clojure-friendly emacs config](https://github.com/flyingmachine/emacs-for-clojure), and [A reasonable Emacs config](https://github.com/purcell/emacs.d). But, those ones neither can suit for my daily use on different machines.

The More Reasonable Emacs strives to be fast, stable and hacker friendly, which can be run on anywhere where Emacs run.


* [Features](#features)
* [Requirements](#requirements)
* [Where](#where)
* [What](#what)
* [Install](#install)
* [Cooking](#cooking)
* [Troubleshooting](#troubleshooting)
* [Mnemonic](mnemonic.md)


Now, let's start from the Level Two. And one more thing: teach youself some little Elisp (M-x info).

## Highlights

* __Carefully designed__: just load what's your need.
* __Adaptive__: Can run on any OS, GUI or Terminal, from most recent Emacs's versions to ancient ones, and one Emacs's configuration suit for all coexisting Emacs on the same machine.
* __Consistent__: Whereever and whenever you can play with Emacs always behaves in the same way.
* __Awesome style__: [screenshot](screenshot.md)
* __Stable__
* Supper __faster__


## Requirements

* Emacs 22.1.1+
* Any keyboard


## Where

* Linux, whatever GUI or Terminal
* Darwin, whatever GUI or Terminal
* Windows, whatever GUI or Terminal
* Non-Internet
* Docker container
* One machine run multiple Emacs instances or modes in same time


## What

* You are the master of your Emacs, ...
* Navigating, finding, grepping in the same way whereever, ...
* Org Mode that's your new life, ...
* Reliable programming expirence, C, Lisp and Python, ...
* Bash on Windows, ...


## Install

Just one thing you need to do, clone it (just <500KB) to your HOME directory:
```sh
$ git clone --depth=1 https://github.com/junjiemars/.emacs.d.git ~/.emacs.d
```

First run More Reasonable Emacs, it should automatically do:
* byte compile elisp source files.
* install packages, if you enabled it and you have a Emacs-24.0+.

After first run More Reasonable Emacs, exit and then reopen Emacs.


On Windows, if you'd [Git-Bash](https://git-scm.com/downloads) installed but no Emacs, you are lucky, a one line code will do it all for you and more: fix some Emacs' issue for you, and you don't need run ```git clone ...```,

instead, 


```sh
$ HAS_EMACS=1 bash <(curl https://raw.githubusercontent.com/junjiemars/kit/master/win/install-win-kits.sh)
```


Now, it will works perfectly, but if you want to more control and more features, such as themes, packages, and fonts etc., see next section: [cooking](#cooking)


## Cooking

Suppose your Emacs HOME at ```~/.emacs.d``` and annotated it as ```emacs-home``` .

Your private configuration directory at ```emacs-home/private/``` 
and I abbreviate it as ```(emacs-home* "private/")```. All things under it you can rename, move, delete.

A magic function ```(clean-compiled-files)```, all Elisp files should be compiled,
so if meet some trouble, run it and reopen the Emacs.


Directory orgnization:
```
emacs-home
├── LICENSE
├── README.md
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
├── screenshot.md
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

Check Emacs configurations:
* ```system-configuration-options```
* ```system-configuration-features```
* ```features```


### Topic

* [Theme](#theme)
* [Font](#font)
* [Shell](#shell)
* [Eshell](#eshell)
* [Desktop](#desktop)
* [Socks](#socks)
* [Package](#package)



### Theme

Easy to switch themes, or try a new one.

The theme's spec locate in ```(emacs-home* "private/self-env-spec.el")```

```lisp
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t))
```


### Font

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

Any ```Shell```, copy environment variables, and on any platforms.

[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) has a poor implementation for the same functionalities, it is unstable and slow. But here, We had a builtin one, and more stable and more faster.


The shell spec locate in ```(emacs-home* "private/self-env-spec.el")```

```lisp
(def-self-env-spec
  :shell (list :env-vars `("JAVA_HOME"
                           "PYTHONPATH"
                           ,(platform-supported-unless windows-nt
                              (platform-supported-if darwin
                                  "DYLD_LIBRARY_PATH"
                                "LD_LIBRARY_PATH")))
               :interactive-shell (platform-supported-unless darwin t nil)
               :exec-path t
               :bin-path "/bin/bash"
               :allowed t))
```


### Eshell


The eshell spec locate in ```(emacs-home* "private/self-env-spec.el")```


```lisp
(def-self-env-spec
  :eshell (list :visual-commands '("mtr")
                :destroy-buffer-when-process-dies t
                :visual-subcommands '(("git" "log"))
                :visual-options nil
                :allowed t))
```


### Desktop

The desktop spec locate in ```(emacs-home* "private/self-env-spec.el")```


```lisp
(def-self-env-spec
  :desktop (list :files-not-to-save
                 ".*\.t?gz\\|\.desktop\\|~$\\|\\/ssh[: ]\\|\.elc$"
                 :buffers-not-to-save "^TAGS\\|\\.log"
                 :modes-not-to-save
                 '(dired-mode fundamental-mode rmail-mode)
                 :restore-eager 8
                 :allowed t))
```

### Socks

Using socks proxy when installing packages or browsing web pages.

The socks spec locate in ```(emacs-home* "private/self-env-spec.el")```

```lisp
(def-self-env-spec
  :socks (list :port 32000
               :server "127.0.0.1"
               :version 5
               :allowed nil))

```



### Package

Don't tell me [use-package](https://github.com/jwiegley/use-package), it's
trying to redefine Emacs. Here you can find more simpler and faster way to 
implement almost functions like ```use-pacakge``` does.

There are two types of packages: __basic__(just like Emacs' builtin) 
and __user defined__. To disable __user defined__ packages 
in ```(def-self-path-ref)```, disable both __basic__ and __user defined__
packages can be done in ```(def-self-env-spec)```. 

```lisp
(def-self-env-spec
  :package (list :remove-unused nil
	  :allowed t))
```


* ```:cond```: decide whether to install ```:packages``` or compile ```:compile```
* ```:packages```: a list of package names or tar file names
* ```:compile```: a list of files that should be compiled only or be loaded after be compiled

The default package spec locate in ```(emacs-home* "private/self-package-spec.el")```

```lisp
(def-self-package-spec
  (list
   :cond (executable-find% "latex")
   :packages '(auctex cdlatex))
  (list
   :cond (and (version-supported-p <= 24.4)
              (executable-find% "java"))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking)
   :compile `(,(compile-unit (emacs-home* "config/use-cider.el") t)
              ,(compile-unit (emacs-home* "config/use-cider-autoload.el"))))
  (list
   :cond (and (version-supported-p <= 24.4)
              (executable-find% "docker"))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond (executable-find% "erlc")
   :packages '(erlang))
  (list
   :cond (and (executable-find% "erlc")
              (executable-find% "lfe"))
   :packages '(lfe-mode)
   :compile `(,(compile-unit (emacs-home* "config/use-lfe-autoload.el"))))
  (list
   :cond (and (terminal-supported-p t)
              (platform-supported-unless darwin t)
              (version-supported-p <= 25.1))
   :packages '(ereader))
  (list
   :cond (and (version-supported-p <= 24.4)
              (executable-find% "git"))
   :packages '(magit)
   :compile `(,(compile-unit (emacs-home* "config/use-magit-autoload.el"))))
  (list
   :cond (and (version-supported-p <= 23.2)
              (or (executable-find% "racket")
                  (executable-find% "chicken")))
   :packages '(geiser))
  (list
   :cond (or (executable-find% "sbcl"))
   :packages '(slime)
   :compile `(,(compile-unit (emacs-home* "config/use-slime.el"))
              ,(compile-unit (emacs-home* "config/use-slime-autoload.el")))))
```


## Troubleshooting

* <kbd>M: (clean-compiled-files)</kbd>, then exit and reopen Emacs.
* check ```*Compilation-Log*``` buffer.
* disable the items in ```(emacs-home* "private/self-path.el")```.


[mnemonic](mnemonic.md) | [screenshot](screenshot.md)
