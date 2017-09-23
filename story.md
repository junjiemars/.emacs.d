Story
=======

I'm a nerd with Emacs, it's awesome if I can [Learn Emacs in less then Ten Years](http://edward.oconnor.cx/2009/07/learn-emacs-in-ten-years). So, I decided to start from Level One not the Level Zero after read Emacs' tutorial (C-h t). Emacs is the most powerful editor on the world there is no **one of**, it's programmable, elegant and **self-documenting**, so you can start from Level Zero if you had time may be less than 10 years to read and try Emacs.

After falling in love with [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), I know it's the time to make Emacs being a part of my body and mind, not just open a file, navigating, editing, and save it. The Level One of mine start from [this is a Clojure-friendly emacs config](https://github.com/flyingmachine/emacs-for-clojure), and [A reasonable Emacs config](https://github.com/purcell/emacs.d). But, those ones neither can suit for my daily use on different machines.

The adaptive Emacs which I need is more stable, more smaller, more faster and more flexible which can be run on anywhere where Emacs can run. So, I decide to build a new one.


* [Features](#features)
* [Requirements](#requirements)
* [Where](#where)
* [What](#what)
* [Install](#install)
* [Cook](#cook)
* [Mnemonic](mnemonic.md)

Now, let's start from the Level Two. And one more thing: teach youself some little Elisp (M-x info).

## Features
* __Carefully designed package manager__: just load what's your need, so it is very stable, and fast, the loading time less than 1s on most machines.
* __Adaptive__: Can run on any OS, GUI or Terminal, from most recent Emacs's versions to ancient ones, and one Emacs's configuration suit for all coexisting Emacs on the same machine.
* __Consistent__: Whereever you can play with Emacs always behaves in the same way.
* __Awesome style__: See it yourself.


## Requirements
* Emacs installation, version 22.1.1+ is ok, but version 24+ is more suitable;
* Any keyboard, not mouse;


## Where
* Linux, whatever GUI or Terminal;
* OSX, whatever GUI or Terminal;
* Windows, whatever GUI or Terminal;


## What
* You are the master of your Emacs, ...
* Smart navigating, find, grep, ...
* Org Mode that's your new life, ...
* Lisp programming, Common Lisp, Scheme, sexy Clojure/Clojurescript, ...
* Bash on Windows, ...
* Emacs can do ...


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


* First run: start Emacs then exit it, take a while if your Emacs version 24+;
* Non-First run: start Emacs then run ```(clean-compiled-files)```, then exit;

Now, start Emacs again, it will works perfectly, but if you want to more control and more features, such as themes, packages, and fonts etc., see next section: [cook](#cook)


## Cook

Suppose your Emacs HOME at ```~/.emacs.d``` and annotated it as ```emacs-home``` .

Your private configuration directory at ```emacs-home/private/``` 
and I abbreviate it as ```private/```. All things you can rename, 
move, delete, and except the ```private/self-path.el``` file.


After you pull the last version from ```https://github.com/junjiemars/.emacs.d.git```, 
in your ```emacs-home/private``` dirctory, there are default configuration files:


* content of ```private/self-path.el```
```lisp
(setq self-def-paths
      (list
       :env-spec (emacs-home* "private/self-env-spec.el")
       :prologue nil 
       :package-spec nil
       :epilogue nil))
```

* ```:env-spec``` specify the _basic environement_ specification file;
* ```:prologue``` specify the _prologue_ file;
* ```:package-spec``` specify the _package definition_ specificiation file;
* ```:epilogue``` specify the _epilogue_ file;



### Using your private Emacs's configuration anywhere

Host ```private/``` in [GitLab](https://gitlab.com), [GitHub](https://github.com), 
or your private _versioning server_, then in anywhere you can pull it down 
into any platform, it will works well: keep the same styles and the same behaviors.


### Theme

* associate ```private/self-env-spec.el``` with ```private/self-path.el```
```lisp
(setq self-def-paths
      (list
       ;; ...
       :env-spec (emacs-home* "private/self-env-spec.el")
       ;; ...
       ))
```

* ```private/self-env-spec.el``` looks like:
```lisp
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t)
  ;; ...
)

;; :name => theme's name
;; :path => where theme file located, if nil then load the built-in theme
;; :allowed => t to load

```

* restart Emacs

### CJK on Windows or Linux

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
           (and (version-supported-p '<= 24.0)
                (bin-exists-p "virtualenv")))
   :packages '(elpy)
   :compile `(,(emacs-home* "config/setup-python.el"))))
```
this _package-spec_ used to support _Python_ programming, 
[default package spec](config/sample-self-package-spec.el)

### Common Lisp programming

### Clojure programming

### Python programming


