# Mnemonic


## Emacs's Documents
It's **self-documenting** and great, keep reading it frequently.

* Tutorial: ```C-h-t```
* Help for Help: ```C-h C-h```
* Emacs manual: ```C-h r```
* Apropos command: ```C-h a```
* Info: ```C-h i```
* Info of mode: ```C-h i d m <x>``` *<x>* is the mode
* Mode: ```C-h-m``` see all the key bindings and documentation of current buffer
* Where is command: ```C-h w``` which keystrokes binding to a given command
* Function: ```C-h-f``` display documentation of the given function
* Variable: ```C-h-v``` display documentation of the given variable
* Keybinding: ```C-h-k``` display documentation of the function invoked by the given keystrokes
* Prefix keybindings: ```<prefix> C-h``` see all key bindings for given prefix keystrokes
* Keybinding briefly: ```C-h c```, which command for given keystroke
* Message: ```C-h e``` see the logging of echo area message
* Man: ```M-x man``` view UNIX manual page
* Woman: ```M-x woman``` view UNIX manual page without ```man``` program
* Coding system: ```C-h C``` describe coding system
* Colors: ```M-x list-colors-display``` display names of defined colors and show what
they look like


## Motion
* goto line: ```M-g g```
* goto nth char: ```M-g c```
* pop global mark: ```C-x C-@```
* jump to definition: ```M-.```
* pop back: ```M-,```, on Terminal is ```M-*```

## Interaction
* \*scratch\* buffer
* via Elisp: ```M-:```
* via Shell Command: ```M-!```, see *Info>Emacs>Shell*
* in Dired mode: ```!```, do shell command
* insert shell output: ```C-u M-!```
* browse-url: ```C-c b```

## Editing
* kill whole line: ```C-S-DEL```
* kill all spaces at point: ```M-\```
* kill all spaces except one at point: ```M-SPC```
* delete indentation to join line to previous line: ```M-^```
* dynamic abbreviation: ```M-/```
* query replace: ```M-%```
* what cursor position: ```C-x =```
* transpose characters: ```C-t```
* transpose words: ```M-t```
* insert parentheses: ```M-(```, wrap selection in parentheses
* toggle read-only mode: ```C-x C-q```
* toggle input method: ```C-\```, switch to __TeX__ ```C-x-RET C-\```
* quoted insert: ```C-q```, such as page break ```C-q C-l```


## Basic sexp commands
* ```forward-sexp```: ```C-M-f```
* ```backward-sexp```: ```C-M-b```
* ```kill-sexp```: ```C-M-k```, delete forward one sexp
* ```transpose-sexp```: ```C-M-t```
* ```backward-up-list```: ```C-M-u```, move up out of an sexp
* ```down-list```: ```C-M-d```, move down into a nested sexp
* ```backward-list```: ```C-M-p```, match parentheses backward
* ```pp-eval-last-sexp```: pretty print


## Window
* dired other window: ```C-x 4 d```
* find file in other window: ```C-x 4 C-f```, or ```C-x 4 f```
* display buffer: ```C-x 4 C-o```, display the buffer in another window
* find tag other window: ```C-x 4 .```
* kill buffer and window: ```C-x 4 0```, just like ```C-x 0``` except kill the buffer
* switch to buffer other window: ```C-x 4 b```
* clone indirect buffer other window: ```C-x 4 c```, clone the buffer in another window

## Frame
* find file other frame: ```C-x 5 C-f```, or ```C-x 5 f```
* display buffer other frame: ```C-x 5 C-o```, or ```C-x 5 b```
* find tag other frame: ```C-x 5 .```
* delete frame: ```C-x 5 0```
* delete other frames: ```C-x 5 1```
* make frame command: ```C-x 5 2```
* dired to other frame: ```C-x 5 d```
* other frame: ```C-x 5 o```
* find file read only other frame: ```C-x 5 r```


## Register
* window configuration to register: ```C-x r w <REG>```, <REG> is single character, 
it can not accross sessions.
* frame configuration to register: ```C-x r f <REG>```
* point to register: ```C-x r <SPC> <REG>```
* jump to register: ```C-x r j <REG>```
* copy to register: ```C-x r s <REG>```
* insert register: ```C-x r i <REG>```
* view register

## Region and Rectangle
* kill-rectangle: ```C-x r k```
* delete-rectangle: ```C-x r d```
* yank-rectangle: ```C-x r y```
* open-rectangle: ```C-x r o```, insert blank space to fill the space of the region-rectangle
* clear-rectangle: ```C-x r c```
* string-rectangle: ```C-x r t STRING <RET>```, replace rectangle contents with STRING on each line
* ```M-x delete-whitespace-rectangle```
* ```M-x string-insert-rectangle <RET> STRING <RET>```

## Keyboard Macro
* start recording macro: ```C-x (```
* stop recording macro: ```C-x )```
* playback macro: ```C-x e```
* apply macro to region lines: ```C-x C-k r```


## Shell
* EShell: ```M-x eshell```
* Shell: ```M-x shell```
* Ansi-Term: ```M-x ansi-term```


## Remote
It's the duty of [TRAMP](https://www.gnu.org/software/tramp/)
* non-sudo: ```C-x C-f /ssh:<remote-id>:/path/to/file RET```, *<remote-id>* such as *x@y.z* or *xyz* in .ssh/config entries.
* sudo: ```C-x C-f /ssh:<remote-id>|sudo:<remote-host>:/path/to/file```, *<remote-host>*
such as *x@localhost* or *localhost* if the user *x* is a sudoer.
* eshell remote: ```cd /ssh:<user>@<remote>:<dir>```




