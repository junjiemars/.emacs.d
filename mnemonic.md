# Mnemonic

* [Emacs Documents](#emacs-documents)
* [Motion](#motion)
* [Interaction](#interaction)
* [Editing](#editing)
* [Basic sexp commands](#basic-sexp-commands)
* [Frame](#frame)
* [Window](#window)
* [Register](#register)
* [Bookmarks](#bookmarks)
* [Region and Rectangle](#region-and-rectangle)
* [Keyboard Macro](#keyboard-macro)
* [Shell](#shell)
* [Remote](#remote)
* [LaTex Requirements](#latex-requirements)
* [Tips](#tips)
  * [Editing Tips](#editing-tips)

## Emacs Documents
It's **self-documenting** and great, keep reading it frequently.

* Tutorial: <kbd>C-h-t</kbd>
* Help for Help: <kbd>C-h C-h</kbd>
* Emacs manual: <kbd>C-h r</kbd>
* Apropos command: <kbd>C-h a</kbd>
* Info: <kbd>C-h i</kbd>
* Info of mode: <kbd>C-h i d m <x></kbd> *<x>* is the mode
* Mode: <kbd>C-h-m</kbd> see all the key bindings and documentation of current buffer
* Where is command: <kbd>C-h w</kbd> which keystrokes binding to a given command
* Function: <kbd>C-h-f</kbd> display documentation of the given function
* Variable: <kbd>C-h-v</kbd> display documentation of the given variable
* Keybinding: <kbd>C-h-k</kbd> display documentation of the function invoked by the given keystrokes
* Prefix keybindings: <kbd><prefix> C-h</kbd> see all key bindings for given prefix keystrokes
* Keybinding briefly: <kbd>C-h c</kbd>, which command for given keystroke
* Message: <kbd>C-h e</kbd> see the logging of echo area message
* Man: <kbd>M-x man</kbd> view UNIX manual page
* Woman: <kbd>M-x woman</kbd> view UNIX manual page without ```man``` program
* Coding system: <kbd>C-h C</kbd> describe coding system
* Colors: <kbd>M-x list-colors-display</kbd> display names of defined colors and show what
they look like


## Motion
* goto line: <kbd>M-g g</kbd>
* goto nth char: <kbd>M-g c</kbd>
* jump between buffers: <kbd>C-x C-@</kbd>, jumps to the global mark acrross buffers
* jump in buffer: <kbd>C-u C-@</kbd>
* jump to definition: <kbd>M-.</kbd>
* pop back to where <kbd>M-.</kbd> was last invoked: <kbd>M-,</kbd>, on Terminal is <kbd>M-*</kbd>


## Interaction
* \*scratch\* buffer
* via Elisp: <kbd>M-:</kbd>
* via Shell command: <kbd>M-!</kbd>, insert shell output: <kbd>C-u M-!</kbd>, see *Info>Emacs>Shell*
* in Dired mode: <kbd>!</kbd>, do shell command
* region as input to Shell command: <kbd>M-|</kbd>, insert from shell output: <kbd>C-u M-|</kbd>
* browse-url: <kbd>C-c b</kbd>
* filename of current buffer: ```(buffer-file-name (current-buffer)```


## Editing
* write file: ```C-x C-w``` same as save as
* kill whole line: ```C-S-DEL```
* kill all spaces at point: ```M-\```
* kill all spaces except one at point: ```M-SPC```
* delete indentation to join line to previous line: ```M-^```
* dynamic abbreviation: ```M-/```
* query replace: ```M-%```
* what cursor position: ```C-x =```
* upcase region: ```C-x C-u```
* downcase region: ```C-x C-l```
* transpose characters: ```C-t```
* transpose words: ```M-t```
* insert parentheses: ```M-(```, wrap selection in parentheses
* toggle read-only mode: ```C-x C-q```
* toggle input method: ```C-\```, switch to __TeX__ ```C-x-RET C-\```
* quoted insert: ```C-q```, such as page break ```C-q C-l```, use ```C-x [``` or ```C-x ]``` to backward or forward


## Basic sexp commands
* ```forward-sexp```: ```C-M-f```
* ```backward-sexp```: ```C-M-b```
* ```kill-sexp```: ```C-M-k```, delete forward one sexp
* ```transpose-sexp```: ```C-M-t```
* ```backward-up-list```: ```C-M-u```, move up out of an sexp
* ```down-list```: ```C-M-d```, move down into a nested sexp
* ```backward-list```: ```C-M-p```, match parentheses backward
* ```pp-eval-last-sexp```: pretty print


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


## Window
* other window: ```C-x o```
* dired other window: ```C-x 4 d```
* find file in other window: ```C-x 4 C-f```, or ```C-x 4 f```
* display buffer: ```C-x 4 C-o```, display the buffer in another window
* find tag other window: ```C-x 4 .```
* kill buffer and window: ```C-x 4 0```, just like ```C-x 0``` except kill the buffer
* switch to buffer other window: ```C-x 4 b```
* clone indirect buffer other window: ```C-x 4 c```, clone the buffer in another window


## Register
* window configuration to register: ```C-x r w <REG>```, <REG> is single character, 
it can not accross sessions.
* frame configuration to register: ```C-x r f <REG>```
* point to register: ```C-x r <SPC> <REG>```
* jump to register: ```C-x r j <REG>```
* copy to register: ```C-x r s <REG>```
* insert register: ```C-x r i <REG>```
* view register


## Bookmarks
Unlike registers, bookmarks have long names, and they persist automatically from one Emacs session to the next.

* set bookmark: ```C-x r m```
* set named bookmark: ```C-x r m BOOKMARK```
* jump to bookmark: ```C-x r b BOOKMARK```
* list all bookmarks: ```C-x r l```
* save bookmark to file: ```M-x bookmark-save```


## Region and Rectangle
* kill rectangle: ```C-x r k```
* delete rectangle: ```C-x r d```
* yank rectangle: ```C-x r y```
* open rectangle: ```C-x r o```, insert blank space to fill the space of the region-rectangle
* clear rectangle: ```C-x r c```
* string rectangle: ```C-x r t```, replace rectangle contents with STRING on each line
* string insert rectangle: ```C-x r g```
* ```M-x delete-whitespace-rectangle```


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


## LaTex Requirements

on Darwin
* ImageMagick
* texlive-basic
* texlive-fonts-recommended
* texlive-latex-extra
* texlive-latex-recommended
* texlive-pictures
* texlive-plain-generic

on Ubuntu:

on Windows:
* ImageMagick
* MiKTex

## Tips

### Editing Tips

#### Insert Region
```txt
aaaa 9
aaaa 6
aaaa 1
aaaa 1
aaaa 0
```
insert multiple lines with _prefix + random number_, ```C-u C-[-:```
```lisp
(dotimes (x 5)
	(insert-string (format "aaaa %s\n" (random 10))))
```


#### Sort Region
```txt
aaaa bbb
aaaa ddd
aaaa zzz
aaaa xxx
aaaa uuu
```
sort by 2nd field: ```C-u 2 M-x sort-fields```, ```reverse-region```
