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
* write file: <kbd>C-x C-w</kbd> same as save as
* kill whole line: <KBD>C-S-DEL</KBD>
* kill all spaces at point: <kbd>M-\</kbd>
* kill all spaces except one at point: <KBD>M-SPC</KBD>
* delete indentation to join line to previous line: <kbd>M-^</kbd>
* dynamic abbreviation: <kbd>M-/</kbd>
* query replace: <kbd>M-%</kbd>
* what cursor position: <kbd>C-x =</kbd>
* upcase region: <kbd>C-x C-u</kbd>
* downcase region: <kbd>C-x C-l</kbd>
* transpose characters: <kbd>C-t</kbd>
* transpose words: <kbd>M-t</kbd>
* insert parentheses: <kbd>M-(</kbd>, wrap selection in parentheses
* toggle read-only mode: <kbd>C-x C-q</kbd>
* toggle input method: <kbd>C-\</kbd>, switch to __TeX__ <kbd>C-x-RET C-\</kbd>
* insert char: <kbd>C-x 8 RET</kbd> GREEK SMALL LETTER LAMBDA
* quoted insert: <kbd>C-q</kbd>, such as page break <kbd>C-q C-l</kbd>, use <kbd>C-x [</kbd> or <kbd>C-x ]</kbd> to backward or forward


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
* find file other frame: <kbd>C-x 5 C-f</kbd>, or <kbd>C-x 5 f</kbd>
* display buffer other frame: <kbd>C-x 5 C-o</kbd>, or <kbd>C-x 5 b</kbd>
* find tag other frame: <kbd>C-x 5 .</kbd>
* delete frame: <kbd>C-x 5 0</kbd>
* delete other frames: <kbd>C-x 5 1</kbd>
* make frame command: <kbd>C-x 5 2</kbd>
* dired to other frame: <kbd>C-x 5 d</kbd>
* other frame: <kbd>C-x 5 o</kbd>
* find file read only other frame: <kbd>C-x 5 r</kbd>


## Window
* other window: <kbd>C-x o</kbd>
* dired other window: <kbd>C-x 4 d</kbd>
* find file in other window: <kbd>C-x 4 C-f</kbd>, or <kbd>C-x 4 f</kbd>
* display buffer: <kbd>C-x 4 C-o</kbd>, display the buffer in another window
* find tag other window: <kbd>C-x 4 .</kbd>
* kill buffer and window: <kbd>C-x 4 0</kbd>, just like <kbd>C-x 0</kbd> except kill the buffer
* switch to buffer other window: <kbd>C-x 4 b</kbd>
* clone indirect buffer other window: <kbd>C-x 4 c</kbd>, clone the buffer in another window


## Register
* window configuration to register: <kbd>C-x r w <REG></kbd>, <REG> is single character, 
it can not accross sessions.
* frame configuration to register: <kbd>C-x r f <REG></kbd>
* point to register: <kbd>C-x r <SPC> <REG></kbd>
* jump to register: <kbd>C-x r j <REG></kbd>
* copy to register: <kbd>C-x r s <REG></kbd>
* insert register: <kbd>C-x r i <REG></kbd>
* view register


## Bookmarks
Unlike registers, bookmarks have long names, and they persist automatically from one Emacs session to the next.

* set bookmark: <kbd>C-x r m</kbd>
* set named bookmark: <kbd>C-x r m BOOKMARK</kbd>
* jump to bookmark: <kbd>C-x r b BOOKMARK</kbd>
* list all bookmarks: <kbd>C-x r l</kbd>
* save bookmark to file: <kbd>M-x bookmark-save</kbd>


## Region and Rectangle
* kill rectangle: <kbd>C-x r k</kbd>
* delete rectangle: <kbd>C-x r d</kbd>
* yank rectangle: <kbd>C-x r y</kbd>
* open rectangle: <kbd>C-x r o</kbd>, insert blank space to fill the space of the region-rectangle
* clear rectangle: <kbd>C-x r c</kbd>
* string rectangle: <kbd>C-x r t</kbd>, replace rectangle contents with STRING on each line
* string insert rectangle: <kbd>C-x r g</kbd>
* <kbd>M-x delete-whitespace-rectangle</kbd>


## Keyboard Macro
* start recording macro: <kbd>C-x (</kbd>
* stop recording macro: <kbd>C-x )</kbd>
* playback macro: <kbd>C-x e</kbd>
* apply macro to region lines: <kbd>C-x C-k r</kbd>


## Shell
* EShell: <kbd>M-x eshell</kbd>
* Shell: <kbd>M-x shell</kbd>
* Ansi-Term: <kbd>M-x ansi-term</kbd>


## Remote
It's the duty of [TRAMP](https://www.gnu.org/software/tramp/)
* non-sudo: <kbd>C-x C-f /ssh:<remote-id>:/path/to/file RET</kbd>, *<remote-id>* such as *x@y.z* or *xyz* in .ssh/config entries.
* sudo: <kbd>C-x C-f /ssh:<remote-id>|sudo:<remote-host>:/path/to/file</kbd>, *<remote-host>*
such as *x@localhost* or *localhost* if the user *x* is a sudoer.
* eshell remote: <kbd>cd /ssh:<user>@<remote>:<dir></kbd>


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
insert multiple lines with _prefix + random number_, <kbd>C-u C-[-:</kbd>
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
sort by 2nd field: <kbd>C-u 2 M-x sort-fields</kbd>, ```reverse-region```
