# My personal .emacs config

According to the docs, Emacs is an extensible, customizable, free/libre text editor - and more. **Much more!**

> At its core is an interpreter for Emacs Lisp, a dialect of the Lisp programming language with extensions to support text editing.

Reference: [Emacs website](https://www.gnu.org/software/emacs/).

## Install using apt

`apt` is the package manager available for Ubuntu Linux, at least at the version 20.04 that is the version I'm currently using. To install Emacs using this package manager is pretty straigh forward:

```bash
$ sudo apt install emacs
```

## Install using terminal

```bash
# download emacs version 27.2
$ wget https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz

# extract the content
$ tar xvzf emacs-27.2.tar.gz
$ cd emacs-27.2/
$ ./configure
$ make
$ sudo make install
```
