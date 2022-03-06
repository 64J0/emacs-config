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

## Learning Elisp - Emacs Lisp

Since Emacs could be configured using Elisp it is totally useful to learn this programming language in order to create new functionalities or even get more familiar with this tool.

In order to learn this I'm currently using this reference: [(Book) Introduction to programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html).

After finishing this first book, I plan to read [Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp](https://github.com/norvig/paip-lisp).

## Some cool packages link

* [org-drill](https://orgmode.org/worg/org-contrib/org-drill.html#:~:text=Running%20the%20drill%20session) reference for running the drill session. This part of the docs is very good since it explains what each number means and how to run the session passing the scope to read our drills.
  * Before publishing an org-drill deck we should use `org-drill-strip-all-data` to remove personal scheduling data.
* [Emacs and BibTeX](https://lucidmanager.org/productivity/emacs-bibtex-mode/).
