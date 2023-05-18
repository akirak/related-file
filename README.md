# related-file

This is a Prolog program that prints the related files of a file.
My plan is to integrate Emacs with this program.

## Installation

On Linux, you can use Nix to install the program.

The program is built with [Ciao](https://github.com/ciao-lang) compiler.
Unfortunately, the compiler is marked as broken on macOS, so you can use it only
on Linux. I am looking for an alternative Prolog implementation that is stable
enough and supports producing a self-contained executable.

## Usage

Synopsis:

``` shell
related-file FILE
```

Each output line is formatted as follows:

    TYPE FILENAME

where `TYPE` indicates the type of the relationship and `FILENAME` is an
absolute path of the related file.
