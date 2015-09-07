# Impressum

An interactive printer.

# Overview

# Features

## Images & Plots

You can embed both images and interactive plots in an Impressum REPL. The plots
are rendered using the [d3][d3] library.

## Date & Time

Timestamps can be displayed as easily parsed widgets.

## Colors

Colors, too.

## Files

Pathnames can be opened directly from the REPL.

## Circularity Detection

Impressum detects circular and shared data and ensures objects are always
printed at most once, with future appearances shown as references to the
previously-printed object.

# Usage

Impressum exports just two functions -- `print-html` and `print-html-string`:

* `(impressum:print-html object &optional (stream *standard-output*))` prints an
  object to a stream.

* `(impressum:print-html-string object)` prints the object to a string.

# Extending

# Components

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.

[d3]: http://d3js.org/
