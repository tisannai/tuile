# Overview

*tuile* is a collection of utilities for Guile.

# pr

Simple printing with support for padding fields. The printer functions
accept also lists as arguments which will be flattened and list
entries are all converted (recursively) to string equivalents. Hence,
the user can just "throw" stuff to the printer functions.

# dbg

Interactive debugging for Guile. For some reason Guile 2.2 is missing
these features.
