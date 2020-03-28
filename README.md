# Overview

*tuile* is a collection of utilities for Guile.

# pr

Simple printing with support for padding fields. The printer functions
accept also lists as arguments which will be flattened and list
entries are all converted (recursively) to string equivalents. Hence,
the user can just "throw" stuff to the printer functions.

# como

Como is a command line parsing library. Simple declaration based
command line definition, which also defines how usage help is
displayed.

# dbg

Interactive debugging for Guile. For some reason Guile 2.2 is missing
these features.

# log

Logging facility for programs. Log support log filtering based on Log
Groups.

# utils

Miscellaneous support functions which are missing from
Guile. Provides, for example, a significantly easier regexp usage than
standard Guile.
