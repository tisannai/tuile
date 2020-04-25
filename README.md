# Overview

*tuile* is a collection of utilities for Guile.


# pr

`pr` provides simple printing with support for padding fields. The
printer functions accept also lists as arguments which will be
flattened and list entries are all converted (recursively) to string
equivalents. Hence, the user can just "throw" stuff to the printer
functions.



# como

`como` is a command line parsing library. Simple declaration based
command line definition, which also defines how usage help is
displayed.

Option types:

| Type       | Optional | Count  | Exclusive | Value      |
| :-----     | :-----   | :----- | :-----    | :-----     |
| help       | Yes      | 0,1    | Yes       | No         |
| switch     | Yes      | 0,1    | No        | No (T,F)   |
| single     | No       | 1      | No        | String     |
| opt-single | Yes      | 0,1    | No        | String     |
| repeat     | No       | 1,N    | No        | List       |
| opt-repeat | Yes      | 0,1,N  | No        | List       |
| multi      | No       | 1,N    | No        | List       |
| opt-multi  | Yes      | 0,1,N  | No        | List       |
| any        | No       | 0,1,N  | No        | Empty,List |
| opt-any    | Yes      | 0,1,N  | No        | Empty,List |
| default    | Yes      | 0,1,N  | No        | Empty,List |

`repeat` is the same as `multi` except with `repeat` only one argument
is parsed from command line after option switch. If `repeat` option is
given multiple times, then `repeat` value list has multiple entries.

`default` is same as `opt-any` except it doesn't need named option
switch.



# dbg

`dbg` provides interactive debugging for Guile. For some reason Guile
2.2 is missing these features. This module requires more work.



# log

`log` is a logging facility for programs. Log support log filtering
based on Log Groups.



# utils

`utils` includes miscellaneous support functions which are missing
from Guile. Provides, for example, a significantly easier regexp usage
than standard Guile.


# fn

`fn` provides file name and path queries. `fn` extracts information
from file name string and the results can be used to create new file
paths.
