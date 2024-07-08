# Session Type Prefix Stripping

Works with GHC 9.2 up to & including 9.10

## Code

The syntax is defined in `src/BSession/Syntax.hs`. Recursion variables are encoded using de-Bruijn indices. For better error messages indices are annotated by their original identifier (ignored when checking for equality). The code differentiates between session types ending in `END` and session types ending in `RET` (written as `..`).

The main workhorse is `stripPrefix` in `src/BSession/Prefix.hs`. The result of prefix stripping is the remaining continuation session. There are three kinds of errors which may occur:

* `ErrorNotAPrefix`: the given prefix can never be a valid prefix of the given session

   For example, `!Int. ..` is never a valid prefix of `?Int. end`.
  
* `ErrorNoCont`: no amount of unfolding will produce a continuation

  For example, `rec X. !T.X` is both a valid session and a valid prefix. But stripping this prefix leaves no continuation.

* `ErrorContConflict`: different branches produce conflicting session types

  Stripping the prefix `+{ .. ; .. }` from `+{ !T.end ; end }` results in a conflict between `!T.end` and `end` for the continuation session.


## Interactive playground

`cabal run`

### Session type syntax

* **single communications:** `?T.S`, `!T.S`

  `T` may be any identifier

* **branching** (not labeled): `+{ S₁ ; … ; Sₙ }`, `&{ S₁ ; … ; Sₙ }` ($n \geq 1$)

  When comparing branching session types for equality the ordering of the branches is of importance.

* **recursive types:** `rec X. S`

  `X` may be any identifier

* **end:** `end` is used in the complete session type, `..` indicates the “hole” in the prefix session type.

### Example

* **session type**

  `!Int. +{ ?Int.end ; end }`
  
* **prefix type**

  `!Int. +{ ?Int. .. ; .. }`

* **result**

  `end`
