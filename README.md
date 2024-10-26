# A demonstration of shift/reset in Haskell

A simple demonstration of delimited continuations using `shift`/`reset` operators in Haskell, showing how they can be used to manipulate control flow in interesting ways.

## Table of Contents

- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Running the Examples](#running-the-examples)
- [Project Structure](#project-structure)
- [Continuations](#continuations)
- [Continuation Passing Style](#continuation-passing-style)
- [An Analogy: setjmp/longjmp in C](#an-analogy-setjmplongjmp-in-c)
- [Basic Idea Behind shift/reset](#basic-idea-behind-shiftreset)
- [Experiments](#experiments)
  - [Experiment 1](#experiment-1)
  - [Experiment 2](#experiment-2)
  - [Experiment 2'](#experiment-2)
- [Contributing](#contributing)
- [Further Reading](#further-reading)

---

## Getting Started

### Prerequisites
- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Stack](https://docs.haskellstack.org/en/stable/) (Haskell Tool Stack)

The code uses the following packages:
- `transformers` (for the delimited continuation monad)
- `base` (standard Haskell library)

### Running the Examples

The code is set up as a Stack script, so you can run it directly:

```bash
stack run
```

This will execute all three experiments in sequence, showing the
different behaviors of delimited continuations.

## Project Structure

- `app/Main.hs` - Contains all the demonstration code with three experiments
- `README.md` - Documentation and explanations

## License

BSD-3-Clause

---

## Continuations

Suppose a machine that computes the two values of a certain quadratic equation.
It uses the quadratic formula:

```haskell
x = (-b ± sqrt(b^2 - 4ac)) / 2a
```

Let's pivot on the value _b_, supposing that other values are fixed. What
does the machine do?

_b_ gets plugged into the _formula_, of course:

```
x = (-[ ] ± sqrt([ ]^2 - 4ac)) / 2a     <- plug in b into these holes
```

From the perspective of _b_, the "rest" of the formula is called a
_continuation_.

_Reifying_ a continuation means to materialize it as a function.

> **Aside:** What does it mean to "materialize it as a function"?
>
> If there's no environment to capture, then a function is just a pointer to
> some code - a pointer to a sequence of machine instructions. Pretty simple.
>
> An _environment_ is just a collection of variables and their values that
> are not given as arguments to the function. In our example, the environment
> can be said to consist of _a_, _b_, and _c_. Capturing an environment means
> to copy that collection into the heap so that the function can refer to it. If
> all the values are in the stack or read-only data segment, then a function
> might not need to "capture" anything. "Capturing" is only necessary if some
> values need to be kept in the heap for some reason.
>
> A _closure_ is a bundle of the code of a function and its environment, stored
> in RAM. This shows that "materializing as a function" has concrete meaning:
> Be it a pointer to code or a pointer to a closure, it's still a pointer; it can
> be copied around, passed as arguments, etc., and so can be treated as regular
> values.
>
> But closures are machine-talk; they're used to implement functions on real
> machines. They're on the same level of abstraction as raw pointers. They're
> not important to the idea of a continuation. I only mention closures so that
> a reader won't think that "materializing as a function" is some esoteric
> concept.

We can factor out the expression above as a function and an application of that
function:

```
x = (\b -> (-b ± sqrt(b^2 - 4ac)) / 2a) <my specific value of b>
```

But now the function part is free to move around, used as values, etc.

## Continuation Passing Style

In the most elementary sense, _continuation passing style_ (CPS) implements
control flow as the passing of continuations. So, instead of returning a value,
it accepts a continuation as an argument and calls that continuation with the
value. So our quadratic formula function may be written in (basic) CPS as follows:

```haskell
f = \b k -> k ((-b ± sqrt(b^2 - 4ac)) / 2a)
```

In the expression above, _k_ is a continuation.

We see that tail-call optimization is a must for CPS to be efficient.
Indeed, `f` does not ever expect _k_ to return. But this means that, if _f_
were to be implemented on a real machine, it need not use a "call" instruction;
but rather an unconditional jump. Hence, for being able to go so low-level with
control flow, CPS has been historically used to compile certain functional
languages as an intermediate representation.

Imitating that, we can write a fully-explicit CPS version of the quadratic
formula:

```haskell
f = \b k -> 
  square b \b2 -> 
    mult 4 a \four_a -> 
      mult four_a c \four_ac ->
        sub b2 four_ac \discriminant ->
          sqrt discriminant \root ->
            mult 2 a \two_a ->
              div root two_a \result ->
                k result
```

It's perhaps paradoxical: languages that treat functions as first-class
values are often criticized for being too high-level, too removed from the
metal. But CPS allows functions to be just jumps, getting close to the metal.

Associating CPS with unconditional jumps was my first level of intuition, and
it was that idea that clicked for me. But there's much more to CPS than that, I
have learned.

## An Analogy: `setjmp/longjmp` in C

In C, it is possible to "go to" one of the predefined "points" (called
`jmp_buf`s) by using the `setjmp` function, and then return to the point
where `setjmp` was called by using the `longjmp` function. This mechanism is
called "non-local jumps," and it's a fairly advanced and somewhat dangerous
feature.

In some programming circles, the `setjmp`/`longjmp` pair of functions in C
is used to implement exceptions, signals, coroutines, and other similar things.
Imagine asynchronous computing in plain-old C.

Now, I do have to note that it's not mainstream to use `setjmp`/`longjmp` to
implement those things. But a minority of people do use them that way.

Advanced continuation-passing style makes such idioms possible in functional
languages. Even pure languages like Haskell. But unlike `setjmp`/`longjmp`,
the idioms are not dangerous; they're perfectly safe. Not only that, they're
also predictable and composable. Both of them are still complex, but if the
inherent complexity of a problem is high, then judicious use of continuations
can make code much more manageable than otherwise. It's certainly a high-risk,
high-reward technique, but with less risk and possibly greater gain than
`setjmp`/`longjmp`.

*(Incomplete)*

## Basic Idea Behind `shift`/`reset`

The `shift`/`reset` pair is a _delimited continuation_ mechanism.
_Delimited_ means that the continuation is only valid within the "box" created
by "reset." _Delimited continuations_ are a highly desired feature in CPS
programming.

In the past, there was probably only one primitive that people knew about:
the _call-with-current-continuation_ (call/cc) facility. call/cc is a
mechanism for capturing the continuation of the _entire_ program. Hence, it's
said to implement _undelimited continuations_. Unfortunately, undelimited
continuations have gained much notoriety for being the cause of many
memory leaks and performance issues. Renowned Haskell expert Oleg Kiselyov
criticized call/cc, pointing out that delimited continuations are,
mathematically speaking, more _expressive_ than undelimited ones: Delimited
continuations can simulate call/cc, but not vice versa.

There are multiple ways to implement delimited continuations. Here we have
`shift` and `reset` from the `transformers` package. I use them just because
they exist in one of the most popular Haskell packages. Other implementations
in Haskell may be less well maintained than this one, though I won't necessarily
discourage using them. Also, I must call this out: a different implementation,
namely `prompt`/`control0` is implemented as primitive operations in
GHC 9.6.1 (as `prompt#` and `control0#`), to help library authors implement
delimited continuations more efficiently. These primitives use memcpy as the
underlying mechanism, so they're fast and simple for the computer to run. But
let's not get ahead of ourselves. What even are "shift" and "reset"?

"reset" creates sort of a "box" so that an inner "shift" can't escape it.

When control flow jumps to the continuation (say, k) captured by "shift,"
it executes as though that "shift" just finished with the value given to k,
and when the "reset" completes, the control flow returns to where it left off
(that is, inside the shift, right after the k).

## Experiment 1

```
experiment1
1
2
4: 10
3: 9
it says 10
```

Control enters "reset" and "shift" as usual (1, 2).

The "shift" captures the continuation (_k_) in the argument to the closure.
Now, when _k_ is invoked, it jumps to the point right after the "shift" (4).
```haskell
resetT do
  ... -- (1)
  z <- shiftT \(k :: Int -> IO Int) -> do
    ... -- (2)
    v <- lift $ k 10
    ...
  point $ "4: " ++ show z
  ...
```

Notice: the "10" passed to _k_ is the same value bound to _z_.

At this point, let's pause and answer: What does _k_ capture?
The rest of the code following the _z_-binding (which calls _shiftT_)
in the _resetT_ call, being a _continuation_,
is _reified_ into _k_. So, _k_ captures "whatever that would have run in
the closest _resetT_, if any,
should the _shiftT_ not have been there." This is why the call to the
_shiftT_ appears to have terminated prematurely (but not quite).

Now, when does control come back to the _v_-binding (calls into _k_)
inside the _shiftT_ call? We'll answer this question in the next paragraph:

In the next phase, once the lambda inside the parent "reset" completes,
control flow returns to the point where _v_ was bound (3):

```haskell
resetT do
  ... -- (1)
  z <- shiftT \(k :: Int -> IO Int) -> do
    ... -- (2)
    v <- lift $ k 10
    point $ "3: " ++ show v -- [new]
    ...
  point $ "4: " ++ show z
  pure (z - 1)              -- [new]
```

Notice: once the lambda in the "reset" resolves to the value 10 - 1 (= 9), by
the 'pure' call, it's bound to _v_ afterwards (3). Thus, _v_ = 9.

So, we know what _resetT_ does in general.
When a call that is prefixed by _resetT_ finishes, the next instruction is
either the normal order (if none of the continuations in any of the _shiftT_
were called), or else, it is right after the corresponding continuation (like _k_
in this example).

Then,
```haskell
-- v = 9
point $ "3: " ++ show v
pure (v + 1)
```

Thus, the final result is 9 + 1 = 10.

## Experiment 2

Experiments 2 and 2' put the word "delimited" in delimited continuation.
A _resetT_ call gives the scope of what _shiftT_ will capture.

```
experiment2
1
2
3
5: 100
4: 300
6: 600
it says 3000
```

This experiment demonstrates nested delimited continuations.
Let's trace the execution:

1. Outer `resetT` begins, prints "1"
2. Inner `resetT` begins, prints "2"
3. `shiftT` captures its continuation up to the **inner `resetT`**, prints "3"
4. `k 100` is called:
   - Binds `b = 100`
   - Prints "5: 100"
   - Returns `b * 3` (300)
5. This 300 becomes `c`
6. Prints "4: 300"
7. Returns `c * 2` (600)
8. This 600 becomes `a`
9. Prints "6: 600"
10. Finally returns `a * 5` (3000)

## Experiment 2'

```
experiment2'
1
2
3
5: 100
6: 300
4: 1500
it says 3000
```

This version is identical except for one crucial difference:
the inner `resetT` is removed. This changes where the continuation extends to:

1. `resetT` begins, prints "1"
2. Prints "2"
3. `shiftT` now captures its continuation up to the **outer `resetT`**,
prints "3"
4. `k 100` is called:
   - Binds `b = 100`
   - Prints "5: 100"
   - Computes `b * 3` (300)
   - Binds this to `a`
   - Prints "6: 300"
   - Returns `a * 5` (1500)
5. This 1500 becomes `c`
6. Prints "4: 1500"
7. Returns `c * 2` (3000)

The key difference is the scope of what `shiftT` captures. In `experiment2`,
the inner `resetT` creates a boundary, so the continuation only includes
operations up to that boundary. In `experiment2'`, the continuation extends
all the way to the outer `resetT`, including the `* 5` operation.

This demonstrates how `resetT` acts as a delimiter, controlling exactly how
much of the computation is captured by `shiftT`. The final result (3000) is
the same in both cases, as multiplication is both associative and commutative,
but the path to get there differs because of how the continuation is segmented.

(Maybe I'll turn Main.hs into a literate Haskell file someday so that the
explanation becomes more clear.)

## Contributing

This is a demonstration repository intended to help understand delimited
continuations. I'm a student myself, and I finally managed to crack the code
on `shift`/`reset` literally today (before then, they were like Egyptian
hieroglyphs). So I'm sure there are errors. But I hope that others can take
the shortcut that I took and avoid the "thought pitfalls" (if you will) that
I fell into. If you find any errors or have suggestions for clearer examples
or explanations, please feel free to open an issue or submit a pull request.

## Further Reading

- [Delimited Continuations in Haskell](https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-Cont.html) documentation
- [Oleg Kiselyov's work on Delimited Continuations](http://okmij.org/ftp/continuations/)
