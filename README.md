# Music Notation Grammar

The goal of this project is to write an executable specification of the modern music notation.
This executable specification is a grammar defined using prolog Definite Clause Grammar (DCG) notation and links spatial relationships of music primitives with semantic relationships of music notation elements.

Concretely, the same code used to describe the modern music notation can be used in two ways:

* using a MusicXML file describing the semantic of a music score as input, the grammar can typeset the music score by generating graphical primitives and their relative positions;
* using a list of music graphical primitives and their positions as input, the grammar can reconstruct the MusicXML file describing the semantic of the music score.

The first use is usually done using music typesetting software such as MuseScore, Lilypond, Sibelius or Finale.
The second use is called Optical Music Recognition (OMR) and is an active research domain of document recognition.
The only open source OMR solution is called Audiveris and works okay for music scores produced by typesetting software.

## Features

Here is a list of currently supported music notation features:

* single measure
  * 5 line staff with a single barline
  * attributes
    * g clef
    * 4/4 time signature
    * key signature
  * note
    * noteheads, stem and flags: whole, half, quarter, 8th, 16th, etc
    * dotted note
    * flat, sharp, natural accidentals
    * ledger lines
    * single beam for grouped notes

See the data folders to have an idea of the different supported music notation elements.

See the [documentation](https://kwon-young.github.io/music/) for a list of defined predicates used in this grammar.

## Why ?

While the origin of this project is quite deep and complicated, the main reason driving this project is the realization that by using a carefully selected set of tools and formalism, the same code __could__ be used to do both music score typesetting __and__ recognition.
The fundamental idea behind this realization is that, for example, when trying to __describe__ the relationship between two graphical objects, we do not want to repeat the exact same relationship depending on the direction of evaluation.

If you take the relationship between a note head and its stem, non logic formalism would need to pick an evaluation order, such as:

1. defining the coordinate of the note head;
2. compute the position of the junction between the note head and the stem;
3. compute the rest of the coordinates of the stem depending on the position of the junction with the note head.

In this formalism, if you ever wanted to define the position of the note head given a specific stem, you would need to rewrite the exact same __relation__, but in the reverse order.
Using prolog and constraint logic programming over reals, we can specify this relationship only once, and it will work in any directions.

If you want to deep dive into the origin of this project, you can read the DMOS section of the documentation.

## How does this work ?

To have a concrete overview of the machinery used in this project see the first part of this section.

To have a high level overview of how this project works, see the second part of this section.

### Concretely

As previously said, the main particularity of this project is the ability to use the grammar in both direction, to typeset a music score and to recognize a music score from a list of graphical primitives.

#### Music Typesetting

For the music typesetting part, we start from a MusicXML file, and produces a special `.pl` that contains a prolog list of graphical primitives that constitute the engraved music scores.

Much more work is required to turn this grammar into a full blown engraver.
Currently, only a few relationship are specified in the grammar, enough to do OMR, but not enough to fully ground the coordinates of the graphical primitives while typesetting.
Then, the prolog list of graphical primitives should be transformed in something like a SVG file using actual music glyph.

But the important thing is that the grammar actually runs in this direction, and produce the correct set of graphical primitives to typeset the music score in the MusicXML file.

#### OMR

The main enabler of this project is the [Verovio](https://www.verovio.org/index.xhtml) music typesetting (or engraving) software.
The particularity of this engraver is its ability to output engraved music scores in the SVG format that still contains the label of music glyph.

So the workflow is the following:

1. First, engrave a MusicXML file using Verovio and produce a SVG file that describes the labels and coordinates of graphical primitives;
2. Use the `svg2pl.py` script to transform the SVG file into a prolog list containing all the graphical primitives that constitute the music score;
3. Run the grammar using the list of graphical primitives as input and produce a MusicXML file describing the music score;
4. This workflow allows us to simply test the whole workflow by comparing the input MusicXML file with the output MusicXML to see if everything is working as expected.

In the future, this project should be able to take PDFs of engraved music score as input.
However, I still need to figure out how to extract graphical primitives from PDFs.
The utility `pdftocairo` of [poppler](https://gitlab.freedesktop.org/poppler/poppler/-/tree/master) would be the most straightforward tool but glyph are exported without their labels.
It shouldn't be too hard to create a glyph classifier, or modify Cairo to keep glyph labels in its SVG output.

With this, we would be able to use multiple engravers for testing the OMR aspect of the grammar!

### In Theory

The grammar is written using prolog.

> Prolog is a programming language that is rooted in classical logic.

See [The Power of Prolog Introduction](https://www.metalevel.at/prolog/introduction) if you want to know more about prolog.

In brief, prolog allows us to specify relationship between things, instead of specifying sequence of computation steps like in other imperative or functional programming language.
As an example, let's take the classical example in logic which is a syllogism:

> All men are mortals.
> Socrates is a man.
> Therefore, Socrates is mortal.

A syllogism always contains three parts: two true premises which implies a conclusion.
Using prolog, we can first logically describe the two true premises:

```prolog
% All men are mortals
% Or in prolog way: Person is mortal if Person is a man.
mortal(Person) :-
  man(Person).

% Socrates is a man.
% We specify that Socrates is a man using a prolog fact.
man(socrates).
```

With this we are now ready to use prolog to derive conclusions from premises:

```prolog

% is Socrates a mortal ? 
?- mortal(socrates).
true.

% who is mortal ?
?- mortal(Person).
Person = socrates.  % to prolog knowledge, only Socrates is a mortal.

% is Zeus a mortal ?
?- mortal(zeus).
false.  % to prolog knowledge, Zeus is not a mortal.
```

Notice how we were able to use the `mortal` predicate in different directions, both using grounded and variable terms as argument the predicate.

Using this property of prolog, we would like to be able to specify the relationship between the semantics of a music score, represented by a MusicXML structure, and its graphical representation, which would be a list of graphical primitives:

```
music(MusicXML, GraphicalPrimitives) :-
  % TODO
  ...
```

By the way, prolog has come since the beginning with a special notation for DCG, which are commonly used to describe a sequence, such as our sequence of graphical primitives.
So we would like to define our relation using prolog DCG:

```prolog
music(MusicXML) -->
  % TODO
  ...
```

And run the grammar using the predicate `phrase/2`:

```prolog
?- phrase(music(MusicXML), GraphicalPrimitives).
```

However, the first real hurdle of this idea is the fact that in plain prolog, integer and floating arithmetic are not *pure*.
This means that:

```prolog
plusone(A, B) :-
  B is A + 1.
```

Can only be used in a single direction:

```prolog
?- A = 1, plusone(A, B).
A = 1,
B = 2.
```

While other directions cannot be used:

```prolog
?- B = 2, plusone(A, B).
ERROR: Arguments are not sufficiently instantiated
ERROR: In:
ERROR:   [11] 2 is _23054+1
ERROR:    [9] toplevel_call(user:user: ...) at /usr/lib64/swipl-8.4.3/boot/toplevel.pl:1158
ERROR:
ERROR: Note: some frames are missing due to last-call optimization.
ERROR: Re-run your program in debug mode (:- debug.) to get more detail.

?- plusone(A, B).
ERROR: Arguments are not sufficiently instantiated
ERROR: In:
ERROR:   [10] _1466 is _1472+1
ERROR:    [9] toplevel_call(user:user: ...) at /usr/lib64/swipl-8.4.3/boot/toplevel.pl:1158
```

The prolog ecosystem answer to this problem is constraint logic programming that comes in the form of prolog libraries such as clpZ, clpqr or [clpBNR](https://ridgeworks.github.io/clpBNR/CLP_BNR_Guide/CLP_BNR_Guide.html).

Using such libraries, we can actually define pure relationship between integers and floats:

```prolog
:- use_module(library(clpBNR)).

plusone(A, B) :-
  {B == A + 1}.
```

Now, we can use the two other directions:

```prolog
?- B = 2, plusone(A, B).
B = 2,
A = 1.

?- [A, B]::integer, plusone(A, B), A = 1.
A = 1,
B = 2.
```

This means that we can use constraints over integer and reals to specify relationships between coordinates of music graphical primitives.

Constraint logic programming covers arithmetics, but there are more impure constructs in prolog such as `atom_number/2`.
Such predicates can be made pure by using coroutining predicates such as `when/2`.

Using both CLPs and coroutining, we can now write a fully pure grammar that can be used to describe declaratively the modern music notation.

#### Difference with classical DCGs

Classical DCG were mainly designed to describe text, so a sequence of characters.
This means that the grammar scans the sequence from left to right and by design, sequence elements are *ordered*.

This means that to describe the word `the`, we can use the following DCG rule to describe that we search for three consecutive letters in a sequence of characters:

```prolog
word -->
  ['t'],
  ['h'],
  ['e'].
```

However, in a bidimensional grammar composed of graphical elements, there are no obvious ordering of the graphical elements.
For example, if we want to describe a simple quarter note, we would like to say:

```prolog
quarter -->
  [blackNotehead],
  [stem].
```

However, there are no guaranties that the note head will be placed next to its stem.
This is were the special `term//1` DCG predicate come into play.
This predicate is used throughout the grammar to consume graphical primitives.
In its current and quite naive form, this predicate just do an exhaustive search on the full list of graphical primitives:

```prolog
term(Term) -->
  [Term].
term(Term), [CurTerm] -->
  [CurTerm],
  term(Term).
```

With this, we can rewrite our previous `quarter//0` rules as follows:

```prolog
quarter -->
  term(blackNotehead),
  term(stem).
```

While this formulation is obviously bad because of the double exhaustive search, this predicate is *pure* and can be used in all directions:

```prolog
?- L = [blackNotehead, stem], phrase(quarter, L, R).
L = [blackNotehead, stem],
R = [] ;
false.

?- length(L, 2), phrase(quarter, L, []).
L = [blackNotehead, stem] ;
L = [stem, blackNotehead] ;
false.
```

Notice the double answers from the second queries, which shows that elements can be in any orders in the sequence.
This formulation is also resistant to noise:

```prolog
?- length(L, 3), phrase(quarter, L, R).
L = [blackNotehead, stem, _A],
R = [_A] ;
L = [blackNotehead, _A, stem],
R = [_A] ;
L = [stem, blackNotehead, _A],
R = [_A] ;
L = [_A, blackNotehead, stem],
R = [_A] ;
L = [stem, _A, blackNotehead],
R = [_A] ;
L = [_A, stem, blackNotehead],
R = [_A] ;
false.
```

One advantage of this formulation is that the implementation of the `term//1` can be improved later, by implementing something like an [R-Tree](https://en.wikipedia.org/wiki/R-tree) for efficient searching in a list of graphical primitives.
However, the implementation would need to be pure, or make the implementation of `term//1` changeable at runtime.

## Dependencies

The prolog implementation used is [swi-prolog](https://www.swi-prolog.org/).

Multiple external packs are used:

* [clpBNR](https://www.swi-prolog.org/pack/list?p=clpBNR)
* [delay](https://www.swi-prolog.org/pack/list?p=delay)

External program used:

* [Verovio](https://www.verovio.org/index.xhtml)
* python with [svgelements](https://github.com/meerk40t/svgelements)
* Make for running tests
