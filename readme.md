
 Max Yourself a Scheme in 48 Hours
===================================

This is a port to BlitzMax of the famous Haskell tutorial, [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), by Jonathan Tang.

Despite the book's introductory statement that "users of a procedural or object-oriented language... will have to forget most of what you already know about programming", this projects attempts a translation to a procedural/OO language anyway.

The result is a bit weird. It's certainly completely useless for learning BlitzMax, because the code is something of a square peg, *piledriven into flat concrete where the round hole should be*, but perhaps it could be useful for more experienced BlitzMax users who want a taste of functional programming. In order to keep the translation literal, it was necessary to implement (inefficient) currying and pattern matching primitives for BlitzMax, which may help procedural programmers to understand those concepts.

Apart from the parser - which uses the declarative [TMeta framework](https://github.com/Leushenko/TMeta-Parser), mainly on the grounds that that's even more functional/declarative than what Haskell does (and because what Haskell actually does is too difficult to replicate) - this is an attempt at a *very literal* translation. Use of Haskell lists is maintained (using Max arrays); use of pattern matching is maintained; use of currying in maintained. The result is highly inefficient but the dogmatically 1:1 translation may prove helpful in understanding the Haskell version for some Maxers.

The project is split up into chapters, each one incrementally implementing a more complete version of the Scheme interpreter, much like the original. Each one should accompany a prose chapter... like the original. Rewriting these prose chapters for BlitzMax remains to be done.


This project is licensed as [CC-BY-SA](https://creativecommons.org/licenses/by-sa/3.0/) under the terms of the original book.  
Original BlitzMax code is copyright (c) Alex "Yasha" Gilding, 2014-.
TMeta is public domain.
