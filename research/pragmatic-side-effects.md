---
title: Pragmatic Side Effects
authors: Jirka Maršík and Maxime Amblard
coauthors: Maxime Amblard
published: True
date: 2015-03-20
year: 2015
venue: Redrawing Pragmasemantic Borders (Redraw 2015)
abstract: 
---

Files
-----

  * [LingEff+Presup.eff](LingEff+Presup.eff)

    The state of my proof-of-concept prototype as of my presentation at
    Redraw 2015. It treats quantification and anaphora (as in de Groote's
    type-theoretic dynamic logic), presupposition, as in Lebedeva's
    dissertation, () alongside with a treatment of the binding problem.

    Feeding this file through [Eff](http://www.eff-lang.org/) produces the
    logical representations of the small suite of examples. One can also
    load the library in the Eff toplevel (REPL) and play around with their
    examples from the grammar.

    NB: The semantic representations are output in s-expression notation
    which isn't nice to read. The use of
    [some pretty-printer](https://github.com/jirkamarsik/tardis/blob/master/src/tardis/logic.clj)
    is recommended.


  * [LingEff+Presup-Move.eff](LingEff+Presup-Move.eff)

    Same as above, but this theory is simplified by not ostentatiously
    treating extraction as an effect and using the traditional treatment
    instead.

  * [Abstract](abstract.pdf)

    The abstract of my talk, as submitted to Redraw 2015.

  * [Slides](slides.pdf)

    Slides for my presentation given at Redraw 2015.
