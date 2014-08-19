---
title: Algebraic Effects and Handlers in Natural Language Interpretation
authors: Jiří Maršík and Maxime Amblard
coauthors: Maxime Amblard
published: True
date: 2014-07-17
year: 2014
venue: NLCS '14 - Second Workshop on Natural Language and Computer Science
abstract: 
  <p>Phenomena on the syntax-semantics interface of natural languages have been
  observed to have links with programming language semantics, namely
  computational effects and evaluation order. We explore this connection to be
  able to profit from recent development in the study of effects. We propose
  adopting algebraic effects and handlers as tools for facilitating a uniform
  and integrated treatment of different non-compositional phenomena on the
  syntax-semantics interface.</p>
  <p>In this paper, we give an exposition of the framework of algebraic effects
  and handlers with an eye towards its applicability in computational
  semantics. We then present some exemplary analyses in the framework: we
  study the interplay of anaphora and quantification by translating the
  continuation-based dynamic logic of de Groote into a more DRT-like theory
  and we propose a treatment of overt wh-movement which avoids higher-order
  types in the syntax.</p>
---

Files
-----

  * [LingEff.eff](LingEff.eff)

    The grammar from the paper (dynamic logic + extraction as an effect)
    implemented as a library an [Eff](http://math.andrej.com/eff/) with a tiny
    suite of examples.

  * [Preprint](paper.pdf)

    The preprint of the paper, as was submitted to NLCS'14.

  * [Slides](slides.pdf)

    Slides for my presentation given at NLCS'14.

  * [Poster](poster.pdf)

    Poster for the LCT 2014 meeting.

  * [Write-up](writeup.pdf) (unfinished)

    A prequel to the paper that motivates an effects-first view of de Groote's
    continuation-based dynamic logic. This includes a refactoring of
    continuation-based dynamic logic in terms of monads which can be seen as
    a first step towards the transition to effects and handlers.
