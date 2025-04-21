#lang scribble/manual
@require[@for-label[orbital racket/base]]

@;; TODO: how do I make this use <kbd>?
@define[(kbd text) (tt text)]


@title{orbital}
@author{kcvedula, ironmoon}

@defmodule[orbital]

Orbital is a chemistry DSL used for composing molecules, 

@table-of-contents[]


@section[#:tag "explore"]{explore}

@defproc[(explore [pict Pict3d]) WS]
Creates an interactive 3D window using @link["https://docs.racket-lang.org/pict3d/index.html"]{pict3d}.
Some aspects of the 


The following keys are used for movement:
@itemlist[
 @item{@kbd{w}, @kbd{a}, @kbd{s}, @kbd{d} to move forward, left, back, and right in space relative to the camera}
 @item{@kbd{space}, @kbd{shift} to move up and down respectivly}]

Likewise, the following keys are used to control the camera:
@itemlist[
 @item{@kbd{q}, @kbd{e} roll the camera left and right respectively}
 @item{The arrow keys @kbd{up}, @kbd{down}, @kbd{left}, @kbd{right} rotate the camera in their corresponding directions}]

