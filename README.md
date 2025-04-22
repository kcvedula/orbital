# orbital

Orbital is a domain-specific language (DSL) for constructing, composing, and
rendering 3D molecular structures in Racket. It provides an expressive syntax
for creating chemical structures and manipulating them programmatically, with
rich support for format conversion, visual exploration, and PubChem integration.

It’s designed for programmers and educators who want to compose molecules with
code—supporting format conversion, visualization, and fragment-based
composition.

## Example

Define a water molecule using high-level sketch syntax:

```racket
(define-sketch-template water
  #:atoms O-1 H-2 H-3
  #:bonds (O-1 H-2)
          (O-1 H-3))
```

Render the structure as a 2D image or explore it in 3D:

```racket
(png->pict (babel (mol->cml water) png))

(explore (mol3d->pict3d
          (cid->mol3d
           (smiles->cid
            (babel (mol->cml water) smiles)))))
```

Orbital enables expressive, programmatic molecule design with seamless rendering
support.

See the [examples.rkt](examples) file for more in-depth examples, as well as
the Scribble documentation.

## Installation

To install the package from source:

```sh
git clone https://github.com/kcvedula/orbital.git
cd orbital/orbital
raco pkg install
```

Then import the library:

```racket
(require orbital)
```

## Documentation

You can view the full documentation by opening `scribblings/orbital.scrbl` in
DrRacket and clicking "Scribble HTML", or by running:

```sh
scribble scribblings/orbital.scrbl
```

> [!WARNING] > `raco docs` does not currently work due to a conflict with `racket/gui`.
> Use Scribble directly or DrRacket.

## Wayland Note

The way `Pict3D` interacts with `racket-gui` uses assumptions based on X11
(e.g., that an OpenGL context will still exist after a window is hidden).
However, Wayland handles OpenGL contexts more strictly, and hiding a window can
invalidate the context, causing a crash.

To circumvent this issue, you can force the application to run under XWayland by
setting the environment variable `GDK_BACKEND=x11`. This reverts GTK to use X11,
which better matches the assumptions `Pict3D` makes.

We’ve included a helper script, `setup-xwayland.sh`, in the root directory for
convenience:

```sh
source ./setup-xwayland.sh
```

## Resources

- [examples.rkt](examples) -- Sample usage of the library
- [PubChem](pubchem) -- Source of element and molecule data
- [Open Babel](openbabel) -- Backend for chemical format conversion

[examples]: ./examples.rkt
[pubchem]: https://pubchem.ncbi.nlm.nih.gov/
[openbabel]: http://openbabel.org/
