#lang scribble/manual
@require[scribble-math
         scribble-math/dollar
         scribble/example
         (for-syntax racket/base syntax/parse)
         (for-label racket orbital
                    (only-in pict3d pict3d?)
                    (only-in pict pict bitmap pict?))]


@; Create an evaluator to use for examples blocks with the DSL required.
@(define eval (make-base-eval '(require racket orbital)))

@(define-syntax simple-ex
   (syntax-parser
     [(_ inner ...+)
      #'(examples #:eval eval #:label #f (eval:no-prompt inner ...))]))

@;; TODO: how do I make this use <kbd>?
@define[(kbd text) (tt text)]


@title[#:tag "orbital" #:style (with-html5 manual-doc-style)]{orbital}
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


@defparam[FPS frames Positive-Integer]
The target frames per second of the simulation. Defaults to 144.

@defparam[DELTA-LOOK rads Real]
The angle (in radians) the camera rotates per key press. Defaults to 0.5 degrees in radians.

@defparam[DELTA-MOVE dist Positive-Real]
The distance the camera moves per frame. Defaults to @${\frac{20}{FPS}}.

@defparam[FOV degs Positive-Real]
The field of view (in degrees) for the 3D camera. Defaults to 60.

@defthing[WS Type]{A structure representing the current world state.
 Contains the current scene, camera position, orientation, and pressed keys.}

@section[#:tag "core"]{Core & Data Types}

@subsection{Periodic Table}


@defstruct*[an-element-symbol ([v symbol?])]{Represents the atomic symbol of an element

 @simple-ex[(an-element-symbol 'H)]
 @simple-ex[(an-element-symbol 'Fe)]
}

@defstruct*[element
            ([atomic-number (between/c 1 118)]
             [symbol an-element-symbol?]
             [name symbol?]
             [atomic-mass number?]
             [cpk-color (or/c (is-a?/c color%) #f)]
             [electron-configuration list?]
             [electronegativity (or/c number? #f)]
             [atomic-radius (or/c number? #f)]
             [ionization-energy (or/c number? #f)]
             [electron-affinity (or/c number? #f)]
             [oxidation-states (listof number?)]
             [standard-state symbol?]
             [melting-point (or/c number? #f)]
             [boiling-point (or/c number? #f)]
             [density (or/c number? #f)]
             [group-block string?]
             [year-discovered (or/c number? #f)])]{
 Represents a chemical element and its associated properties. The properties will generally be fetched from pubchem.
 Some fields are optional and may be @racket[#f] if unknown.

 @simple-ex[(element 1 (an-element-symbol 'H) 'Hydrogen 1.008
                     (make-object color% 255 255 255) '((1 s 1))
                     2.2 120 13.598 0.754 '(1 -1) 'gas
                     13.81 20.28 8.988e-5 "Nonmetal" 1766)]
}

@defthing[periodic-table (listof element?)]{
 A cleaned list of chemical elements, parsed from PubChem's periodic table dataset.

 Each entry is an @racket[element] struct, constructed from raw PubChem data and normalized to ensure consistent types and structure (e.g., parsed oxidation states, electron configurations, and colors).

 This table is loaded from a local cache on disk, or fetched and saved on first use.
}


@subsection{Molecule}

@defstruct*[atom
            ([element an-element-symbol?]
             [mass-number (or/c #f positive-integer?)]
             [chirality (or/c #f 'R 'S)]
             [formal-charge (or/c #f integer?)])]{
 Represents a single atom in a molecule, including optional structural and chemical details.

 All fields beyond the atomic symbol are optional and may be @racket[#f] when unspecified:
 @itemlist[
 @item{@tt{mass-number} — the isotope mass number (e.g., 12, 14)}
 @item{@tt{chirality} — whether the atom is chiral and its configuration (R or S)}
 @item{@tt{formal-charge} — the formal charge on the atom}
 ]

 @simple-ex[(atom (an-element-symbol 'C) 12 'R 0)]
 @simple-ex[(atom (an-element-symbol 'O) #f #f -1)]

}

@defstruct*[bond
            ([id1 positive-integer?]
             [id2 positive-integer?]
             [order (or/c 1 2 3)]
             [stereo (or/c #f 'E 'Z)])]{
 Represents a bond between two atoms in a molecule, typically a covalent.

 @itemlist[
 @item{@tt{id1} and @tt{id2} are atom IDs in the molecule}
 @item{@tt{order} is the bond order — must be 1 (single), 2 (double), or 3 (triple)}
 @item{@tt{stereo} indicates geometric isomerism (cis/trans), where @racket['E] means "entgegen" (opposite) and @racket['Z] means "zusammen" (together); @racket[#f] if not applicable}
 ]

}

@defstruct*[mol
            ([atoms (hash/c positive-integer? atom?)]
             [bonds (listof bond?)])]{
 Represents a molecular structure, composed of atoms and the bonds between them.

 @itemlist[
 @item{@tt{atoms} is a hash mapping unique positive integer IDs to @racket[atom] structs. These IDs are used to reference atoms in bonds.}
 @item{@tt{bonds} is a list of @racket[bond] structs describing connections between the atoms.}
 ]

 This struct serves as the base representation for molecular fragments and complete molecules. It is extended by other types such as @racket[template] and @racket[substituent].

 @simple-ex[
 (define h (atom (an-element-symbol 'H) #f #f 0))
 (define o (atom (an-element-symbol 'O) #f #f 0))
 (define water
   (mol (hash 1 h
              2 o
              3 h)
        (list (bond 1 2 1 #f)
              (bond 2 3 1 #f))))
 (mol-bonds water)
 ]
}

@defstruct*[(template mol)
            ([next-id positive-integer?]
             [bonding-atoms-ids (listof positive-integer?)])]{
 Represents a molecular fragment under construction, extending @racket[mol].

 @itemlist[
 @item{@tt{next-id} is the next available atom ID that can be assigned. It must be greater than any existing atom ID in the @racket[atoms] hash.}
 @item{@tt{bonding-atoms-ids} is a list of atom IDs that represent "open bonding sites" — atoms in the fragment that are available to connect to other fragments or substituents.}
 ]

 This struct is typically used when composing molecules from smaller pieces.
}
@; TODO: example

@defstruct*[(substituent template) ()]{
 Represents a reusable molecular fragment that can be attached to another molecule. Inherits from @racket[template].

 A @racket[substituent] typically contains one or more open bonding sites, defined by @racket[bonding-atoms-ids], that determine how it can be connected to a larger structure.
}

@defstruct*[info-substituent-addition
            ([substituent substituent?]
             [bond bond?]
             [num-times (between/c 1 8)])]{
 Describes how a @racket[substituent] should be added to a molecular template.

 @itemlist[
 @item{@tt{substituent} is the fragment to be inserted.}
 @item{@tt{bond} specifies where the substituent will attach — the atoms in the bond correspond to IDs in the host template and the substituent.}
 @item{@tt{num-times} is how many times the substituent should be inserted at that location (e.g., for symmetry or repetition).}
 ]
}

@subsection{Babel Format}

@defstruct*[smiles ([v string?])]{
 Represents a SMILES (Simplified Molecular Input Line Entry System) string encoding a molecule.
}

@defstruct*[cml ([v string?])]{
 Represents a CML (Chemical Markup Language) document as a string.
}

@defstruct*[png ([v bytes?])]{
 Represents an image of a molecule encoded as PNG bytes.
}


@subsection{PubChem}

@defstruct*[cid ([v positive-integer?])]{
 Represents a PubChem Compound ID.
}

@defstruct*[conformer ([v string?])]{
 Represents a PubChem conformer identifier string.
}

@subsection{3D & Rendering}

@defstruct*[atom3d
            ([id positive-integer?]
             [element (and/c natural? (between/c 1 118))]
             [x real?]
             [y real?]
             [z real?])]{
 Represents an atom in 3D space. The @tt{id} is a unique identifier, and @tt{element} is the atomic number. Coordinates are given in Cartesian space.
}

@defstruct*[bond3d
            ([a1 positive-integer?]
             [a2 positive-integer?]
             [order (or/c 1 2 3)])]{
 Represents a bond between two atoms in 3D space, by their IDs. The @tt{order} field specifies whether the bond is single, double, or triple.
}

@defstruct*[mol3d
            ([atoms3d (listof atom3d?)]
             [bonds3d (listof bond3d?)])]{
 Represents a molecule with 3D coordinates for atoms and their corresponding bonds. Used for visualization
}

@defproc[(png->pict [p png?]) pict?]{
 Converts a @racket[png] object—containing raw PNG bytes—into a @racket[pict] that can be used for rendering.
}

@defproc[(mol3d->pict3d [m mol3d?]) pict3d?]{
 Renders a 3D molecular structure into a @racket[pict3d] scene.

 Atoms are displayed as spheres using their van der Waals radius and CPK color (if available), and bonds are shown as cylinders. Bond multiplicity (single, double, triple) is visually represented by parallel cylinders.

 Useful for visualizing geometry or exploring molecules interactively via @racket[explore].
}

@subsection{Networking}

@defstruct*[https-get-resp
            ([status any/c]
             [headers any/c]
             [raw any/c])]{
 Represents the result of an HTTPS GET request. This is our internal representation and includes the status code, headers, and raw response body.
}

@section[#:tag "conversion"]{Conversions}


@subsection{Babel Conversion}

@defparam[cmd-prefix prefix string?]{
 A parameter that holds a prefix string to prepend to shell commands.
 This is used internally when invoking external tools like Open Babel.
 By default an empty string.

 @simple-ex[(cmd-prefix "nix-shell --run")]
 @simple-ex[(cmd-prefix "wsl")]
}

@defproc[(babel [input (or/c smiles? cml?)] [output-format (or/c (== smiles) (== cml) (== png))]) (or/c smiles? cml? png?)]{
 Converts molecular data between supported formats (e.g., SMILES, CML, PNG) using Open Babel.

 @itemlist[
 @item{@racket[input] is the input molecule data, represented as a @racket[smiles] or @racket[cml] struct.}
 @item{@racket[output-format] is the desired output format, specified as one of the constructors @racket[smiles], @racket[cml], or @racket[png].}
 ]

 Returns a new value of the corresponding output type.

 @; todo: how to eval this when it uses cmd line?
 @racketblock[(babel (smiles "O") cml)]
}

@subsection{PubChem Conversion}

@defproc[(smiles->cid [s smiles?]) cid?]{
 Looks up the PubChem Compound ID (CID) for a given @racket[smiles] representation of a molecule.

 This uses PubChem's PUG REST interface to resolve the structure.

 @simple-ex[(smiles->cid (smiles "CCO"))]
}

@defproc[(cid->smiles [c cid?]) smiles?]{
 Fetches the canonical SMILES string for the given PubChem Compound ID.

 This is useful for retrieving standardized structure representations.

 @simple-ex[(cid->smiles (cid 702))]
}

@defproc[(cid->mol3d [c cid?]) mol3d?]{
 Downloads a 3D molecular structure for the given PubChem Compound ID and converts it to a @racket[mol3d] value.

 This structure can be rendered using @racket[mol3d->pict3d] or explored with @racket[explore].

 @simple-ex[(cid->mol3d (cid 702))]
}

@subsection{CML}

@defproc[(mol->cml [m mol?]) cml?]{
 Converts a @racket[mol] structure into a @racket[cml] value (Chemical Markup Language).

 The resulting @racket[cml] value is an XML-based string representation of the molecule, containing atoms and bonds as conforming CML markup. This is useful for exporting or visualizing molecular data in interoperable formats.

 Atom attributes like @tt{mass-number} and @tt{formal-charge} are included if present. Bond stereochemistry is also preserved.

 @simple-ex[(mol->cml water) ] }
