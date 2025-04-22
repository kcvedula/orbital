# Project Structure

While the majority of the implementation details of our DSL is exposed to
the end user of the library and is documented in our doc, the structure
of the code is as follows:

## Core Types and Data Structures

- **[`types.rkt`](./types.rkt)**: Defines the fundamental data structures used throughout the project, including:

  - Elements and atomic properties
  - Atoms and bonds
  - Molecules and templates
  - Data structures for 3D representation
  - Types for external services (PubChem, networking)

- **[`periodic-table.rkt`](./periodic-table.rkt)**: Provides data about the periodic table elements, including:
  - Atomic properties (number, mass, radius)
  - Electronic configuration
  - Physical properties (melting point, boiling point)
  - Visual properties (CPK colors)

## DSL Frontend

- **[`templates-and-substituents-stx.rkt`](./templates-and-substituents-stx.rkt)**: Implements the syntax layer of the DSL, providing:

  - `define-sketch-template`: Define molecule templates
  - `sketch-template`: Create molecule templates inline
  - `extend-template`: Extend existing templates with new atoms and bonds
  - Comprehensive static checking for well-formed templates

- **[`templates-and-substituents.rkt`](./templates-and-substituents.rkt)**: Provides the runtime support for the DSL, including:
  - Template manipulation (adding substituents, combining templates)
  - Transformations (alpha-renaming, substitution)
  - Conversion between different representations

## Rendering and Visualization

- **[`render2d.rkt`](./render2d.rkt)**: 2D rendering of molecules

  - Converts PNG data to Racket pict objects for display

- **[`render3d.rkt`](./render3d.rkt)**: 3D rendering of molecules

  - Converts molecular data to 3D visualizations using pict3d
  - Handles atom size, bond representation, and coloring

- **[`explore.rkt`](./explore.rkt)**: Interactive 3D exploration
  - Provides a framework for navigating 3D molecular models
  - Configurable camera and movement controls

## External Integrations

- **[`babel.rkt`](./babel.rkt)**: Interface to OpenBabel for format conversion

  - Converts between different molecular formats (SMILES, CML, PNG)

- **[`cid.rkt`](./cid.rkt)**: Interface to PubChem

  - Lookup molecules by CID (PubChem Compound ID)
  - Convert between SMILES and CID
  - Retrieve 3D coordinates for molecules

- **[`https-get.rkt`](./https-get.rkt)**: Networking utility for external API calls

- **[`mol-to-cml.rkt`](./mol-to-cml.rkt)**: Conversion to Chemical Markup Language (CML)
  - Serializes molecule structures to XML format

## Compilation Pipeline

The DSL implements a multi-stage compilation process:

1. **Parsing and Syntax Checking**: The macro layer parses the DSL syntax and performs static checks

   - Well-formedness of atom and bond identifiers
   - Validity of element symbols, mass numbers, charges
   - Connectivity checks to ensure the molecule forms a connected graph

2. **Template Compilation**: Converts the parsed syntax to runtime template objects

   - Handles atom identity and properties
   - Maintains bond relationships
   - Preserves "bonding points" for future extension

3. **Template Manipulation**: The runtime layer supports operations on templates

   - Template combination
   - Substituent addition
   - Alpha-renaming to avoid identifier conflicts

4. **Rendering**: The compiled templates can be rendered in various formats
   - 2D visualization through OpenBabel and PNG conversion
   - 3D visualization through the pict3d library
   - Conversion to standard formats like CML
