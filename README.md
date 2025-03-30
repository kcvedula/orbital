# orbital

3D Molecular Rendering in Racket

## To Explore

Clone this repository and in the same directory open a file and require.

Open test.rkt and use the defined

## Colors

https://jmol.sourceforge.net/jscolors/

## Atom Sizes

https://pubchem.ncbi.nlm.nih.gov/ptable/atomic-radius/

## Setup

### Wayland

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
