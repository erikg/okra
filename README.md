# Okra

## CFFI bindings to Ogre

<table align="center" width="100%">
  <tr>
    <td>&nbsp;</td>
    <td align="center">
      <img src="http://www.aerique.net/software/okra/okra.png">
    </td>
    <td>&nbsp;</td>
  </tr>
</table>

### Notes to self

* check out: http://www.antisphere.com/Wiki/tools:anttweakbar
* check out: http://github.com/Ramarren/cl-geometry/
* check out: http://github.com/nikodemus/sb-cga/
* linux to win cross-compiling: http://www.vtk.org/Wiki/CmakeMingw


### Introduction

Okra provides CFFI bindings to the [Ogre](http://www.ogre3d.org/) 3D
graphics engine.

There's a [video](http://www.youtube.com/watch?v=INeUifM2Bhg) of the
flock demo on YouTube.


### License

This project is released under the simplified
[BSD](http://www.opensource.net/licenses/bsd-license.php) license.


### Documentation

See doc/Okra.html or doc/Okra.pdf for documentation.


### To Do

In order of priority:

1. Clean up the code (specfically code duplication)
2. Document the code
3. Make a more extensive Lisp layer on top of the direct bindings
4. Slowly support more and more of the Ogre API
5. Tests (lower priority since the examples suffice for now)


### Platforms

The latest Git checkout has been tested on the following platforms:

* CLISP 2.44.1 / Debian Linux 2.6.30 / Ogre 1.6.2 src
* CLISP 2.48 / Debian Linux 2.6.30 / Ogre 1.6.2 src
* Clozure CL 1.4-dev-r12681M-trunk / Debian Linux 2.6.30 / Ogre 1.6.2 src
* SBCL 1.0.25.debian / Debian Linux 2.6.30 / Ogre 1.6.2 src
* SBCL 1.0.31.0.debian / Debian Linux 2.6.30 / Ogre 1.6.2 src
* Clozure CL ?.? / Windows XP 2 / Ogre 1.6.1 SDK
* Clozure CL ?.? / Windows Vista / Ogre 1.6.1 SDK
* SBCL 1.0.22 / Windows XP 2 / Ogre 1.6.1 SDK
* SBCL 1.0.29 / Windows Vista / Ogre 1.6.1 SDK

Older Clozure CL checkouts had some strange problem with bugging out
on a call to cl::sin both on Linux as well as on Windows.


### Downloads

* http://github.com/aerique/okra/downloads
* Windows executables for Okra: [okra-20090910.zip](http://www.xs4all.nl/~euqirea/downloads/okra-20090910.zip).  
  This includes executables for SBCL and CCL with Okra included.  
  *note: You'll need to have the [DirectX End-User Runtime](http://www.microsoft.com/downloads/Browse.aspx?displaylang=en&categoryid=2) installed!*
* A Windows executable for the Flock example: [flock.zip](http://www.xs4all.nl/~euqirea/downloads/flock.zip)  
  Please download this, check it for virusses, try to run both
  flock-ccl.exe & flock-sbcl.exe and [tell me](aerique@xs4all.nl) how
  it went (and tell me your version of Windows).  
  *note: You'll need to have the [DirectX End-User Runtime](http://www.microsoft.com/downloads/Browse.aspx?displaylang=en&categoryid=2) installed!*  
  *note 2: I've included d3dx9_30.dll but I'm not sure if this is
  enough for people who don't have Direct3D installed. Comments are
  welcome.*


### Version numbering

Okra follows the Ogre version numbering with its own version number
tagged on at the end. This way you can easily see which version of
Ogre these bindings are written for and what the latest release of
Okra itself is.


### Related projects

* [Black Tie](http://github.com/aerique/black-tie): A noise library in
  Common Lisp.
* [Buclet](http://github.com/aerique/buclet): Common Lisp bindings for
  the Bullet physics library.
* [clois-lane](http://github.com/aerique/clois-lane): Common Lisp
  bindings for the Object-Oriented Input System (OIS).

<table align="center" width="100%">
  <tr>
    <td align="center">
      <img src="http://www.aerique.net/software/okra/perlin-blob-a1.png">
    </td>
    <td align="center">
      <img src="http://www.aerique.net/software/okra/perlin-blob-a2.png">
    </td>
  </tr>
</table>
