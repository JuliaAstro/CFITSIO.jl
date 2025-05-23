```@meta
CurrentModule = CFITSIO
```

# CFITSIO.jl

[![GitHub](https://img.shields.io/badge/Code-GitHub-black.svg)](https://github.com/juliaastro/CFITSIO.jl)
[![Build Status](https://github.com/JuliaAstro/CFITSIO.jl/workflows/CI/badge.svg)](https://github.com/JuliaAstro/CFITSIO.jl/actions)
[![PkgEval](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/C/CFITSIO.svg)](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/report.html)
[![Coverage](https://codecov.io/gh/JuliaAstro/CFITSIO.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaAstro/CFITSIO.jl)

This module provides an interface familiar to users of the
[CFITSIO](http://heasarc.gsfc.nasa.gov/fitsio/) C library. It can be used with

```julia
using CFITSIO
```

The functions exported by this module operate on `FITSFile` objects,
which is a thin wrapper around a pointer to a CFITSIO `fitsfile`.
For the most part, the functions are thin wrappers around the CFITSIO
routines of the same names. Typically, they:

* Convert from Julia types to C types as necessary.
* Check the returned status value and raise an appropriate exception if
  non-zero.

The functionality is described in the various sections.

```@contents
Pages = Main.pages_files
```

## Quick start

```@setup quickstart
using CFITSIO
```

```@repl quickstart
fname = tempname() * ".fits";
f = fits_create_file(fname);
A = ones(2,2)
fits_create_img(f, A)
fits_get_hdu_type(f)
fits_write_pix(f, A)
B = similar(A);
fits_read_pix(f, B);
B
fits_read_key_str(f, "NAXIS")
fits_create_binary_tbl(f, 0, [("COUNT", "J", "counts"), ("ENERGY", "D", "energy")], "Spectrum")
fits_write_col(f, 1, 1, 1, [2, 10, 5])
fits_write_col(f, 2, 1, 1, [10.0, 15.0, 20.0])
counts, energy = zeros(Int,3), zeros(Float64,3)
fits_read_col(f, 1, 1, 1, counts)
fits_read_col(f, 1, 1, 1, energy)
counts, energy
fits_close_file(f)
```
