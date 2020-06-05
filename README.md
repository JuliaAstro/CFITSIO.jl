# CFITSIO.jl

[![Build Status](https://github.com/JuliaAstro/CFITSIO.jl/workflows/CI/badge.svg)](https://github.com/JuliaAstro/CFITSIO.jl/actions)
[![PkgEval](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/C/CFITSIO.svg)](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/report.html)
[![Coverage](https://codecov.io/gh/JuliaAstro/CFITSIO.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaAstro/CFITSIO.jl)

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaAstro.github.io/CFITSIO.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaAstro.github.io/CFITSIO.jl/dev)

## C-style interface to CFITSIO functions

- Function names closely mirror the C interface (e.g., `fits_open_file()`).
- Functions operate on `FITSFile`, a thin wrapper for `fitsfile` C struct
  (`FITSFile` has concept of "current HDU", as in CFITSIO).
- Note that the wrapper functions *do* check the return status from CFITSIO
  and throw an error with the appropriate message.

For more information and usage examples, please visit the [documentation](https://JuliaAstro.github.io/CFITSIO.jl/dev)
