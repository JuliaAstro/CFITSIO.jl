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

The following tables give the correspondances between CFITSIO "types",
the BITPIX keyword and Julia types.

## Type Conversions

### CFITSIO Types
| CODE | CFITSIO   | Julia   |
|-----:|-----------|---------|
|      | int       | `Cint`  |
|      | long      | `Clong` |
|      | LONGLONG  | `Int64` |

### FITS BITPIX
| CODE | CFITSIO       | Julia     |
|-----:|---------------|-----------|
|    8 | BYTE\_IMG     | `Uint8`   |
|   16 | SHORT\_IMG    | `Int16`   |
|   32 | LONG\_IMG     | `Int32`   |
|   64 | LONGLONG\_IMG | `Int64`   |
|  -32 | FLOAT\_IMG    | `Float32` |
|  -64 | DOUBLE\_IMG   | `Float64` |

### CFITSIO Aliases
| CODE | CFITSIO        | Julia    | Comments                                                         |
|-----:|----------------|----------|:-----------------------------------------------------------------|
|   10 | SBYTE\_IMG     | `Int8`   | written as: BITPIX = 8, BSCALE = 1, BZERO = -128                 |
|   20 | USHORT\_IMG    | `Uint16` | written as: BITPIX = 16, BSCALE = 1, BZERO = 32768               |
|   40 | LONG\_IMG      | `Uint32` | written as: BITPIX = 32, BSCALE = 1, BZERO = 2147483648          |
|   80 | ULONGLONG\_IMG | `UInt64` | written as: BITPIX = 64, BSCALE = 1, BZERO = 9223372036854775808 |

### FITS Table Data Types
| CODE | CFITSIO     | Julia              |
|-----:|-------------|--------------------|
|    1 | TBIT        |                    |
|   11 | TBYTE       | `Cuchar`, `Uint8`  |
|   12 | TSBYTE      | `Cchar`, `Int8`    |
|   14 | TLOGICAL    | `Bool  `           |
|   16 | TSTRING     | `String  `         |
|   20 | TUSHORT     | `Cushort`          |
|   21 | TSHORT      | `Cshort`           |
|   30 | TUINT       | `Cuint`            |
|   31 | TINT        | `Cint`             |
|   40 | TULONG      | `Culong`           |
|   41 | TLONG       | `Clong`            |
|   42 | TFLOAT      | `Cfloat`           |
|   80 | TULONGLONG  | `UInt64`           |
|   81 | TLONGLONG   | `Int64`            |
|   82 | TDOUBLE     | `Cdouble`          |
|   83 | TCOMPLEX    | `Complex{Cfloat}`  |
|  163 | TDBLCOMPLEX | `Complex{Cdouble}` |

```@docs
bitpix_from_type
type_from_bitpix
cfitsio_typecode
```

## File access

```@docs
fits_create_file
fits_create_diskfile
fits_clobber_file
fits_open_file
fits_open_diskfile
fits_open_table
fits_open_image
fits_open_data
fits_close_file
fits_delete_file
fits_file_name
fits_file_mode
```

## HDU Routines

The functions described in this section change the current
HDU and to find their number and type. The following is a short
example which shows how to use them:

```julia
num = fits_get_num_hdus(f)
println("Number of HDUs in the file: ", num)

for i = 1:num
    hdu_type = fits_movabs_hdu(f, i)
    println(i, ") hdu_type = ", hdu_type)
end
```

```@docs
fits_get_num_hdus
fits_movabs_hdu
fits_movrel_hdu
fits_movnam_hdu
fits_copy_file
fits_copy_hdu
fits_copy_data
fits_delete_hdu
```

## Header Keyword Routines

```@docs
fits_get_hdrspace
fits_read_keyword
fits_read_record
fits_read_keyn
fits_write_key
fits_write_record
fits_delete_record
fits_delete_key
fits_hdr2str
fits_copy_header
```

## Image HDU Routines

```@docs
fits_get_img_size
fits_create_img
fits_insert_img
fits_write_pix
fits_write_pixnull
fits_write_subset
fits_read_pix
fits_read_pixnull
fits_read_subset
fits_copy_image_section
fits_write_null_img
fits_resize_img
```

## Table Routines

There are two functions to create a new HDU table extension:
`fits_create_ascii_table` and `fits_create_binary_table`. In general,
one should pick the second as binary tables require less space on the
disk and are more efficient to read and write. (Moreover, a few
datatypes are not supported in ASCII tables). In order to create a
table, the programmer must specify the characteristics of each column
by passing an array of tuples. Here is an example:

```julia
f = fits_create_file("!new.fits")
coldefs = [("SPEED", "1D", "m/s"),
           ("MASS", "1E", "kg"),
           ("PARTICLE", "20A", "Name")]
fits_create_binary_tbl(f, 10, coldefs, "PARTICLE")
```

This example creates a table with room for 10 entries, each of them
describing the characteristics of a particle: its speed, its mass, and
its name (codified as a 20-character string). See the documentation of
`fits_create_ascii_tbl` for more details.

```@docs
fits_create_ascii_tbl
fits_create_binary_tbl
fits_get_coltype
fits_insert_rows
fits_delete_rows
fits_read_col
fits_write_col
```

## Miscellaneous

```@docs
libcfitsio_version
```
