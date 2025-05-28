```@meta
CurrentModule = CFITSIO
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
fits_create_tbl
fits_delete_col
fits_delete_rowlist
fits_get_colnum
fits_get_coltype
fits_get_eqcoltype
fits_get_num_cols
fits_get_num_rows
fits_get_rowsize
fits_insert_col
fits_insert_cols
fits_insert_rows
fits_delete_rows
fits_read_col
fits_read_atblhdr
fits_read_btblhdr
fits_read_descript
fits_read_keys_lng
fits_read_tdim
fits_write_col
fits_write_tdim
```
