```@meta
CurrentModule = CFITSIO
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
fits_write_chksum
fits_update_chksum
fits_verify_chksum
```
