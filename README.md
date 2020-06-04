# CFITSIO.jl

[![Build Status](https://github.com/JuliaAstro/CFITSIO.jl/workflows/CI/badge.svg)](https://github.com/JuliaAstro/CFITSIO.jl/actions)
[![Coverage](https://codecov.io/gh/JuliaAstro/CFITSIO.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaAstro/CFITSIO.jl)

## C-style interface to CFITSIO functions

- Function names closely mirror the C interface (e.g., `fits_open_file()`).
- Functions operate on `FITSFile`, a thin wrapper for `fitsfile` C struct
  (`FITSFile` has concept of "current HDU", as in CFITSIO).
- Note that the wrapper functions *do* check the return status from CFITSIO
  and throw an error with the appropriate message.

The following tables give the correspondances between CFITSIO "types",
the BITPIX keyword and Julia types.

### CFITSIO Types
|                  CODE 	| CFITSIO      	| Julia            	| Comments                                                	|
|----------------------:	|--------------	|------------------	|---------------------------------------------------------	|
|                       	| int          	| Cint             	|                                                         	|
|                       	| long         	| Clong            	|                                                         	|
|                       	| LONGLONG     	| Int64            	| 64-bit integer                                          	|

### FITS BITPIX
|                  CODE 	| CFITSIO      	| Julia            	| Comments                                                	|
|----------------------:	|--------------	|------------------	|---------------------------------------------------------	|
|                     8 	| BYTE_IMG     	| Uint8            	|                                                         	|
|                    16 	| SHORT_IMG    	| Int16            	|                                                         	|
|                    32 	| LONG_IMG     	| Int32            	|                                                         	|
|                    64 	| LONGLONG_IMG 	| Int64            	|                                                         	|
|                   -32 	| FLOAT_IMG    	| Float32          	|                                                         	|
|                   -64 	| DOUBLE_IMG   	| Float64          	|                                                         	|

### CFITSIO Aliases
|                  CODE 	| CFITSIO      	| Julia            	| Comments                                                	|
|----------------------:	|--------------	|------------------	|---------------------------------------------------------	|
|                    10 	| SBYTE_IMG    	| Int8             	| written as: BITPIX = 8, BSCALE = 1, BZERO = -128        	|
|                    20 	| USHORT_IMG   	| Uint16           	| written as: BITPIX = 16, BSCALE = 1, BZERO = 32768      	|
|                    40 	| LONG_IMG     	| Uint32           	| written as: BITPIX = 32, BSCALE = 1, BZERO = 2147483648 	|

### FITS Table Data Types
|                  CODE 	| CFITSIO      	| Julia            	| Comments                                                	|
|----------------------:	|--------------	|------------------	|---------------------------------------------------------	|
|                     1 	| TBIT         	|                  	|                                                         	|
|                    11 	| TBYTE        	| Cuchar = Unit8   	|                                                         	|
|                    12 	| TSBYTE       	| Cchar = Int8     	|                                                         	|
|                    14 	| TLOGICAL     	| Bool             	|                                                         	|
|                    16 	| TSTRING      	| String           	|                                                         	|
|                    20 	| TUSHORT      	| Cushort          	|                                                         	|
|                    21 	| TSHORT       	| Cshort           	|                                                         	|
|                    30 	| TUINT        	| Cuint            	|                                                         	|
|                    31 	| TINT         	| Cint             	|                                                         	|
|                    40 	| TULONG       	| Culong           	|                                                         	|
|                    41 	| TLONG        	| Clong            	|                                                         	|
|                    42 	| TFLOAT       	| Cfloat           	|                                                         	|
|                    81 	| TLONGLONG    	| Int64            	|                                                         	|
|                    82 	| TDOUBLE      	| Cdouble          	|                                                         	|
|                    83 	| TCOMPLEX     	| Complex{Cfloat}  	|                                                         	|
|                   163 	| TDBLCOMPLEX  	| Complex{Cdouble} 	|                                                         	|