## Type Conversions

The following tables give the correspondances between CFITSIO "types",
the BITPIX keyword and Julia types.

### CFITSIO Types
| CODE | CFITSIO   | Julia   |
|-----:|-----------|---------|
|      | int       | `Cint`  |
|      | long      | `Clong` |
|      | LONGLONG  | `Int64` |

### FITS BITPIX
| CODE | CFITSIO       | Julia     |
|-----:|---------------|-----------|
|    8 | BYTE\_IMG     | `UInt8`   |
|   16 | SHORT\_IMG    | `Int16`   |
|   32 | LONG\_IMG     | `Int32`   |
|   64 | LONGLONG\_IMG | `Int64`   |
|  -32 | FLOAT\_IMG    | `Float32` |
|  -64 | DOUBLE\_IMG   | `Float64` |

### CFITSIO Aliases
| CODE | CFITSIO        | Julia    | Comments                                                         |
|-----:|----------------|----------|:-----------------------------------------------------------------|
|   10 | SBYTE\_IMG     | `Int8`   | written as: BITPIX = 8, BSCALE = 1, BZERO = -128                 |
|   20 | USHORT\_IMG    | `UInt16` | written as: BITPIX = 16, BSCALE = 1, BZERO = 32768               |
|   40 | LONG\_IMG      | `UInt32` | written as: BITPIX = 32, BSCALE = 1, BZERO = 2147483648          |
|   80 | ULONGLONG\_IMG | `UInt64` | written as: BITPIX = 64, BSCALE = 1, BZERO = 9223372036854775808 |

### FITS Table Data Types
| CODE | CFITSIO     | Julia              |
|-----:|-------------|--------------------|
|    1 | TBIT        |                    |
|   11 | TBYTE       | `Cuchar`, `UInt8`  |
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
