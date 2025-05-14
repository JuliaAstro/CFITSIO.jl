module CFITSIO
using CFITSIO_jll

export FITSFile,
    FITSMemoryHandle,
    fits_assert_open,
    fits_clobber_file,
    fits_close_file,
    fits_copy_image_section,
    fits_create_ascii_tbl,
    fits_create_binary_tbl,
    fits_create_diskfile,
    fits_create_file,
    fits_create_img,
    fits_delete_file,
    fits_delete_key,
    fits_delete_record,
    fits_delete_rows,
    fits_file_mode,
    fits_file_name,
    fits_get_hdrspace,
    fits_get_hdu_num,
    fits_get_hdu_type,
    fits_delete_hdu,
    fits_get_img_dim,
    fits_get_img_equivtype,
    fits_get_img_size,
    fits_get_img_type,
    fits_get_num_cols,
    fits_get_num_hdus,
    fits_get_num_rows,
    fits_get_rowsize,
    fits_get_colnum,
    fits_get_coltype,
    fits_get_eqcoltype,
    fits_get_version,
    fits_read_tdim,
    fits_hdr2str,
    fits_insert_img,
    fits_insert_rows,
    fits_movabs_hdu,
    fits_movrel_hdu,
    fits_movnam_hdu,
    fits_open_data,
    fits_open_diskfile,
    fits_open_file,
    fits_open_image,
    fits_open_table,
    fits_open_memfile,
    fits_read_col,
    fits_read_descript,
    fits_read_keyn,
    fits_read_key_str,
    fits_read_key_lng,
    fits_read_keys_lng,
    fits_read_keyword,
    fits_read_pix,
    fits_read_pixnull,
    fits_read_record,
    fits_read_subset,
    fits_read_atblhdr,
    fits_read_btblhdr,
    fits_resize_img,
    fits_update_key,
    fits_write_col,
    fits_write_date,
    fits_write_comment,
    fits_write_history,
    fits_write_key,
    fits_write_pix,
    fits_write_pixnull,
    fits_write_subset,
    fits_write_null_img,
    fits_write_record,
    fits_write_tdim,
    libcfitsio_version,
    cfitsio_typecode,
    bitpix_from_type,
    type_from_bitpix


@enum FileMode R = 0 RW = 1

"""
    cfitsio_typecode(::Type) -> Cint

Return the CFITSIO type code for the given Julia type
"""
function cfitsio_typecode end

"""
    bitpix_from_type(::Type) -> Cint

Return the FITS BITPIX code for the given Julia type
"""
function bitpix_from_type end

"""
    type_from_bitpix(::Integer) -> Type

Return the Julia type from the FITS BITPIX code
"""
function type_from_bitpix end

for (T, code) in (
    (UInt8, 11),
    (Int8, 12),
    (Bool, 14),
    (String, 16),
    (Cushort, 20),
    (Cshort, 21),
    (Cuint, 30),
    (Cint, 31),
    (UInt64, 80),
    (Int64, 81),
    (Float32, 42),
    (Float64, 82),
    (ComplexF32, 83),
    (ComplexF64, 163),
)
    @eval cfitsio_typecode(::Type{$T}) = Cint($code)
end

for (T, code) in ((UInt8,     8), # BYTE_IMG
                  (Int16,    16), # SHORT_IMG
                  (Int32,    32), # LONG_IMG
                  (Int64,    64), # LONGLONG_IMG
                  (Float32, -32), # FLOAT_IMG
                  (Float64, -64), # DOUBLE_IMG
                  (Int8,     10), # SBYTE_IMG
                  (UInt16,   20), # USHORT_IMG
                  (UInt32,   40), # ULONG_IMG
                  (UInt64,   80)) # ULONGLONG_IMG
    local value = Cint(code)
    @eval begin
        bitpix_from_type(::Type{$T}) = $value
        type_from_bitpix(::Val{$value}) = $T
    end
end
type_from_bitpix(code::Integer) = type_from_bitpix(Val(Cint(code)))

# Above, we don't define a method for Clong because it is either Cint (Int32)
# or Int64 depending on the platform, and those methods are already defined.
# Culong is either UInt64 or Cuint depending on platform.

const FLEN_FILENAME = 1025 # max length of a filename  */
const FLEN_KEYWORD =   75  # max length of a keyword (HIERARCH convention) */
const FLEN_CARD =      81  # length of a FITS header card */
const FLEN_VALUE =     71  # max length of a keyword value string */
const FLEN_COMMENT =   73  # max length of a keyword comment string */
const FLEN_ERRMSG =    81  # max length of a FITSIO error message */
const FLEN_STATUS =    31  # max length of a FITSIO status text string */

# The following block are all functions that have separate variants for Clong
# and 64-bit integers in cfitsio. Rather than providing both of these, we
# provide only one according to the native integer type on the platform.
if promote_type(Int, Clong) == Clong
    const Long_or_LongLong = Clong
    const ffgtdm = "ffgtdm"
    const ffgnrw = "ffgnrw"
    const ffptdm = "ffptdm"
    const ffgtcl = "ffgtcl"
    const ffeqty = "ffeqty"
    const ffgdes = "ffgdes"
    const ffgisz = "ffgisz"
    const ffghbn = "ffghbn"
    const ffghtb = "ffghtb"
    const ffghpr = "ffghpr"
else
    const Long_or_LongLong = Int64
    const ffgtdm = "ffgtdmll"
    const ffgnrw = "ffgnrwll"
    const ffptdm = "ffptdmll"
    const ffgtcl = "ffgtclll"
    const ffeqty = "ffeqtyll"
    const ffgdes = "ffgdesll"
    const ffgisz = "ffgiszll"
    const ffghbn = "ffghbnll"
    const ffghtb = "ffghtbll"
    const ffghpr = "ffghprll"
end

# -----------------------------------------------------------------------------
# FITSFile type

mutable struct FITSFile
    ptr::Ptr{Cvoid}

    FITSFile(ptr::Ptr{Cvoid}) = finalizer(fits_close_file, new(ptr))
end

# FITS wants to be able to update the ptr, so keep them
# in a mutable struct
mutable struct FITSMemoryHandle
    ptr::Ptr{Cvoid}
    size::Csize_t
end
FITSMemoryHandle() = FITSMemoryHandle(C_NULL, 0)

# -----------------------------------------------------------------------------
# error messaging

function fits_assert_open(f::FITSFile)
    if f.ptr == C_NULL
        error("attempt to access a FITS file that has been closed previously")
    end
end

function fits_assert_nonempty(f::FITSFile)
    if fits_get_num_hdus(f) == 0
        error("No HDU found in FITS file")
    end
end

struct CFITSIOError{T} <: Exception
    filename :: T
    errcode :: Cint
    errmsgshort :: String
    errmsgfull :: String
end
function Base.showerror(io::IO, c::CFITSIOError)

    print(io, "CFITSIO has encountered an error")
    if c.filename !== nothing
        print(io, " while processing ", c.filename)
    end
    println(io, ". Error code ", c.errcode, ": ", c.errmsgshort)
    if !isempty(c.errmsgfull)
        println(io, "Detailed error message follows: ")
        print(io, c.errmsgfull)
    end
end

tostring(v) = GC.@preserve v unsafe_string(pointer(v))

function fits_get_errstatus(status::Cint)
    msg = Vector{UInt8}(undef, FLEN_STATUS)
    ccall((:ffgerr, libcfitsio), Cvoid, (Cint, Ptr{UInt8}), status, msg)
    tostring(msg)
end

function fits_read_errmsg()
    msg = Vector{UInt8}(undef, FLEN_ERRMSG)
    msgstr = ""
    ccall((:ffgmsg, libcfitsio), Cvoid, (Ptr{UInt8},), msg)
    msgstr = tostring(msg)
    errstr = msgstr
    while msgstr != ""
        ccall((:ffgmsg, libcfitsio), Cvoid, (Ptr{UInt8},), msg)
        msgstr = tostring(msg)
        errstr *= '\n' * msgstr
    end
    return errstr
end

function fits_assert_ok(status::Cint, filename = nothing)
    if status != 0
        err = CFITSIOError(filename,
                status,
                fits_get_errstatus(status),
                fits_read_errmsg(),
            )
        throw(err)
    end
end

fits_assert_isascii(str::String) =
    !isascii(str) && error("FITS file format accepts ASCII strings only")

fits_get_version() = ccall((:ffvers, libcfitsio), Cfloat, (Ref{Cfloat},), 0.0)

# -----------------------------------------------------------------------------
# Utility function

zerost(::Type{T}, n) where {T} = ntuple(_ -> zero(T), n)
onest(::Type{T}, n) where {T} = ntuple(_ -> one(T), n)

# -----------------------------------------------------------------------------
# file access & info functions

"""
    fits_create_file(filename::AbstractString)

Create and open a new empty output `FITSFile`. This methods uses the
[extended file name syntax](https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/node83.html)
to create the file.

See also [`fits_create_diskfile`](@ref) which does not use the extended filename parser.
"""
function fits_create_file(filename::AbstractString)
    ptr = Ref{Ptr{Cvoid}}()
    status = Ref{Cint}(0)
    ccall(
        (:ffinit, libcfitsio),
        Cint,
        (Ref{Ptr{Cvoid}}, Ptr{UInt8}, Ref{Cint}),
        ptr,
        filename,
        status,
    )
    fits_assert_ok(status[], filename)
    FITSFile(ptr[])
end

"""
    fits_create_diskfile(filename::AbstractString)

Create and open a new empty output `FITSFile`. Unlike [`fits_create_file`](@ref), this function does
not use an extended filename parser and treats the string as is as the filename.
"""
function fits_create_diskfile(filename::AbstractString)
    ptr = Ref{Ptr{Cvoid}}()
    status = Ref{Cint}(0)
    ccall(
        (:ffdkinit, libcfitsio),
        Cint,
        (Ref{Ptr{Cvoid}}, Ptr{UInt8}, Ref{Cint}),
        ptr,
        filename,
        status,
    )
    fits_assert_ok(status[], filename)
    FITSFile(ptr[])
end

"""
    fits_clobber_file(filename::AbstractString)

Like [`fits_create_file`](@ref), but overwrites `filename` if it exists.
"""
fits_clobber_file(filename::AbstractString) = fits_create_file("!" * filename)

"""
    fits_open_data(filename::String, [mode = 0])

Open an existing data file (like [`fits_open_file`](@ref)) and move to the first HDU
containing either an image or a table.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.RW`)
"""
fits_open_data

"""
    fits_open_file(filename::String, [mode = 0])

Open an existing data file.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.RW`)

This function uses the extended filename syntax to open the file. See also [`fits_open_diskfile`](@ref)
that does not use the extended filename parser and uses `filename` as is as the name of the file.
"""
fits_open_file

"""
    fits_open_diskfile(filename::String, [mode = 0])

Open an existing data file.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.RW`)

This function does not use the extended filename parser, and uses `filename` as is as the name
of the file that is to be opened. See also [`fits_open_file`](@ref) which uses the extended filename syntax.
"""
fits_open_diskfile

"""
    fits_open_image(filename::String, [mode = 0])

Open an existing data file (like [`fits_open_file`](@ref)) and move to the first
HDU containing an image.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.RW`)
"""
fits_open_image

"""
    fits_open_table(filename::String, [mode = 0])

Open an existing data file (like [`fits_open_file`](@ref)) and move to the first
HDU containing either an ASCII or a binary table.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.RW`)
"""
fits_open_table

for (a, b) in (
        (:fits_open_data, "ffdopn"),
        (:fits_open_file, "ffopen"),
        (:fits_open_image, "ffiopn"),
        (:fits_open_table, "fftopn"),
        (:fits_open_diskfile, "ffdkopn"),
    )

    @eval begin
        function ($a)(filename::AbstractString, mode = 0)
            ptr = Ref{Ptr{Cvoid}}()
            status = Ref{Cint}(0)
            ccall(
                ($b, libcfitsio),
                Cint,
                (Ref{Ptr{Cvoid}}, Ptr{UInt8}, Cint, Ref{Cint}),
                ptr,
                filename,
                mode,
                status,
            )
            fits_assert_ok(status[], filename)
            FITSFile(ptr[])
        end
    end
end

# filename is ignored by the C library
function fits_open_memfile(data::Vector{UInt8}, mode = 0, filename = "")
    # Only reading is supported right now
    if Int(mode) != 0
        error("only reading is supported currently, so mode must be 0 or CFITSIO.R. Received mode = $mode")
    end
    ptr = Ref{Ptr{Cvoid}}(C_NULL)
    status = Ref{Cint}(0)
    GC.@preserve data begin
        handle = FITSMemoryHandle(pointer(data), length(data))
        dataptr = Ptr{Ptr{Cvoid}}(pointer_from_objref(handle))
        sizeptr = Ptr{Csize_t}(dataptr + sizeof(Ptr{Cvoid}))
        ccall(
            ("ffomem", libcfitsio),
            Cint,
            (
                Ptr{Ptr{Cvoid}},
                Ptr{UInt8},
                Cint,
                Ptr{Ptr{UInt8}},
                Ptr{Csize_t},
                Csize_t,
                Ptr{Cvoid},
                Ptr{Cint},
            ),
            ptr,
            filename,
            mode,
            dataptr,
            sizeptr,
            2880,
            C_NULL,
            status,
        )
        fits_assert_ok(status[])
    end
    FITSFile(ptr[]), handle
end

"""
    fits_close_file(f::FITSFile)

Close a previously opened FITS file.
"""
fits_close_file

"""
    fits_delete_file(f::FITSFile)

Close an opened FITS file (like [`fits_close_file`](@ref)) and removes it
from the disk.
"""
fits_delete_file

for (a, b) in ((:fits_close_file, "ffclos"), (:fits_delete_file, "ffdelt"))
    @eval begin
        function ($a)(f::FITSFile)

            # fits_close_file() is called during garbage collection, but file
            # may already be closed by user, so we need to check if it is open.
            if (ptr = f.ptr) != C_NULL
                f.ptr = C_NULL # avoid closing twice even if an error occurs
                status = Ref{Cint}(0)
                ccall(($b, libcfitsio), Cint, (Ptr{Cvoid}, Ref{Cint}), ptr, status)
                fits_assert_ok(status[])
            end
        end
    end
end

Base.close(f::FITSFile) = fits_close_file(f)

"""
    fits_file_name(f::FITSFile)

Return the name of the file associated with object `f`.
"""
function fits_file_name(f::FITSFile)
    fits_assert_open(f)
    value = Vector{UInt8}(undef, FLEN_FILENAME)
    status = Ref{Cint}(0)
    ccall(
        (:ffflnm, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        value,
        status,
    )
    fits_assert_ok(status[])
    tostring(value)
end

"""
    fits_file_mode(f::FITSFile)

Return the I/O mode of the FITS file, where 0 indicates a read-only mode and
1 indicates a read-write mode.
"""
function fits_file_mode(f::FITSFile)
    fits_assert_open(f)
    result = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        ("ffflmd", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        result,
        status,
    )
    fits_assert_ok(status[])
    result[]
end


# -----------------------------------------------------------------------------
# header access functions

"""
    fits_get_hdrspace(f::FITSFile) -> (keysexist, morekeys)

Return the number of existing keywords (not counting the END keyword)
and the amount of space currently available for more keywords.
"""
function fits_get_hdrspace(f::FITSFile)
    fits_assert_open(f)
    keysexist = Ref{Cint}(0)
    morekeys = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffghsp, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        keysexist,
        morekeys,
        status,
    )
    fits_assert_ok(status[])
    (keysexist[], morekeys[])
end

function fits_read_key_str(f::FITSFile, keyname::String)
    fits_assert_open(f)
    value = Vector{UInt8}(undef, FLEN_VALUE)
    comment = Vector{UInt8}(undef, FLEN_COMMENT)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkys, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        comment,
        status,
    )
    fits_assert_ok(status[])
    tostring(value), tostring(comment)
end

function fits_read_key_lng(f::FITSFile, keyname::String)
    fits_assert_open(f)
    value = Ref{Clong}(0)
    comment = Vector{UInt8}(undef, FLEN_COMMENT)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkyj, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Clong}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        comment,
        status,
    )
    fits_assert_ok(status[])
    value[], tostring(comment)
end

function fits_read_keys_lng(f::FITSFile, keyname::String, nstart::Integer, nmax::Integer)
    fits_assert_open(f)
    value = Vector{Clong}(undef, nmax - nstart + 1)
    nfound = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgknj, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Cint, Cint, Ptr{Clong}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        keyname,
        nstart,
        nmax,
        value,
        nfound,
        status,
    )
    fits_assert_ok(status[])
    value, nfound[]
end

"""
    fits_read_keyword(f::FITSFile, keyname::String) -> (value, comment)

yields the specified keyword value and commend (as a tuple of strings),
throws and error if the keyword is not found.

"""
function fits_read_keyword(f::FITSFile, keyname::String)
    fits_assert_open(f)
    value = Vector{UInt8}(undef, FLEN_VALUE)
    comment = Vector{UInt8}(undef, FLEN_COMMENT)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkey, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        comment,
        status,
    )
    fits_assert_ok(status[])
    tostring(value), tostring(comment)
end


"""
    fits_read_record(f::FITSFile, keynum::Int) -> String

Return the nth header record in the CHU. The first keyword in the
header is at `keynum = 1`.
"""
function fits_read_record(f::FITSFile, keynum::Integer)
    fits_assert_open(f)
    card = Vector{UInt8}(undef, FLEN_CARD)
    status = Ref{Cint}(0)
    ccall(
        (:ffgrec, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keynum,
        card,
        status,
    )
    fits_assert_ok(status[])
    tostring(card)
end


"""
    fits_read_keyn(f::FITSFile, keynum::Int) -> (name, value, comment)

Return the nth header record in the CHU. The first keyword in the header is at `keynum = 1`.
"""
function fits_read_keyn(f::FITSFile, keynum::Integer)
    fits_assert_open(f)
    # CFITSIO follows the ESO HIERARCH convention where
    # keyword names may be longer than 8 characters (which is the FITS standard)
    # https://heasarc.gsfc.nasa.gov/fitsio/c/f_user/node28.html
    keyname = Vector{UInt8}(undef, FLEN_KEYWORD)
    value = Vector{UInt8}(undef, FLEN_VALUE)
    comment = Vector{UInt8}(undef, FLEN_COMMENT)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkyn, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keynum,
        keyname,
        value,
        comment,
        status,
    )
    fits_assert_ok(status[])
    (
        tostring(keyname),
        tostring(value),
        tostring(comment),
    )
end

"""
    fits_read_atblhdr(f::FITSFile, maxdim::Integer)

Read the header of an ASCII table HDU,
where `maxdim` represents the maximum number of columns to read.
The function returns the length of a row in bytes, the number of
rows, the number of columns, the column names, the byte offsets
to each column, the TFORMn values, the TUNITn values, and the `EXTNAME`
keyword, if any.
"""
fits_read_atblhdr

"""
    fits_read_btblhdr(f::FITSFile, maxdim::Integer)

Read the header of a binary table HDU,
where `maxdim` represents the maximum number of columns to read.
The function returns the number of rows, the number of columns,
the column names, the TFORMn values, the TUNITn values, and the `EXTNAME` and `PCOUNT`
keywords.
"""
fits_read_btblhdr

@eval begin
    function fits_read_atblhdr(f::FITSFile, maxdim::Integer)
        fits_assert_open(f)
        status = Ref{Cint}(0)
        rowlen = Ref{$Long_or_LongLong}(0) # length of table row in bytes
        nrows = Ref{$Long_or_LongLong}(0) # number of rows in the table
        tfields = Ref{Cint}(0) # number of columns in the table
        ttype = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # name of each column
        tbcol = Ref{Int64}(0) # byte offset in row to each column
        tform = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # value of TFORMn keyword for each column (datatype code as string)
        tunit = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # value of TUNITn keyword for each column
        extname = Vector{UInt8}(undef, FLEN_VALUE) # value of EXTNAME keyword, if any
        ccall(
            (ffghtb, libcfitsio),
            Cint,
            (Ptr{Cvoid} #= f.ptr =#,
                Cint #= maxdim =#,
                Ref{$Long_or_LongLong} #= rowlen =#,
                Ref{$Long_or_LongLong} #= nrows =#,
                Ref{Cint} #= tfields =#,
                Ptr{Ptr{UInt8}} #= ttype =#,
                Ref{Int64} #= tbcol =#,
                Ptr{Ptr{UInt8}} #= tform =#,
                Ptr{Ptr{UInt8}} #= tunit =#,
                Ptr{UInt8} #= extname =#,
                Ref{Cint} #= status =#,
                ),
            f.ptr,
            maxdim,
            rowlen,
            nrows,
            tfields,
            ttype,
            tbcol,
            tform,
            tunit,
            extname,
            status
        )
        fits_assert_ok(status[])
        ttype = ttype[1:min(end, tfields[])]
        tform = tform[1:min(end, tfields[])]
        tunit = tunit[1:min(end, tfields[])]
        return Int(rowlen[]), Int(nrows[]), Int(tfields[]),
            map(tostring, ttype), Int(tbcol[]),
            map(tostring, tform), map(tostring, tunit), tostring(extname)
    end
    function fits_read_btblhdr(f::FITSFile, maxdim::Integer)
        fits_assert_open(f)
        status = Ref{Cint}(0)
        nrows = Ref{$Long_or_LongLong}(0) # number of rows in the table
        tfields = Ref{Cint}(0) # number of columns in the table
        ttype = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # name of each column
        tform = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # value of TFORMn keyword for each column (datatype code as string)
        tunit = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim] # value of TUNITn keyword for each column
        extname = Vector{UInt8}(undef, FLEN_VALUE) # value of EXTNAME keyword, if any
        pcount = Ref{Clong}(0) # value of PCOUNT keyword
        ccall(
            (ffghbn, libcfitsio),
            Cint,
            (Ptr{Cvoid} #= f.ptr =#,
                Cint #= maxdim =#,
                Ref{$Long_or_LongLong} #= nrows =#,
                Ref{Cint} #= tfields =#,
                Ptr{Ptr{UInt8}} #= ttype =#,
                Ptr{Ptr{UInt8}} #= tform =#,
                Ptr{Ptr{UInt8}} #= tunit =#,
                Ptr{UInt8} #= extname =#,
                Ref{Clong} #= pcount =#,
                Ref{Cint} #= status =#,
                ),
            f.ptr,
            maxdim,
            nrows,
            tfields,
            ttype,
            tform,
            tunit,
            extname,
            pcount,
            status
        )
        fits_assert_ok(status[])
        ttype = ttype[1:min(end, tfields[])]
        tform = tform[1:min(end, tfields[])]
        tunit = tunit[1:min(end, tfields[])]
        return Int(nrows[]), Int(tfields[]),
            map(tostring, ttype), map(tostring, tform), map(tostring, tunit),
            tostring(extname), Int(pcount[])
    end
end

"""
    fits_write_key(f::FITSFile, keyname::String, value, comment::String)

Write a keyword of the appropriate data type into the CHU.
"""
function fits_write_key(
    f::FITSFile,
    keyname::String,
    value::Union{Real,String},
    comment::String,
    )

    fits_assert_open(f)
    fits_assert_isascii(keyname)
    fits_assert_isascii(comment)
    cvalue = isa(value, String) ? value :
        isa(value, Bool) ? Cint[value] : reinterpret(UInt8, [value])
    status = Ref{Cint}(0)
    ccall(
        (:ffpky, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        cfitsio_typecode(typeof(value)),
        keyname,
        cvalue,
        comment,
        status,
    )
    fits_assert_ok(status[])
end

function fits_write_date(f::FITSFile)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall((:ffpdat, libcfitsio), Cint, (Ptr{Cvoid}, Ref{Cint}), f.ptr, status)
    fits_assert_ok(status[])
end

function fits_write_comment(f::FITSFile, comment::String)
    fits_assert_open(f)
    fits_assert_isascii(comment)
    status = Ref{Cint}(0)
    ccall(
        (:ffpcom, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        comment,
        status,
    )
    fits_assert_ok(status[])
end

function fits_write_history(f::FITSFile, history::String)
    fits_assert_open(f)
    fits_assert_isascii(history)
    status = Ref{Cint}(0)
    ccall(
        (:ffphis, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        history,
        status,
    )
    fits_assert_ok(status[])
end

# update key: if already present, update it, otherwise add it.
for (a, T, S) in (
            ("ffukys", :String, :(Ptr{UInt8})),
            ("ffukyl", :Bool, :Cint),
            ("ffukyj", :Integer, :Int64),
        )
    @eval begin
        function fits_update_key(
            f::FITSFile,
            key::String,
            value::$T,
            comment::Union{String,Ptr{Cvoid}} = C_NULL,
            )

            fits_assert_open(f)
            isa(value, String) && fits_assert_isascii(value)
            isa(comment, String) && fits_assert_isascii(comment)
            status = Ref{Cint}(0)
            ccall(
                ($a, libcfitsio),
                Cint,
                (Ptr{Cvoid}, Ptr{UInt8}, $S, Ptr{UInt8}, Ref{Cint}),
                f.ptr,
                key,
                value,
                comment,
                status,
            )
            fits_assert_ok(status[])
        end
    end
end

function fits_update_key(
        f::FITSFile,
        key::String,
        value::AbstractFloat,
        comment::Union{String,Ptr{Cvoid}} = C_NULL,
    )

    fits_assert_open(f)
    isa(comment, String) && fits_assert_isascii(comment)
    status = Ref{Cint}(0)
    ccall(
        ("ffukyd", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Cdouble, Cint, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        key,
        value,
        -15,
        comment,
        status,
    )
    fits_assert_ok(status[])
end

function fits_update_key(
        f::FITSFile,
        key::String,
        value::Nothing,
        comment::Union{String,Ptr{Cvoid}} = C_NULL,
    )

    fits_assert_open(f)
    isa(comment, String) && fits_assert_isascii(comment)
    status = Ref{Cint}(0)
    ccall(
        ("ffukyu", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        key,
        comment,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_record(f::FITSFile, card::String)

Write a user specified keyword record into the CHU.
"""
function fits_write_record(f::FITSFile, card::String)
    fits_assert_open(f)
    fits_assert_isascii(card)
    status = Ref{Cint}(0)
    ccall(
        (:ffprec, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        card,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_delete_record(f::FITSFile, keynum::Int)

Delete the keyword record at the specified index.
"""
function fits_delete_record(f::FITSFile, keynum::Integer)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall((:ffdrec, libcfitsio), Cint, (Ptr{Cvoid}, Cint, Ref{Cint}), f.ptr, keynum, status)
    fits_assert_ok(status[])
end

"""
    fits_delete_key(f::FITSFile, keyname::String)

Delete the keyword named `keyname`.
"""
function fits_delete_key(f::FITSFile, keyname::String)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffdkey, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_hdr2str(f::FITSFile, nocomments::Bool=false)

Return the header of the CHDU as a string. If `nocomments` is `true`, comment
cards are stripped from the output.
"""
function fits_hdr2str(f::FITSFile, nocomments::Bool = false)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    header = Ref{Ptr{UInt8}}()
    nkeys = Ref{Cint}(0)
    ccall(
        (:ffhdr2str, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ref{Ptr{UInt8}}, Cint, Ptr{Ptr{UInt8}}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        nocomments,
        C_NULL,
        0,
        header,
        nkeys,
        status,
    )
    result = unsafe_string(header[])

    # free header pointer allocated by cfitsio (result is a copy)
    ccall((:fffree, libcfitsio), Ref{Cint}, (Ptr{UInt8}, Ref{Cint}), header[], status)
    fits_assert_ok(status[])
    result
end


# -----------------------------------------------------------------------------
# HDU info functions and moving the current HDU

function hdu_int_to_type(hdu_type_int)
    if hdu_type_int == 0
        return :image_hdu
    elseif hdu_type_int == 1
        return :ascii_table
    elseif hdu_type_int == 2
        return :binary_table
    end

    :unknown
end

"""
    fits_movabs_hdu(f::FITSFile, hduNum::Integer)

Change the current HDU to the value specified by `hduNum`, and return a symbol
describing the type of the HDU.

Possible symbols are: `image_hdu`, `ascii_table`, or `binary_table`.
The value of `hduNum` must range between 1 and the value returned by
[`fits_get_num_hdus`](@ref).
"""
function fits_movabs_hdu end

"""
    fits_movrel_hdu(f::FITSFile, hduNum::Integer)

Change the current HDU by moving forward or backward by `hduNum` HDUs
(positive means forward), and return the same as [`fits_movabs_hdu`](@ref).
"""
function fits_movrel_hdu end
for (a, b) in ((:fits_movabs_hdu, "ffmahd"), (:fits_movrel_hdu, "ffmrhd"))
    @eval begin
        function ($a)(f::FITSFile, hduNum::Integer)
            fits_assert_open(f)
            hdu_type = Ref{Cint}(0)
            status = Ref{Cint}(0)
            ccall(
                ($b, libcfitsio),
                Cint,
                (Ptr{Cvoid}, Cint, Ref{Cint}, Ref{Cint}),
                f.ptr,
                hduNum,
                hdu_type,
                status,
            )
            fits_assert_ok(status[])
            hdu_int_to_type(hdu_type[])
        end
    end
end

"""
    fits_movnam_hdu(f::FITSFile, extname::String, extver::Integer=0,
                    hdu_type_int::Integer=-1)

Change the current HDU by moving to the (first) HDU which has the specified
extension type and EXTNAME and EXTVER keyword values (or HDUNAME and HDUVER keywords).

If `extver` is 0 (the default) then the EXTVER keyword is ignored and the first HDU
with a matching EXTNAME (or HDUNAME) keyword will be found. If `hdu_type_int`
is -1 (the default) only the extname and extver values will be used to locate the
correct extension. If no matching HDU is found in the file, the current HDU will
remain unchanged.
"""
function fits_movnam_hdu(
        f::FITSFile,
        extname::String,
        extver::Integer = 0,
        hdu_type::Integer = -1,
    )

    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffmnhd, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Cint, Ref{Cint}),
        f.ptr,
        hdu_type,
        extname,
        extver,
        status,
    )
    fits_assert_ok(status[])
end

function fits_get_hdu_num(f::FITSFile)
    fits_assert_open(f)
    hdunum = Ref{Cint}(0)
    ccall((:ffghdn, libcfitsio), Cint, (Ptr{Cvoid}, Ref{Cint}), f.ptr, hdunum)
    hdunum[]
end

function fits_get_hdu_type(f::FITSFile)
    fits_assert_open(f)
    hdutype = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffghdt, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        hdutype,
        status,
    )
    fits_assert_ok(status[])
    hdu_int_to_type(hdutype[])
end

"""
    fits_delete_hdu(f::FITSFile)

Delete the HDU from the FITS file and shift the following HDUs forward.
If `f` is the primary HDU in the file then it'll be replaced by a
null primary HDU with no data and minimal header information.

Return a symbol to indicate the type of the new current HDU.
Possible symbols are: `image_hdu`, `ascii_table`, or `binary_table`.
The value of `hduNum` must range between 1 and the value returned by
[`fits_get_num_hdus`](@ref).
"""
function fits_delete_hdu(f::FITSFile)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    hdutype = Ref{Cint}(0)
    ccall(
        (:ffdhdu, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        hdutype,
        status,
    )
    fits_assert_ok(status[])
    hdu_int_to_type(hdutype[])
end

# -----------------------------------------------------------------------------
# image HDU functions

"""
    fits_get_img_size(f::FITSFile)

Get the dimensions of the image.
"""
fits_get_img_size
for (a, b) in (
        (:fits_get_img_type, "ffgidt"),
        (:fits_get_img_equivtype, "ffgiet"),
        (:fits_get_img_dim, "ffgidm"),
    )

    @eval function ($a)(f::FITSFile)
        fits_assert_open(f)
        result = Ref{Cint}(0)
        status = Ref{Cint}(0)
        ccall(
            ($b, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}),
            f.ptr,
            result,
            status,
        )
        fits_assert_ok(status[])
        result[]
    end
end

"""
    fits_create_img(f::FITSFile, T::Type, naxes::Vector{<:Integer})

Create a new primary array or IMAGE extension with the specified data type `T` and size `naxes`.
"""
function fits_create_img(f::FITSFile, ::Type{T}, naxes::Vector{<:Integer}) where {T}
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffcrimll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{Int64}, Ref{Cint}),
        f.ptr,
        bitpix_from_type(T),
        length(naxes),
        convert(Vector{Int64}, naxes),
        status,
    )
    fits_assert_ok(status[])
end

# This method accepts a tuple of pixels instead of a vector
function fits_create_img(f::FITSFile, ::Type{T}, naxes::NTuple{N,Integer}) where {T,N}
    status = Ref{Cint}(0)
    naxesr = Ref(convert(NTuple{N,Int64}, naxes))
    ccall(
        (:ffcrimll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{NTuple{N,Int64}}, Ref{Cint}),
        f.ptr,
        bitpix_from_type(T),
        N,
        naxesr,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_create_img(f::FITSFile, A::AbstractArray)

Create a new primary array or IMAGE extension with the element type and size of `A`,
that is capable of storing the entire array `A`.
"""
fits_create_img(f::FITSFile, a::AbstractArray) = fits_create_img(f, eltype(a), size(a))

"""
    fits_insert_img(f::FITSFile, T::Type,
                    naxes::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}})

Insert a new image extension immediately following the CHDU, or insert a new Primary Array
at the beginning of the file.
"""
function fits_insert_img(f::FITSFile, T::Type, naxes::Vector{<:Integer})
    fits_assert_open(f)

    status = Ref{Cint}(0)
    ccall(
        (:ffiimgll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Cint,
            Ptr{Int64},
            Ref{Cint},
        ),
        f.ptr,
        bitpix_from_type(T),
        length(naxes),
        convert(Vector{Int64}, naxes),
        status,
    )
    fits_assert_ok(status[])
end

function fits_insert_img(f::FITSFile, T::Type, naxes::NTuple{N,Integer}) where {N}
    fits_assert_open(f)

    status = Ref{Cint}(0)
    naxesr = Ref(map(Int64, naxes))
    ccall(
        (:ffiimgll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Cint,
            Ptr{NTuple{N,Int64}},
            Ref{Cint},
        ),
        f.ptr,
        bitpix_from_type(T),
        N,
        naxesr,
        status,
    )
    fits_assert_ok(status[])
end

fits_insert_img(f::FITSFile, a::AbstractArray) = fits_insert_img(f, eltype(a), size(a))

"""
    fits_write_pix(f::FITSFile,
                   fpixel::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}},
                   nelements::Integer, data::StridedArray)

Write `nelements` pixels from `data` into the FITS file starting from the pixel `fpixel`.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_write_pixnull`](@ref)
"""
function fits_write_pix(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        nelements::Integer,
        data::StridedArray,
    )

    fits_assert_open(f)

    status = Ref{Cint}(0)
    ccall(
        (:ffppxll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Int64},
            Int64,
            Ptr{Cvoid},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Int64}, fpixel),
        nelements,
        data,
        status,
    )
    fits_assert_ok(status[])
end

# This method accepts a tuple of pixels instead of a vector
function fits_write_pix(
    f::FITSFile,
    fpixel::NTuple{N,Integer},
    nelements::Integer,
    data::StridedArray,
    ) where {N}

    fits_assert_open(f)

    status = Ref{Cint}(0)
    fpixelr = Ref(convert(NTuple{N,Int64}, fpixel))
    ccall(
        (:ffppxll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{NTuple{N,Int64}}, Int64, Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        nelements,
        data,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_pix(f::FITSFile, data::StridedArray)

Write the entire array `data` into the FITS file.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_write_pixnull`](@ref), [`fits_write_subset`](@ref)
"""
function fits_write_pix(f::FITSFile, data::StridedArray)
    fits_write_pix(f, onest(Int64, ndims(data)), length(data), data)
end

# cfitsio expects the null value to be of the same type as the eltype of data
# It may also be C_NULL or nothing
# We check if it is a number and convert it to the correct eltype, otherwise leave it alone
_maybeconvert(::Type{ET}, nullval::Real) where {ET<:Real} = convert(ET, nullval)
_maybeconvert(::Type, nullval) = nullval

"""
    fits_write_pixnull(f::FITSFile,
                       fpixel::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}},
                       nelements::Integer, data::StridedArray, nulval)

Write `nelements` pixels from `data` into the FITS file starting from the pixel `fpixel`.
The argument `nulval` specifies the values that are to be considered as "null values", and replaced
by appropriate numbers corresponding to the element type of `data`.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_write_pix`](@ref)
"""
function fits_write_pixnull(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        nelements::Integer,
        data::StridedArray,
        nulval,
    )

    fits_assert_open(f)

    status = Ref{Cint}(0)
    ccall(
        (:ffppxnll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Int64},
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Int64}, fpixel),
        nelements,
        data,
        Ref(_maybeconvert(eltype(data), nulval)),
        status,
    )
    fits_assert_ok(status[])
end

function fits_write_pixnull(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        nelements::Integer,
        data::StridedArray,
        nulval,
    ) where {N}

    fits_assert_open(f)
    status = Ref{Cint}(0)
    fpixelr = Ref(convert(NTuple{N,Int64}, fpixel))

    ccall(
        (:ffppxnll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Int64}},
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        nelements,
        data,
        Ref(_maybeconvert(eltype(data), nulval)),
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_pixnull(f::FITSFile, data::StridedArray, nulval)

Write the entire array `data` into the FITS file.
The argument `nulval` specifies the values that are to be considered as
"null values", and replaced by appropriate numbers corresponding to
the element type of `data`.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_write_pix`](@ref)
"""
function fits_write_pixnull(f::FITSFile, data::StridedArray, nulval)
    fits_write_pixnull(f, onest(Int64, ndims(data)), length(data), data, nulval)
end

"""
    fits_write_subset(f::FITSFile,
                      fpixel::V, lpixel::V,
                      data::StridedArray) where {V<:Union{Vector{<:Integer}, Tuple{Vararg{Integer}}}}

Write a rectangular section of the FITS image. The number of pixels to be
written will be computed from the first and last pixels (specified as the
`fpixel` and `lpixel` arguments respectively).

!!! note
    The section to be written out must be contiguous in memory, so all the
    dimensions aside from the last one must span the entire axis range.
    The arguments `fpixel` and `lpixel` must account for this.

See also: [`fits_write_pix`](@ref)
"""
function fits_write_subset(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        lpixel::Vector{<:Integer},
        data::StridedArray,
    )

    fits_assert_open(f)

    status = Ref{Cint}(0)
    ccall(
        (:ffpss, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Clong},
            Ptr{Clong},
            Ptr{Cvoid},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Clong}, fpixel),
        convert(Vector{Clong}, lpixel),
        data,
        status,
    )
    fits_assert_ok(status[])
end

function fits_write_subset(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        lpixel::NTuple{N,Integer},
        data::StridedArray,
    ) where {N}

    fits_assert_open(f)

    status = Ref{Cint}(0)
    fpixelr, lpixelr = map((fpixel, lpixel)) do x
        Ref(convert(NTuple{N,Clong}, x))
    end

    ccall(
        (:ffpss, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Clong}},
            Ptr{NTuple{N,Clong}},
            Ptr{Cvoid},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        lpixelr,
        data,
        status,
    )
    fits_assert_ok(status[])
end

function fits_read_pix(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        nelements::Integer,
        nullval,
        data::StridedArray,
    )

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgpxvll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Int64},
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Int64}, fpixel),
        nelements,
        Ref(_maybeconvert(eltype(data), nullval)),
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

# This method accepts a tuple of pixels instead of a vector
function fits_read_pix(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        nelements::Integer,
        nullval,
        data::StridedArray,
    ) where {N}

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    fpixelr = Ref(convert(NTuple{N,Int64}, fpixel))
    ccall(
        (:ffgpxvll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Int64}},
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        nelements,
        Ref(_maybeconvert(eltype(data), nullval)),
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

"""
    fits_read_pix(f::FITSFile,
                  fpixel::NTuple{Vector{<:Integer}, Tuple{Vararg{Integer}}},
                  nelements::Integer, [nulval], data::StridedArray)

Read `nelements` pixels from the FITS file into `data` starting from the pixel `fpixel`.
If the optional argument `nulval` is specified and is non-zero, any null value present in the array will be
replaced by it.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_read_pixnull`](@ref), [`fits_read_subset`](@ref)
"""
function fits_read_pix(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        nelements::Integer,
        data::StridedArray,
    )

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgpxvll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Int64},
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Int64}, fpixel),
        nelements,
        C_NULL,
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

# This method accepts a tuple of pixels instead of a vector
function fits_read_pix(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        nelements::Int,
        data::StridedArray,
    ) where {N}

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    fpixelr = Ref(convert(NTuple{N,Int64}, fpixel))
    ccall(
        (:ffgpxvll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{NTuple{N,Int64}}, Int64, Ptr{Cvoid}, Ptr{Cvoid}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        nelements,
        C_NULL,
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

"""
    fits_read_pix(f::FITSFile, data::StridedArray, [nulval])

Read `length(data)` pixels from the FITS file into `data` starting from the first pixel.
The optional argument `nulval`, if specified and non-zero, is used to replace
any null value present in the array.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_read_pixnull`](@ref)
"""
function fits_read_pix(f::FITSFile, data::StridedArray)
    fits_read_pix(f, onest(Int64, ndims(data)), length(data), data)
end

function fits_read_pix(f::FITSFile, data::StridedArray, nulval)
    fits_read_pix(f, onest(Int64, ndims(data)), length(data), nulval, data)
end

"""
    fits_read_pixnull(f::FITSFile,
                      fpixel::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}},
                      nelements::Integer, data::StridedArray, nullarray::Array{UInt8})

Read `nelements` pixels from the FITS file into `data` starting from the pixel `fpixel`.
At output, the indices of `nullarray` where `data` has a corresponding null
value are set to `1`.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_read_pix`](@ref)
"""
function fits_read_pixnull(f::FITSFile,
        fpixel::Vector{<:Integer},
        nelements::Integer,
        data::StridedArray,
        nullarray::Array{UInt8},
    )

    fits_assert_open(f)
    fits_assert_nonempty(f)

    if length(data) != length(nullarray)
        error("data and nullarray must have the same number of elements")
    end

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgpxfll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Int64},
            Int64,
            Ptr{Cvoid},
            Ptr{UInt8},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Int64}, fpixel),
        nelements,
        data,
        nullarray,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

function fits_read_pixnull(f::FITSFile,
        fpixel::NTuple{N,Integer},
        nelements::Integer,
        data::StridedArray,
        nullarray::Array{UInt8},
    ) where {N}

    fits_assert_open(f)
    fits_assert_nonempty(f)

    if length(data) != length(nullarray)
        error("data and nullarray must have the same number of elements")
    end

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    fpixelr = Ref(convert(NTuple{N,Int64}, fpixel))

    ccall(
        (:ffgpxfll, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Int64}},
            Int64,
            Ptr{Cvoid},
            Ptr{UInt8},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        nelements,
        data,
        nullarray,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

"""
    fits_read_pixnull(f::FITSFile, data::StridedArray, nullarray::Array{UInt8})

Read `length(data)` pixels from the FITS file into `data` starting from the first pixel.
At output, the indices of `nullarray` where `data` has a corresponding null value are set to `1`.

!!! note
    `data` needs to be stored contiguously in memory.

See also: [`fits_read_pix`](@ref)
"""
function fits_read_pixnull(f::FITSFile, data::StridedArray, nullarray::Array{UInt8})
    fits_read_pixnull(f, onest(Int64, ndims(data)), length(data), data, nullarray)
end

"""
    fits_read_subset(f::FITSFile,
                     fpixel::V, lpixel::V, inc::V,
                     [nulval],
                     data::StridedArray) where {V<:Union{Vector{<:Integer}, Tuple{Vararg{Integer}}}}

Read a rectangular section of the FITS image. The number of pixels to be read
will be computed from the first and last pixels (specified as
the `fpixel` and `lpixel` arguments respectively).
The argument `inc` specifies the step-size in pixels along each dimension.

If the optional argument `nulval` is specified and is non-zero, null values in
`data` will be replaced by it.

!!! note
    `data` needs to be stored contiguously in memory, and will be populated
    contiguously with the pixels that are read in.

See also: [`fits_read_pix`](@ref)
"""
function fits_read_subset(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        lpixel::Vector{<:Integer},
        inc::Vector{<:Integer},
        data::StridedArray,
    )

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgsv, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Clong},
            Ptr{Clong},
            Ptr{Clong},
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Clong}, fpixel),
        convert(Vector{Clong}, lpixel),
        convert(Vector{Clong}, inc),
        C_NULL,
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

function fits_read_subset(
        f::FITSFile,
        fpixel::Vector{<:Integer},
        lpixel::Vector{<:Integer},
        inc::Vector{<:Integer},
        nulval,
        data::StridedArray,
    )

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgsv, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{Clong},
            Ptr{Clong},
            Ptr{Clong},
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        convert(Vector{Clong}, fpixel),
        convert(Vector{Clong}, lpixel),
        convert(Vector{Clong}, inc),
        Ref(_maybeconvert(eltype(data), nulval)),
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

function fits_read_subset(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        lpixel::NTuple{N,Integer},
        inc::NTuple{N,Integer},
        data::StridedArray,
    ) where {N}

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    fpixelr, lpixelr, incr  = map((fpixel, lpixel, inc)) do x
        Ref(convert(NTuple{N,Clong}, x))
    end

    ccall(
        (:ffgsv, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Clong}},
            Ptr{NTuple{N,Clong}},
            Ptr{NTuple{N,Clong}},
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        lpixelr,
        incr,
        C_NULL,
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

function fits_read_subset(
        f::FITSFile,
        fpixel::NTuple{N,Integer},
        lpixel::NTuple{N,Integer},
        inc::NTuple{N,Integer},
        nulval,
        data::StridedArray,
    ) where {N}

    fits_assert_open(f)
    fits_assert_nonempty(f)

    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    fpixelr, lpixelr, incr  = map((fpixel, lpixel, inc)) do x
        Ref(convert(NTuple{N,Clong}, x))
    end
    ccall(
        (:ffgsv, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Ptr{NTuple{N,Clong}},
            Ptr{NTuple{N,Clong}},
            Ptr{NTuple{N,Clong}},
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        fpixelr,
        lpixelr,
        incr,
        Ref(_maybeconvert(eltype(data), nulval)),
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
    anynull[]
end

"""
    fits_copy_image_section(fin::FITSFile, fout::FITSFile, section::String)

Copy a rectangular section of an image from `fin` and write it to a new FITS primary
image or image extension in `fout`. The section specifier is described on the
[`CFITSIO website`](https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/node97.html).
"""
function fits_copy_image_section(fin::FITSFile, fout::FITSFile, section::String)
    fits_assert_open(fin)
    fits_assert_nonempty(fin)
    fits_assert_open(fout)

    status = Ref{Cint}(0)
    ccall(
        (:fits_copy_image_section, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        fin.ptr,
        fout.ptr,
        section,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_null_img(f::FITSFile, firstelem::Integer, nelements::Integer)

Set a stretch of elements to the appropriate null value, starting from the
pixel number `firstelem` and extending over `nelements` pixels.
"""
function fits_write_null_img(f::FITSFile, firstelem::Integer, nelements::Integer)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffpprn, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Clonglong, Clonglong, Ref{Cint}),
        f.ptr,
        firstelem,
        nelements,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_resize_img(f::FITSFile, T::Type, naxis::Integer,
                    sz::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}})

Modify the size, dimensions and optionally the element type of the image in `f`.
The new image will have an element type `T`, be a `naxis`-dimensional image with
size `sz`. If the new image is larger than the existing one, it will be
zero-padded at the end. If the new image is smaller, existing image data will be
truncated.

!!! note
    This method reinterprets the data instead of coercing the elements.

# Example
```jldoctest
julia> f = fits_clobber_file(tempname());

julia> a = [1 2; 3 4];

julia> fits_create_img(f, a);

julia> fits_write_pix(f, a);

julia> fits_get_img_size(f)
2-element Vector{Int64}:
 2
 2

julia> fits_resize_img(f, [3,3]);

julia> fits_get_img_size(f)
2-element Vector{Int64}:
 3
 3

julia> b = similar(a, (3,3));

julia> fits_read_pix(f, b); b
3×3 Matrix{Int64}:
 1  4  0
 3  0  0
 2  0  0

julia> fits_resize_img(f, [4]);

julia> b = similar(a, (4,));

julia> fits_read_pix(f, b); b
4-element Vector{Int64}:
 1
 3
 2
 4
```
"""
function fits_resize_img(f::FITSFile, T::Type, naxis::Integer, sz::Vector{<:Integer})
    fits_assert_open(f)
    fits_assert_nonempty(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffrsim, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{Clong}, Ref{Cint}),
        f.ptr,
        bitpix_from_type(T),
        naxis,
        convert(Vector{Clong}, sz),
        status,
    )
    fits_assert_ok(status[])
end

function fits_resize_img(f::FITSFile, T::Type, naxis::Integer, sz::NTuple{N,Integer}) where {N}
    fits_assert_open(f)
    fits_assert_nonempty(f)
    status = Ref{Cint}(0)
    szr = Ref(convert(NTuple{N,Clong}, sz))
    ccall(
        (:ffrsim, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{NTuple{N,Clong}}, Ref{Cint}),
        f.ptr,
        bitpix_from_type(T),
        naxis,
        szr,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_resize_img(f::FITSFile, sz::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}})

Resize the image to the new size `sz`. The element type is preserved, and the number of dimensions
is set equal to `length(sz)`.
"""
function fits_resize_img(f::FITSFile, sz::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}})
    fits_assert_open(f)
    fits_assert_nonempty(f)
    T = type_from_bitpix(fits_get_img_type(f))
    naxis = length(sz)
    fits_resize_img(f, T, naxis, sz)
end

"""
    fits_resize_img(f::FITSFile, T::Type)

Change the element type of the image to `T`, leaving the size unchanged.
"""
function fits_resize_img(f::FITSFile, T::Type)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    sz = fits_get_img_size(f)
    naxis = fits_get_img_dim(f)
    fits_resize_img(f, T, naxis, sz)
end

# -----------------------------------------------------------------------------
# ASCII/binary table HDU functions

# The three fields are: ttype, tform, tunit (CFITSIO's terminology)
const ColumnDef = Tuple{String,String,String}

"""
    fits_create_binary_tbl(f::FITSFile, numrows::Integer,
                           coldefs::Array{ColumnDef},
                           extname::String)

Append a new HDU containing a binary table. The meaning of the parameters is the same
as in a call to [`fits_create_ascii_tbl`](@ref).

In general, one should pick this function for creating tables in a new HDU,
as binary tables require less space on the disk and are more efficient to read and write.
(Moreover, a few datatypes are not supported in ASCII tables).
"""
function fits_create_binary_tbl end

"""
    fits_create_ascii_tbl(f::FITSFile, numrows::Integer,
                          coldefs::Array{CFITSIO.ColumnDef},
                          extname::String)

Append a new HDU containing an ASCII table.

The table will have `numrows` rows (this parameter can be set to zero), each
initialized with the default value. In order to create a table, the programmer
must specify the characteristics of each column. The columns are specified by the
`coldefs` variable, which is an array of tuples.
Each tuple must have three string fields:

1. The name of the column.
2. The data type and the repetition count. It must be a string made by a number
   (the repetition count) followed by a letter specifying the type (in the example
   above, `D` stands for `Float64`, `E` stands for `Float32`, `A` stands for `Char`).
   Refer to the CFITSIO documentation for more information about the syntax of this
   parameter.
3. The measure unit of this field. This is used only as a comment.

The value of `extname` sets the "extended name" of the table, i.e., a string
that in some situations can be used to refer to the HDU itself.

Note that, unlike for binary tables, CFITSIO puts some limitations to the
types that can be used in an ASCII table column. Refer to the CFITSIO manual
for further information.

See also [`fits_create_binary_tbl`](@ref) for a similar function which
creates binary tables.
"""
function fits_create_ascii_tbl end
for (a, b) in ((:fits_create_binary_tbl, 2), (:fits_create_ascii_tbl, 1))
    @eval begin
        function ($a)(
            f::FITSFile,
            numrows::Integer,
            coldefs::Array{ColumnDef},
            extname::String,
            )

            fits_assert_open(f)

            # Ensure that extension name, column names and units are
            # ASCII, as these get written to the file. We don't check
            # need to check that tform is ASCII because presumably
            # cfitsio will thrown an appropriate error if it doesn't
            # recognize the tform string.
            fits_assert_isascii(extname)
            for coldef in coldefs
                fits_assert_isascii(coldef[1])
                fits_assert_isascii(coldef[3])
            end

            # get length and convert coldefs to three arrays of Ptr{Uint8}
            ntype = length(coldefs)
            GC.@preserve coldefs begin
                ttype = [pointer(x[1]) for x in coldefs]
                tform = [pointer(x[2]) for x in coldefs]
                tunit = [pointer(x[3]) for x in coldefs]
                status = Ref{Cint}(0)

                ccall(
                    ("ffcrtb", libcfitsio),
                    Cint,
                    (
                        Ptr{Cvoid},
                        Cint,
                        Int64,
                        Cint,
                        Ptr{Ptr{UInt8}},
                        Ptr{Ptr{UInt8}},
                        Ptr{Ptr{UInt8}},
                        Ptr{UInt8},
                        Ref{Cint},
                    ),
                    f.ptr,
                    $b,
                    numrows,
                    ntype,
                    ttype,
                    tform,
                    tunit,
                    extname,
                    status,
                )
            end
            fits_assert_ok(status[])
        end
    end
end

"""
    fits_get_num_hdus(f::FITSFile)

Return the number of HDUs in the file.
"""
fits_get_num_hdus
for (a, b, T) in ((:fits_get_num_cols, "ffgncl", :Cint),
                  (:fits_get_num_hdus, "ffthdu", :Cint),
                  (:fits_get_rowsize, "ffgrsz", :Clong))

    @eval begin
        function ($a)(f::FITSFile)
            fits_assert_open(f)
            result = Ref{$T}(0)
            status = Ref{Cint}(0)
            ccall(
                ($b, libcfitsio),
                Cint,
                (Ptr{Cvoid}, Ref{$T}, Ref{Cint}),
                f.ptr,
                result,
                status,
            )
            fits_assert_ok(status[])
            result[]
        end
    end
end

function fits_get_colnum(f::FITSFile, tmplt::String; case_sensitive::Bool = true)
    fits_assert_open(f)
    result = Ref{Cint}(0)
    status = Ref{Cint}(0)

    # Second argument is case-sensitivity of search: 0 = case-insensitive
    #                                                1 = case-sensitive
    ccall(
        ("ffgcno", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        case_sensitive,
        tmplt,
        result,
        status,
    )
    fits_assert_ok(status[])
    return result[]
end

"""
    fits_get_coltype(f::FITSFile, colnum::Integer)

Provided that the current HDU contains either an ASCII or binary table, return
information about the column at position `colnum` (counting from 1).

Return is a tuple containing

- `typecode`: CFITSIO integer type code of the column.
- `repcount`: Repetition count for the column.
- `width`: Width of an individual element.
"""
function fits_get_coltype end

@eval begin
    function fits_get_coltype(ff::FITSFile, colnum::Integer)
        fits_assert_open(ff)
        typecode = Ref{Cint}(0)
        repcnt = Ref{$Long_or_LongLong}(0)
        width = Ref{$Long_or_LongLong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgtcl, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ref{Cint}, Ref{$Long_or_LongLong}, Ref{$Long_or_LongLong}, Ref{Cint}),
            ff.ptr,
            colnum,
            typecode,
            repcnt,
            width,
            status,
        )
        fits_assert_ok(status[])
        return Int(typecode[]), Int(repcnt[]), Int(width[])
    end

    function fits_get_eqcoltype(ff::FITSFile, colnum::Integer)
        fits_assert_open(ff)
        typecode = Ref{Cint}(0)
        repcnt = Ref{$Long_or_LongLong}(0)
        width = Ref{$Long_or_LongLong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffeqty, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ref{Cint}, Ref{$Long_or_LongLong}, Ref{$Long_or_LongLong}, Ref{Cint}),
            ff.ptr,
            colnum,
            typecode,
            repcnt,
            width,
            status,
        )
        fits_assert_ok(status[])
        return Int(typecode[]), Int(repcnt[]), Int(width[])
    end

    function fits_get_img_size(f::FITSFile)
        fits_assert_open(f)
        ndim = fits_get_img_dim(f)
        naxes = Vector{$Long_or_LongLong}(undef, ndim)
        status = Ref{Cint}(0)
        ccall(
            ($ffgisz, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ptr{$Long_or_LongLong}, Ref{Cint}),
            f.ptr,
            ndim,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        naxes
    end

    function fits_get_img_size(f::FITSFile, ::Val{N}) where {N}
        naxes = Ref(zerost($Long_or_LongLong, N))
        status = Ref{Cint}(0)
        ccall(
            ($ffgisz, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ptr{NTuple{N,$Long_or_LongLong}}, Ref{Cint}),
            f.ptr,
            N,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        naxes[]
    end

    function fits_get_num_rows(f::FITSFile)
        fits_assert_open(f)
        result = Ref{$Long_or_LongLong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgnrw, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Ref{$Long_or_LongLong}, Ref{Cint}),
            f.ptr,
            result,
            status,
        )
        fits_assert_ok(status[])
        return Int(result[])
    end

    # `fits_read_tdim` returns the dimensions of a table column in a
    # binary table. Normally this information is given by the TDIMn
    # keyword, but if this keyword is not present then this routine
    # returns `[r]` with `r` equals to the repeat count in the TFORM
    # keyword.
    function fits_read_tdim(ff::FITSFile, colnum::Integer)
        fits_assert_open(ff)
        naxes = Vector{$Long_or_LongLong}(undef, 99)  # 99 is the maximum allowed number of axes
        naxis = Ref{Cint}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgtdm, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Cint, Ref{Cint}, Ptr{$Long_or_LongLong}, Ref{Cint}),
            ff.ptr,
            colnum,
            length(naxes),
            naxis,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        return naxes[1:naxis[]]
    end

    function fits_write_tdim(ff::FITSFile, colnum::Integer, naxes::Array{$Long_or_LongLong})
        fits_assert_open(ff)
        status = Ref{Cint}(0)
        ccall(
            ($ffptdm, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Cint, Ptr{$Long_or_LongLong}, Ref{Cint}),
            ff.ptr,
            colnum,
            length(naxes),
            naxes,
            status,
        )
        fits_assert_ok(status[])
    end

    function fits_read_descript(f::FITSFile, colnum::Integer, rownum::Integer)
        fits_assert_open(f)
        repeat = Ref{$Long_or_LongLong}(0)
        offset = Ref{$Long_or_LongLong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgdes, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Int64, Ref{$Long_or_LongLong}, Ref{$Long_or_LongLong}, Ref{Cint}),
            f.ptr,
            colnum,
            rownum,
            repeat,
            offset,
            status,
        )
        fits_assert_ok(status[])
        return Int(repeat[]), Int(offset[])
    end
end

"""
    fits_read_col(f, colnum, firstrow, firstelem, data)

Read data from one column of an ASCII/binary table and convert the data into the
specified type `T`.

### Arguments ###

* `f::FITSFile`: the file to be read.
* `colnum::Integer`: the column number, where the value of the first column is `1`.
* `firstrow::Integer`: the elements to be read start from this row.
* `firstelem::Integer`: specifies which is the first element to be read, when each
  cell contains more than one element (i.e., the "repetition count" of the field is
  greater than one).
* `data::Array`: at the end of the call, this will be filled with the elements read
from the column. The length of the array gives the overall number of elements.
"""
function fits_read_col(
        f::FITSFile,
        colnum::Integer,
        firstrow::Integer,
        firstelem::Integer,
        data::Array{String},
    )

    fits_assert_open(f)

    # get width: number of characters in each string
    typecode, repcount, width = fits_get_eqcoltype(f, colnum)

    # ensure that data are strings, otherwise cfitsio will try to write
    # formatted strings, which have widths given by fits_get_col_display_width
    # not by the repeat value from fits_get_coltype.
    abs(typecode) == 16 || error("not a string column")

    # create an array of character buffers of the correct width
    buffers = [Vector{UInt8}(undef, width+1) for i in 1:length(data)]
    # Call the CFITSIO function
    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgcvs, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Int64,
            Int64,
            Int64,
            Ptr{UInt8},
            Ptr{Ptr{UInt8}},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        colnum,
        firstrow,
        firstelem,
        length(data),
        " ",
        buffers,
        anynull,
        status,
    )
    fits_assert_ok(status[])

    # Create strings out of the buffers, terminating at null characters.
    # Note that `String(x)` does not copy the buffer x.
    for i in 1:length(data)
        zeropos = something(findfirst(isequal(0x00), buffers[i]), 0)
        data[i] = (zeropos >= 1) ? String(buffers[i][1:(zeropos-1)]) : String(buffers[i])
    end
end

function fits_read_col(
        f::FITSFile,
        colnum::Integer,
        firstrow::Integer,
        firstelem::Integer,
        data::Array,
    )

    fits_assert_open(f)
    anynull = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgcv, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Cint,
            Int64,
            Int64,
            Int64,
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
            Ref{Cint},
        ),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        colnum,
        firstrow,
        firstelem,
        length(data),
        C_NULL,
        data,
        anynull,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_col(f, colnum, firstrow, firstelem, data)

Write some data in one column of a ASCII/binary table.

If there is no room for the elements, new rows will be created. (It is therefore
useless to call [`fits_insert_rows`](@ref) if you only need to *append* elements
to the end of a table.)

* `f::FITSFile`: the file in which data will be written.
* `colnum::Integer`: the column number, where the value of the first column is `1`.
* `firstrow::Integer`: the data wil be written from this row onwards.
* `firstelem::Integer`: specifies the position in the row where the first element
  will be written.
* `data::Array`: contains the elements that are to be written to the column of the table.
"""
function fits_write_col(
    f::FITSFile,
    colnum::Integer,
    firstrow::Integer,
    firstelem::Integer,
    data::Array{String},
    )

    fits_assert_open(f)
    for el in data
        fits_assert_isascii(el)
    end
    status = Ref{Cint}(0)
    ccall(
        (:ffpcls, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Int64, Int64, Int64, Ptr{Ptr{UInt8}}, Ref{Cint}),
        f.ptr,
        colnum,
        firstrow,
        firstelem,
        length(data),
        data,
        status,
    )
    fits_assert_ok(status[])
end

function fits_write_col(
    f::FITSFile,
    colnum::Integer,
    firstrow::Integer,
    firstelem::Integer,
    data::Array,
    )

    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffpcl, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Int64, Int64, Int64, Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        cfitsio_typecode(eltype(data)),
        colnum,
        firstrow,
        firstelem,
        length(data),
        data,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_insert_rows(f::FITSFile, firstrow::Integer, nrows::Integer)

Insert a number of rows equal to `nrows` after the row number `firstrow`.

The elements in each row are initialized to their default value: you can
modify them later using [`fits_write_col`](@ref).

Since the first row is at position 1, in order to insert rows *before*
the first one `firstrow` must be equal to zero.
"""
fits_insert_rows

"""
    fits_delete_rows(f::FITSFile, firstrow::integer, nrows::Integer)

Delete `nrows` rows, starting from the one at position `firstrow`. The index of
the first row is 1.
"""
fits_delete_rows

for (a, b) in ((:fits_insert_rows, "ffirow"), (:fits_delete_rows, "ffdrow"))
    @eval begin
        function ($a)(f::FITSFile, firstrow::Integer, nrows::Integer)
            fits_assert_open(f)
            status = Ref{Cint}(0)
            ccall(
                ($b, libcfitsio),
                Cint,
                (Ptr{Cvoid}, Int64, Int64, Ref{Cint}),
                f.ptr,
                firstrow,
                nrows,
                status,
            )
            fits_assert_ok(status[])
        end
    end
end

"""
    libcfitsio_version() -> VersionNumber

Return the version of the underlying CFITSIO library

# Example

```julia-repl
julia> libcfitsio_version()
v"3.37.0"
```

"""
function libcfitsio_version(version = fits_get_version())
    # fits_get_version returns a float. e.g., 3.341f0. We parse that
    # into a proper version number. E.g., 3.341 -> v"3.34.1"
    v = round(Int, 1000 * version)
    x = div(v, 1000)
    y = div(rem(v, 1000), 10)
    z = rem(v, 10)
    VersionNumber(x, y, z)
end

end # module
