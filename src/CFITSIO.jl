module CFITSIO
using CFITSIO_jll

export FITSFile,
    FITSMemoryHandle,
    fits_assert_open,
    fits_clobber_file,
    fits_close_file,
    fits_copy_data,
    fits_copy_image_section,
    fits_copy_file,
    fits_copy_hdu,
    fits_copy_header,
    fits_create_ascii_tbl,
    fits_create_binary_tbl,
    fits_create_tbl,
    fits_create_diskfile,
    fits_create_file,
    fits_create_empty_img,
    fits_create_img,
    fits_delete_col,
    fits_delete_file,
    fits_delete_key,
    fits_delete_record,
    fits_delete_rowlist,
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
    fits_get_img_param,
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
    fits_insert_col,
    fits_insert_cols,
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
    fits_read_key_lng,
    fits_read_key_str,
    fits_read_key_unit,
    fits_read_keyn,
    fits_read_keys_lng,
    fits_read_keyword,
    fits_read_pix,
    fits_read_pixnull,
    fits_read_record,
    fits_read_subset,
    fits_read_atblhdr,
    fits_read_btblhdr,
    fits_read_imghdr,
    fits_resize_img,
    fits_update_chksum,
    fits_update_key,
    fits_verify_chksum,
    fits_write_chksum,
    fits_write_col,
    fits_write_comment,
    fits_write_date,
    fits_write_history,
    fits_write_key,
    fits_write_key_unit,
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
@enum FileModeVerbose READONLY = Int(R) READWRITE = Int(RW)

const PREPEND_PRIMARY = -9 # copied from fitsio.h

"""
    cfitsio_typecode(::Type)::Cint

Return the CFITSIO type code for the given Julia type
"""
function cfitsio_typecode end

"""
    bitpix_from_type(::Type)::Cint

Return the FITS BITPIX code for the given Julia type
"""
function bitpix_from_type end

"""
    type_from_bitpix(::Integer)::Type

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
# The following block are all functions that have separate variants for Clong
# and 64-bit integers in cfitsio. Rather than providing both of these, we
# provide only one according to the native integer type on the platform.
if promote_type(Int, Clong) == Clong
    const Clong_or_Clonglong = Clong
    const ffgtdm = "ffgtdm"
    const ffgnrw = "ffgnrw"
    const ffptdm = "ffptdm"
    const ffgtcl = "ffgtcl"
    const ffeqty = "ffeqty"
    const ffgdes = "ffgdes"
    const ffgisz = "ffgisz"
    const ffgipr = "ffgipr"
    const ffghtb = "ffghtb"
    const ffghpr = "ffghpr"
    const ffghbn = "ffghbn"
else
    const Clong_or_Clonglong = Int64
    const ffgtdm = "ffgtdmll"
    const ffgnrw = "ffgnrwll"
    const ffptdm = "ffptdmll"
    const ffgtcl = "ffgtclll"
    const ffeqty = "ffeqtyll"
    const ffgdes = "ffgdesll"
    const ffgisz = "ffgiszll"
    const ffgipr = "ffgiprll"
    const ffghtb = "ffghtbll"
    const ffghpr = "ffghprll"
    const ffghbn = "ffghbnll"
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
        throw(ArgumentError("attempt to access a FITS file that has been closed previously"))
    end
end

function fits_assert_nonempty(f::FITSFile)
    if fits_get_num_hdus(f) == 0
        throw(ArgumentError("No HDU found in FITS file"))
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

tostring(v::Vector{UInt8}) = GC.@preserve v unsafe_string(pointer(v))

function checklength(v, expected_length, name)
    if length(v) < expected_length
        throw(ArgumentError("Expected $name to have length $expected_length, but got length $(length(v))"))
    end
end

fits_get_errstatus_buffer() = (; err_text = Vector{UInt8}(undef, FLEN_STATUS))
function fits_get_errstatus(status::Integer; err_text::Vector{UInt8} = fits_get_errstatus_buffer().err_text)
    checklength(err_text, FLEN_STATUS, "err_text")
    ccall((:ffgerr, libcfitsio), Cvoid, (Cint, Ptr{UInt8}), status, err_text)
    tostring(err_text)
end

fits_read_errmsg_buffer() = (; err_msg = Vector{UInt8}(undef, FLEN_ERRMSG))
function fits_read_errmsg(; err_msg::Vector{UInt8} = fits_read_errmsg_buffer().err_msg)
    checklength(err_msg, FLEN_ERRMSG, "err_msg")
    msgstr = ""
    ccall((:ffgmsg, libcfitsio), Cvoid, (Ptr{UInt8},), err_msg)
    msgstr = tostring(err_msg)
    errstr = msgstr
    while msgstr != ""
        ccall((:ffgmsg, libcfitsio), Cvoid, (Ptr{UInt8},), err_msg)
        msgstr = tostring(err_msg)
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
function fits_create_file end

"""
    fits_create_diskfile(filename::AbstractString)

Create and open a new empty output `FITSFile`. Unlike [`fits_create_file`](@ref), this function does
not use an extended filename parser and treats the string as is as the filename.
"""
function fits_create_diskfile end

for (f, fC) in ((:fits_create_file, "ffinit"), (:fits_create_diskfile, "ffdkinit"))
    @eval begin
        function ($f)(filename::AbstractString)
            ptr = Ref{Ptr{Cvoid}}()
            status = Ref{Cint}(0)
            ccall(
                ($fC, libcfitsio),
                Cint,
                (Ref{Ptr{Cvoid}}, Cstring, Ref{Cint}),
                ptr,
                filename,
                status,
            )
            fits_assert_ok(status[], filename)
            FITSFile(ptr[])
        end
    end
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
function fits_open_data end

"""
    fits_open_file(filename::String, [mode = 0])

Open an existing data file.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.READONLY` or `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.READWRITE` or `CFITSIO.RW`)

This function uses the extended filename syntax to open the file. See also [`fits_open_diskfile`](@ref)
that does not use the extended filename parser and uses `filename` as is as the name of the file.
"""
function fits_open_file end

"""
    fits_open_diskfile(filename::String, [mode = 0])

Open an existing data file.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.READONLY` or `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.READWRITE` or `CFITSIO.RW`)

This function does not use the extended filename parser, and uses `filename` as is as the name
of the file that is to be opened. See also [`fits_open_file`](@ref) which uses the extended filename syntax.
"""
function fits_open_diskfile end

"""
    fits_open_image(filename::String, [mode = 0])

Open an existing data file (like [`fits_open_file`](@ref)) and move to the first
HDU containing an image.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.READONLY` or `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.READWRITE` or `CFITSIO.RW`)
"""
function fits_open_image end

"""
    fits_open_table(filename::String, [mode = 0])

Open an existing data file (like [`fits_open_file`](@ref)) and move to the first
HDU containing either an ASCII or a binary table.

## Modes:
* 0 : Read only (equivalently denoted by `CFITSIO.READONLY` or `CFITSIO.R`)
* 1 : Read-write (equivalently denoted by `CFITSIO.READWRITE` or `CFITSIO.RW`)
"""
function fits_open_table end

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
                (Ref{Ptr{Cvoid}}, Cstring, Cint, Ref{Cint}),
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

fits_file_name_buffer() = (; filename = Vector{UInt8}(undef, FLEN_FILENAME))
"""
    fits_file_name(f::FITSFile)

Return the name of the file associated with object `f`.
"""
function fits_file_name(f::FITSFile; filename::Vector{UInt8} = fits_file_name_buffer().filename)
    checklength(filename, FLEN_FILENAME, "filename")
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffflnm, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        filename,
        status,
    )
    fits_assert_ok(status[])
    tostring(filename)
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

fits_read_key_str_buffer_value() = Vector{UInt8}(undef, FLEN_VALUE)
fits_read_key_str_buffer_comment() = Vector{UInt8}(undef, FLEN_COMMENT)
fits_read_key_str_buffer() = (; value = fits_read_key_str_buffer_value(),
                                comment = fits_read_key_str_buffer_comment(),
                            )
function fits_read_key_str(f::FITSFile, keyname::String;
            value::Vector{UInt8} = fits_read_key_str_buffer_value(),
            comment::Union{Vector{UInt8}, Nothing} = fits_read_key_str_buffer_comment(),
        )
    checklength(value, FLEN_VALUE, "value")
    isnothing(comment) || checklength(comment, FLEN_COMMENT, "comment")
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkys, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
    tostring(value), isnothing(comment) ? nothing : tostring(comment)
end

fits_read_key_lng_buffer() = (; comment = fits_read_key_str_buffer_comment())
function fits_read_key_lng(f::FITSFile, keyname::String;
        comment::Union{Vector{UInt8}, Nothing} = fits_read_key_lng_buffer().comment)
    isnothing(comment) || checklength(comment, FLEN_COMMENT, "comment")
    fits_assert_open(f)
    value = Ref{Clong}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkyj, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Ref{Clong}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
    value[], isnothing(comment) ? nothing : tostring(comment)
end

fits_read_keys_lng_buffer(nmax, nstart) = (; value = Vector{Clong}(undef, nmax - nstart + 1))
function fits_read_keys_lng(f::FITSFile, keyname::String, nstart::Integer, nmax::Integer;
        value::Vector{Clong} = fits_read_keys_lng_buffer(nmax, nstart).value)
    checklength(value, nmax - nstart + 1, "value")
    fits_assert_open(f)
    nfound = Ref{Cint}(0)
    status = Ref{Cint}(0)
    ccall(
        (:ffgknj, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Cint, Cint, Ptr{Clong}, Ref{Cint}, Ref{Cint}),
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

fits_read_keyword_buffer_value() = fits_read_key_str_buffer_value()
fits_read_keyword_buffer_comment() = fits_read_key_str_buffer_comment()
fits_read_keyword_buffer() = (; value = fits_read_keyword_buffer_value(),
                                comment = fits_read_keyword_buffer_comment(),
                                )
"""
    fits_read_keyword(f::FITSFile, keyname::String) -> (value, comment)

yields the specified keyword value and commend (as a tuple of strings),
throws and error if the keyword is not found.
"""
function fits_read_keyword(f::FITSFile, keyname::String;
            value::Vector{UInt8} = fits_read_keyword_buffer_value(),
            comment::Union{Vector{UInt8}, Nothing} = fits_read_keyword_buffer_comment(),
        )
    checklength(value, FLEN_VALUE, "value")
    isnothing(comment) || checklength(comment, FLEN_COMMENT, "comment")
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkey, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        value,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
    tostring(value), isnothing(comment) ? nothing : tostring(comment)
end

fits_read_record_buffer() = (; card = Vector{UInt8}(undef, FLEN_CARD))
"""
    fits_read_record(f::FITSFile, keynum::Int)::String

Return the nth header record in the CHU. The first keyword in the
header is at `keynum = 1`.
"""
function fits_read_record(f::FITSFile, keynum::Integer;
        card::Vector{UInt8} = fits_read_record_buffer().card,
        )
    checklength(card, FLEN_CARD, "card")
    fits_assert_open(f)
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

# CFITSIO follows the ESO HIERARCH convention where
# keyword names may be longer than 8 characters (which is the FITS standard)
# https://heasarc.gsfc.nasa.gov/fitsio/c/f_user/node28.html
fits_read_keyn_buffer_keyname() = Vector{UInt8}(undef, FLEN_KEYWORD)
fits_read_keyn_buffer_value() = fits_read_key_str_buffer_value()
fits_read_keyn_buffer_comment() = fits_read_key_str_buffer_comment()
fits_read_keyn_buffer() = (;
    keyname = fits_read_keyn_buffer_keyname(),
    value = fits_read_keyn_buffer_value(),
    comment = fits_read_keyn_buffer_comment(),
    )
"""
    fits_read_keyn(f::FITSFile, keynum::Int) -> (name, value, comment)

Return the nth header record in the CHU. The first keyword in the header is at `keynum = 1`.
"""
function fits_read_keyn(f::FITSFile, keynum::Integer;
            keyname::Vector{UInt8} = fits_read_keyn_buffer_keyname(),
            value::Vector{UInt8} = fits_read_keyn_buffer_value(),
            comment::Union{Vector{UInt8}, Nothing} = fits_read_keyn_buffer_comment(),
        )
    checklength(keyname, FLEN_KEYWORD, "keyname")
    checklength(value, FLEN_VALUE, "value")
    isnothing(comment) || checklength(comment, FLEN_COMMENT, "comment")
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffgkyn, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keynum,
        keyname,
        value,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
    (
        tostring(keyname),
        tostring(value),
        isnothing(comment) ? nothing : tostring(comment),
    )
end

fits_read_key_unit_buffer() = (; unit = fits_read_key_str_buffer_comment())

"""
    fits_read_key_unit(f::FITSFile, keyname::String)

Read the physical unit of the keyword `keyname` in the header.
"""
function fits_read_key_unit(f::FITSFile, keyname::String;
        unit::Vector{UInt8} = fits_read_key_unit_buffer().unit,
        )
    checklength(unit, FLEN_COMMENT, "unit")
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffgunt, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        keyname,
        unit,
        status,
    )
    fits_assert_ok(status[])
    return tostring(unit)
end

"""
    fits_read_atblhdr(f::FITSFile, maxdim::Integer = 99)

Read the header of an ASCII table HDU,
where `maxdim` represents the maximum number of columns to read.
The function returns the length of a row in bytes, the number of
rows, the number of columns, the column names as a `Vector{String}`, the byte offsets
to each column, the TFORMn values as a `Vector{String}`, the TUNITn values as a `Vector{String}`, and the `EXTNAME::String`
keyword, if any.
"""
function fits_read_atblhdr end

"""
    fits_read_btblhdr(f::FITSFile, maxdim::Integer = 99)

Read the header of a binary table HDU,
where `maxdim` represents the maximum number of columns to read.
The function returns the number of rows, the number of columns,
the column names as a `Vector{String}`, the TFORMn values  as a `Vector{String}`,
the TUNITn values as a `Vector{String}`, and the `EXTNAME::String` and `PCOUNT::Int`
keywords.
"""
function fits_read_btblhdr end

"""
    fits_read_imghdr(f::FITSFile, maxdim::Integer = 99)

Read the header of an image HDU,
where `maxdim` represents the maximum number of dimensions to read.
By default, `maxdim == 99` will read the size along every dimension of the image.
The function returns the values of `SIMPLE::Bool`,
`BITPIX::Int`, `NAXIS::Int`, `NAXES::Vector{Int}`,
`PCOUNT::Int`, `GCOUNT::Int`, and `EXTEND::Bool`.
The length of `NAXES` is set equal to `min(NAXIS, maxdim)`.

The `BITPIX` value indicates the data type of the image, and
it may be converted to a Julia type using the [`type_from_bitpix`](@ref) function.

!!! note
    `PCOUNT` is typically `0` for image HDUs, and `GCOUNT` is typically `1` for modern files.
"""
function fits_read_imghdr end

fits_read_atblhdr_buffer_ttype(maxdim) = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim]
fits_read_atblhdr_buffer_tform(maxdim) = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim]
fits_read_atblhdr_buffer_tunit(maxdim) = [Vector{UInt8}(undef, FLEN_VALUE) for _ in 1:maxdim]
fits_read_atblhdr_buffer_extname() = Vector{UInt8}(undef, FLEN_VALUE)
fits_read_atblhdr_buffer(maxdim) = (;
    ttype = fits_read_atblhdr_buffer_ttype(maxdim),  # name of each column
    tform = fits_read_atblhdr_buffer_tform(maxdim),  # value of TFORMn keyword for each column (datatype code as string)
    tunit = fits_read_atblhdr_buffer_tunit(maxdim),  # value of TUNITn keyword for each column
    extname = fits_read_atblhdr_buffer_extname(),    # value of EXTNAME keyword, if any
    tbcol = fits_read_atblhdr_buffer_tbcol(maxdim),  # byte offset of each column
)

fits_read_btblhdr_buffer_ttype(maxdim) = fits_read_atblhdr_buffer_ttype(maxdim)
fits_read_btblhdr_buffer_tform(maxdim) = fits_read_atblhdr_buffer_tform(maxdim)
fits_read_btblhdr_buffer_tunit(maxdim) = fits_read_atblhdr_buffer_tunit(maxdim)
fits_read_btblhdr_buffer_extname() = fits_read_atblhdr_buffer_extname()
fits_read_btblhdr_buffer(maxdim) = (;
    ttype = fits_read_btblhdr_buffer_ttype(maxdim),
    tform = fits_read_btblhdr_buffer_tform(maxdim),
    tunit = fits_read_btblhdr_buffer_tunit(maxdim),
    extname = fits_read_btblhdr_buffer_extname(),
)

@eval begin
    fits_read_imghdr_buffer(maxdim) = (; naxes = Vector{$Clong_or_Clonglong}(undef, maxdim))
    function fits_read_imghdr(f::FITSFile, maxdim::Integer = 99;
            naxes::Union{Vector{$Clong_or_Clonglong}, Nothing} = fits_read_imghdr_buffer(maxdim).naxes,
        )
        fits_assert_open(f)
        if !isnothing(naxes)
            ndim = fits_get_img_dim(f)
            checklength(naxes, ndim, "naxes")
        end
        status = Ref{Cint}(0)
        simple = Ref{Cint}(0) # does file conform to FITS standard? 1/0
        bitpix = Ref{Cint}(0) # number of bits per data value pixel
        naxis = Ref{Cint}(0) # number of axes in the data array
        pcount = Ref{Clong}(0) # number of group parameters (usually 0)
        gcount = Ref{Clong}(0) # number of random groups (usually 1 or 0)
        extend = Ref{Cint}(0) # may FITS file have extensions?
        ccall(
            (ffghpr, libcfitsio),
            Cint,
            (Ptr{Cvoid} #= f.ptr =#,
                Cint #= maxdim =#,
                Ref{Cint} #= simple =#,
                Ref{Cint} #= bitpix =#,
                Ref{Cint} #= naxis =#,
                Ptr{$Clong_or_Clonglong} #= naxes =#,
                Ref{Clong} #= pcount =#,
                Ref{Clong} #= gcount =#,
                Ref{Cint} #= extend =#,
                Ref{Cint} #= status =#,
                ),
            f.ptr,
            maxdim,
            simple,
            bitpix,
            naxis,
            ifelse(isnothing(naxes), C_NULL, naxes),
            pcount,
            gcount,
            extend,
            status
        )
        fits_assert_ok(status[])
        if !isnothing(naxes)
            naxes = naxes[1:min(end, naxis[])]
        end
        return Bool(simple[]), Int(bitpix[]), Int(naxis[]),
                naxes, Int(pcount[]), Int(gcount[]), Bool(extend[])
    end
    fits_read_atblhdr_buffer_tbcol(maxdim) = Vector{$Clong_or_Clonglong}(undef, maxdim)
    function fits_read_atblhdr(f::FITSFile, maxdim::Integer = 99;
            ttype::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_atblhdr_buffer_ttype(maxdim),
            tform::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_atblhdr_buffer_tform(maxdim),
            tunit::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_atblhdr_buffer_tunit(maxdim),
            extname::Union{Vector{UInt8}, Nothing} = fits_read_atblhdr_buffer_extname(),
            tbcol::Union{Vector{$Clong_or_Clonglong}, Nothing} = fits_read_atblhdr_buffer_tbcol(maxdim),
        )
        isnothing(ttype) || checklength(ttype, maxdim, "ttype")
        isnothing(tform) || checklength(tform, maxdim, "tform")
        isnothing(tunit) || checklength(tunit, maxdim, "tunit")
        isnothing(extname) || checklength(extname, FLEN_VALUE, "extname")
        isnothing(tbcol) || checklength(tbcol, maxdim, "tbcol")
        fits_assert_open(f)
        status = Ref{Cint}(0)
        rowlen = Ref{$Clong_or_Clonglong}(0) # length of table row in bytes
        nrows = Ref{$Clong_or_Clonglong}(0) # number of rows in the table
        tfields = Ref{Cint}(0) # number of columns in the table
        ccall(
            (ffghtb, libcfitsio),
            Cint,
            (Ptr{Cvoid} #= f.ptr =#,
                Cint #= maxdim =#,
                Ref{$Clong_or_Clonglong} #= rowlen =#,
                Ref{$Clong_or_Clonglong} #= nrows =#,
                Ref{Cint} #= tfields =#,
                Ptr{Ptr{UInt8}} #= ttype =#,
                Ptr{$Clong_or_Clonglong} #= tbcol =#,
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
            ifelse(isnothing(ttype), C_NULL, ttype),
            ifelse(isnothing(tbcol), C_NULL, tbcol),
            ifelse(isnothing(tform), C_NULL, tform),
            ifelse(isnothing(tunit), C_NULL, tunit),
            ifelse(isnothing(extname), C_NULL, extname),
            status
        )
        fits_assert_ok(status[])
        ncols = Int(tfields[])
        ttypestr = isnothing(ttype) ? nothing : map(tostring, @view ttype[1:min(end, ncols)])
        tformstr = isnothing(tform) ? nothing : map(tostring, @view tform[1:min(end, ncols)])
        tunitstr = isnothing(tunit) ? nothing : map(tostring, @view tunit[1:min(end, ncols)])
        tbcolInt = isnothing(tbcol) ? nothing : convert(Vector{Int}, @view tbcol[1:min(end, ncols)])
        extnamestr = isnothing(extname) ? nothing : tostring(extname)
        return Int(rowlen[]), Int(nrows[]), ncols,
            ttypestr, tbcolInt, tformstr, tunitstr, extnamestr
    end
    function fits_read_btblhdr(f::FITSFile, maxdim::Integer = 99;
            ttype::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_btblhdr_buffer_ttype(maxdim),
            tform::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_btblhdr_buffer_tform(maxdim),
            tunit::Union{Vector{Vector{UInt8}}, Nothing} = fits_read_btblhdr_buffer_tunit(maxdim),
            extname::Union{Vector{UInt8}, Nothing} = fits_read_btblhdr_buffer_extname(),
        )
        isnothing(ttype) || checklength(ttype, maxdim, "ttype")
        isnothing(tform) || checklength(tform, maxdim, "tform")
        isnothing(tunit) || checklength(tunit, maxdim, "tunit")
        isnothing(extname) || checklength(extname, FLEN_VALUE, "extname")
        fits_assert_open(f)
        status = Ref{Cint}(0)
        nrows = Ref{$Clong_or_Clonglong}(0) # number of rows in the table
        tfields = Ref{Cint}(0) # number of columns in the table
        pcount = Ref{$Clong_or_Clonglong}(0) # value of PCOUNT keyword
        ccall(
            (ffghbn, libcfitsio),
            Cint,
            (Ptr{Cvoid} #= f.ptr =#,
                Cint #= maxdim =#,
                Ref{$Clong_or_Clonglong} #= nrows =#,
                Ref{Cint} #= tfields =#,
                Ptr{Ptr{UInt8}} #= ttype =#,
                Ptr{Ptr{UInt8}} #= tform =#,
                Ptr{Ptr{UInt8}} #= tunit =#,
                Ptr{UInt8} #= extname =#,
                Ref{$Clong_or_Clonglong} #= pcount =#,
                Ref{Cint} #= status =#,
                ),
            f.ptr,
            maxdim,
            nrows,
            tfields,
            ifelse(isnothing(ttype), C_NULL, ttype),
            ifelse(isnothing(tform), C_NULL, tform),
            ifelse(isnothing(tunit), C_NULL, tunit),
            ifelse(isnothing(extname), C_NULL, extname),
            pcount,
            status
        )
        fits_assert_ok(status[])
        ncols = Int(tfields[])
        ttypestr = isnothing(ttype) ? nothing : map(tostring, @view ttype[1:min(end, ncols)])
        tformstr = isnothing(tform) ? nothing : map(tostring, @view tform[1:min(end, ncols)])
        tunitstr = isnothing(tunit) ? nothing : map(tostring, @view tunit[1:min(end, ncols)])
        extnamestr = isnothing(extname) ? nothing : tostring(extname)
        return Int(nrows[]), ncols,
            ttypestr, tformstr, tunitstr, extnamestr, Int(pcount[])
    end
end

"""
    fits_write_key(f::FITSFile, keyname::String, value, comment::Union{String, Nothing} = nothing)

Write a keyword of the appropriate data type into the CHU.
If `comment` is `nothing`, the keyword is written without a comment.
"""
function fits_write_key(
    f::FITSFile,
    keyname::String,
    value::Union{Real,String},
    comment::Union{String, Nothing} = nothing,
    )

    fits_assert_open(f)
    fits_assert_isascii(keyname)
    !isnothing(comment) && fits_assert_isascii(comment)
    cvalue = isa(value, String) ? value :
        isa(value, Bool) ? Cint[value] : reinterpret(UInt8, [value])
    status = Ref{Cint}(0)
    ccall(
        (:ffpky, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cstring, Ptr{UInt8}, Cstring, Ref{Cint}),
        f.ptr,
        cfitsio_typecode(typeof(value)),
        keyname,
        cvalue,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_key_unit(f::FITSFile, keyname::String, unit::String)

Write the physical units string into an existing keyword record.
The keyword must already exist in the header.
The unit string is enclosed in square brackets at the beginning of the keyword comment field.
"""
function fits_write_key_unit(f::FITSFile, keyname::String, unit::String)
    fits_assert_open(f)
    fits_assert_isascii(keyname)
    fits_assert_isascii(unit)
    status = Ref{Cint}(0)
    ccall(
        (:ffpunt, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Cstring, Ref{Cint}),
        f.ptr,
        keyname,
        unit,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_write_date(f::FITSFile)

Write the current date and time into the FITS header. If a DATE keyword already
exists, it is replaced by the new value. The date is written in the format
`YYYY-MM-DDThh:mm:ss` (ISO 8601).
"""
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
        (Ptr{Cvoid}, Cstring, Ref{Cint}),
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
        (Ptr{Cvoid}, Cstring, Ref{Cint}),
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
            comment::Union{String,Ptr{Nothing},Nothing} = nothing,
            )

            fits_assert_open(f)
            isa(value, String) && fits_assert_isascii(value)
            isa(comment, String) && fits_assert_isascii(comment)
            status = Ref{Cint}(0)
            ccall(
                ($a, libcfitsio),
                Cint,
                (Ptr{Cvoid}, Cstring, $S, Ptr{UInt8}, Ref{Cint}),
                f.ptr,
                key,
                value,
                ifelse(isnothing(comment), C_NULL, comment),
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
        comment::Union{String,Ptr{Nothing},Nothing} = nothing,
    )

    fits_assert_open(f)
    isa(comment, String) && fits_assert_isascii(comment)
    status = Ref{Cint}(0)
    ccall(
        ("ffukyd", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Cdouble, Cint, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        key,
        value,
        -15,
        ifelse(isnothing(comment), C_NULL, comment),
        status,
    )
    fits_assert_ok(status[])
end

function fits_update_key(
        f::FITSFile,
        key::String,
        value::Nothing,
        comment::Union{String,Ptr{Nothing},Nothing} = nothing,
    )

    fits_assert_open(f)
    isa(comment, String) && fits_assert_isascii(comment)
    status = Ref{Cint}(0)
    ccall(
        ("ffukyu", libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cstring, Ptr{UInt8}, Ref{Cint}),
        f.ptr,
        key,
        ifelse(isnothing(comment), C_NULL, comment),
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
        (Ptr{Cvoid}, Cstring, Ref{Cint}),
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
        (Ptr{Cvoid}, Cstring, Ref{Cint}),
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
    fits_free_memory(header[])
    fits_assert_ok(status[])
    result
end

"""
    fits_copy_header(fin::FITSFile, fout::FITSFile)

Copy the header (not the data) associated with the current HDU from `fin` to `fout`.
If the current HDU in `fout` is not empty, it will be closed and a new HDU will be appended.
An empty output HDU will be created with the header but no data.
"""
function fits_copy_header(fin::FITSFile, fout::FITSFile)
    fits_assert_open(fin)
    fits_assert_open(fout)
    status = Ref{Cint}(0)
    ccall(
        (:ffcphd, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, Ref{Cint}),
        fin.ptr,
        fout.ptr,
        status,
    )
    fits_assert_ok(status[])
end

# -----------------------------------------------------------------------------
# HDU info functions and moving the current HDU
@enum HDUType IMAGE_HDU=0 ASCII_TBL=1 BINARY_TBL=2 ANY_HDU=-1
function hdu_int_to_type(hdu_type_int)
    if hdu_type_int == Int(IMAGE_HDU)
        return :image_hdu
    elseif hdu_type_int == Int(ASCII_TBL)
        return :ascii_table
    elseif hdu_type_int == Int(BINARY_TBL)
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
        (Ptr{Cvoid}, Cint, Cstring, Cint, Ref{Cint}),
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

Return the size along each dimension in the current Image HDU.

See also [`fits_get_img_type`](@ref), [`fits_get_img_dim`](@ref) and [`fits_get_img_param`](@ref).
"""
function fits_get_img_size end

"""
    fits_get_img_param(f::FITSFile)

Return the bitpix, number of dimensions and the size along each dimension of the current
image HDU.

See also [`fits_get_img_type`](@ref), [`fits_get_img_dim`](@ref) and [`fits_get_img_size`](@ref).
"""
function fits_get_img_param end

"""
    fits_get_img_dim(f::FITSFile)

Return the number of dimensions in the current image HDU.

See also [`fits_get_img_type`](@ref), [`fits_get_img_size`](@ref) and [`fits_get_img_param`](@ref).
"""
function fits_get_img_dim end

"""
    fits_get_img_type(f::FITSFile)

Return the datatype (bitpix) of the current image HDU. This may be converted to a Julia type by using
the function [`type_from_bitpix`](@ref).
"""
function fits_get_img_type end

for (a, b) in (
        (:fits_get_img_type, "ffgidt"),
        (:fits_get_img_equivtype, "ffgiet"),
        (:fits_get_img_dim, "ffgidm"),
    )

    @eval function ($a)(f::FITSFile)
        fits_assert_open(f)
        bitpix = Ref{Cint}(0)
        status = Ref{Cint}(0)
        ccall(
            ($b, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}),
            f.ptr,
            bitpix,
            status,
        )
        fits_assert_ok(status[])
        bitpix[]
    end
end

"""
    fits_create_empty_img(f::FITSFile)

Create an empty image HDU with no dimensions, and of type `Int`.
See [`fits_create_img`](@ref).
"""
fits_create_empty_img(f::FITSFile) = fits_create_img(f, Int, C_NULL)

"""
    fits_create_img(f::FITSFile, T::Type, naxes::Vector{<:Integer})

Create a new primary array or IMAGE extension with the specified data type `T` and size `naxes`.
"""
function fits_create_img(f::FITSFile, ::Type{T}, naxes::Union{Vector{<:Integer}, Ptr{Cvoid}}) where {T}
    fits_assert_open(f)
    status = Ref{Cint}(0)
    N = naxes === C_NULL ? 0 : length(naxes)
    naxesr = naxes === C_NULL ? C_NULL : convert(Vector{Int64}, naxes)
    ccall(
        (:ffcrimll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{Int64}, Ref{Cint}),
        f.ptr,
        bitpix_from_type(T),
        N,
        naxesr,
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
                    naxes::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}}; prepend_primary::Bool = false)

Insert a new image extension immediately following the current HDU (CHDU), or insert a new primary array
at the beginning of the file.

A new primary array may be inserted at the beginning of the FITS file by calling `fits_insert_img` with
`prepend_primary` set to `true`. In this case, the existing primary HDU is converted to an image extension,
and the new primary array will become the CHDU.

The inserted array has an eltype `T` and size `naxes`.

    fits_insert_img(f::FITSFile, a::AbstractArray{<:Real}; prepend_primary::Bool = false)

Insert a new image HDU with an element type of `eltype(a)` and a size of `size(a)` that is capable
of storing the array `a`. The flag `prepend_primary` may be specified to insert a new primary array at the
beginning of the FITS file.
"""
function fits_insert_img(f::FITSFile, T::Type, naxes::Vector{<:Integer}; prepend_primary::Bool = false)
    fits_assert_open(f)

    status = Ref{Cint}(prepend_primary ? PREPEND_PRIMARY : 0)
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

function fits_insert_img(f::FITSFile, T::Type, naxes::NTuple{N,Integer}; prepend_primary::Bool = false) where {N}
    fits_assert_open(f)

    status = Ref{Cint}(prepend_primary ? PREPEND_PRIMARY : 0)
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

fits_insert_img(f::FITSFile, a::AbstractArray{<:Real}; prepend_primary::Bool = false) = fits_insert_img(f, eltype(a), size(a); prepend_primary=prepend_primary)

"""
    fits_copy_file(fin::FITSFile, fout::FITSFile, previous::Bool, current::Bool, following::Bool)

Copy all or a part of the HDUs from the input file `fin`,
and append them to the output file `fout`.
The flags `previous`, `current` and `following` specify which HDUs are to be copied.

* If `previous` is true, all the HDUs prior to the current input HDU are copied.
* If `current` is true, the current input HDU is copied.
* If `following` is true, all the HDUs following the current input HDU are copied.

These flags may be combined, so if all are set to `true` then all the HDUs are copied from
`fin` to `fout`.

On exit, the input is unchanged, and the last HDU in the output is set as the current HDU.
"""
function fits_copy_file(fin::FITSFile, fout::FITSFile,
    previous::Bool, current::Bool, following::Bool,
    )

    fits_assert_open(fin)
    fits_assert_open(fout)

    status = Ref{Cint}(0)

    ccall(
        (:ffcpfl, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Ptr{Cvoid},
            Cint,
            Cint,
            Cint,
            Ref{Cint},
        ),
        fin.ptr,
        fout.ptr,
        previous,
        current,
        following,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_copy_hdu(fin::FITSFile, fout::FITSFile[, morekeys::Integer = 0])

Copy the current HDU from the input file `fin` and append it to the output file `fout`.
Space may be reserved for `morekeys` additional keywords in the output header.
"""
function fits_copy_hdu(fin::FITSFile, fout::FITSFile, morekeys::Integer = 0)

    fits_assert_open(fin)
    fits_assert_open(fout)

    status = Ref{Cint}(0)

    ccall(
        (:ffcopy, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Ptr{Cvoid},
            Cint,
            Ref{Cint},
        ),
        fin.ptr,
        fout.ptr,
        morekeys,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_copy_data(fin::FITSFile, fout::FITSFile)

Copy the data (not the header) from the current HDU in `fin` to the current HDU in `fout`.
This will overwrite pre-existing data in the output HDU.
"""
function fits_copy_data(fin::FITSFile, fout::FITSFile)
    fits_assert_open(fin)
    fits_assert_open(fout)

    status = Ref{Cint}(0)

    ccall(
        (:ffcpdt, libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Ptr{Cvoid},
            Ref{Cint},
        ),
        fin.ptr,
        fout.ptr,
        status,
    )
    fits_assert_ok(status[])
end

# check if the array is contiguous in memory
iscontiguous(array::Union{Array,StridedArray{<:Any,0}}) = true
function iscontiguous(array)
    strd = strides(array)
    sz = size(array)
    isone(strd[1]) && check_contiguous(Base.tail(strd),sz[1],Base.tail(sz)...)
end
assert_contiguous(array) = iscontiguous(array) || throw(ArgumentError("array is not contiguous in memory"))

function check_contiguous(strd,s,d,sz...)
    strd[1] == s && check_contiguous(Base.tail(strd),s*d,sz...)
end
check_contiguous(::Tuple{},args...) = true

function checkndims(pixel, ndim)
    if length(pixel) < ndim
        throw(ArgumentError("number of pixels $(length(pixel)) is less than the number of dimensions $ndim"))
    end
end

function check_contiguous_and_length(data, nelements)
    assert_contiguous(data)
    length(data) >= nelements ||
        throw(ArgumentError("data must have at least nelements=$nelements elements"))
end

function validate_image_size(f::FITSFile, nel)
    # check that the required keywords exist in the header
    # this is necessary if the HDU has just been created, and
    # has not been written to disk yet
    # this is a guardrail against writing to an empty fits file
    ndim = fits_get_img_dim(f)
    ndim > 0 || throw(ArgumentError("HDU has no dimensions"))
    sz = fits_get_img_size(f)
    prod(sz) >= nel ||
        throw(ArgumentError("HDU size $sz is smaller the number of elements to write $nel"))
end
function _CartesianIndex(fpixel, ::Val{N}) where {N}
    # redundant trailing indices may be ignored
    trailing_inds = if fpixel isa AbstractVector
        @view(fpixel[N+1:end])
    else
        fpixel[N+1:end]
    end
    all(isone, trailing_inds) ||
            throw(ArgumentError("trailing indices are not 1"))
    CartesianIndex(ntuple(i->fpixel[i], N))
end
function check_data_bounds(data, fpixel::Union{AbstractVector, Tuple}, nelements::Integer)
    firstind = _CartesianIndex(fpixel, Val(ndims(data)))
    linind = LinearIndices(data)
    checkbounds(data, range(linind[firstind], length=nelements))
end
function check_data_bounds(data, fpixel::Union{AbstractVector, Tuple}, lpixel::Union{AbstractVector, Tuple})
    firstind = _CartesianIndex(fpixel, Val(ndims(data)))
    lastind = _CartesianIndex(lpixel, Val(ndims(data)))
    checkbounds(data, firstind:lastind)
end

"""
    fits_write_pix(f::FITSFile,
                   fpixel::Union{Vector{<:Integer}, Tuple{Vararg{Integer}}},
                   nelements::Integer, data::StridedArray)

Write `nelements` pixels from `data` into the FITS file starting from the pixel `fpixel`.

!!! note
    The HDU must have been created previously, and its size
    must match the number of elements being written.

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

    check_data_bounds(data, fpixel, nelements)
    assert_contiguous(data)
    fits_assert_open(f)
    validate_image_size(f, nelements)

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

    check_data_bounds(data, fpixel, nelements)
    assert_contiguous(data)
    fits_assert_open(f)
    validate_image_size(f, nelements)

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
    The HDU must have been created previously, and its size
    must match the number of elements being written.

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
    The HDU must have been created previously, and its size
    must match the number of elements being written.

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

    check_data_bounds(data, fpixel, nelements)
    assert_contiguous(data)
    fits_assert_open(f)
    validate_image_size(f, nelements)

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

    check_data_bounds(data, fpixel, nelements)
    assert_contiguous(data)
    fits_assert_open(f)
    validate_image_size(f, nelements)
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
    The HDU must have been created previously, and its size
    must match the number of elements being written.

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
    The HDU must have been created previously, and its size
    must match the number of elements being written.

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

    check_data_bounds(data, fpixel, lpixel)
    assert_contiguous(data)
    fits_assert_open(f)
    nelements = prod(((f,l),) -> length(f:l), zip(fpixel, lpixel))
    validate_image_size(f, nelements)

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

    check_data_bounds(data, fpixel, lpixel)
    assert_contiguous(data)
    fits_assert_open(f)
    nelements = prod(((f,l),) -> length(f:l), zip(fpixel, lpixel))
    validate_image_size(f, nelements)

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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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
    fits_read_pix(f, ones(Int64, fits_get_img_dim(f)), length(data), data)
end

function fits_read_pix(f::FITSFile, data::StridedArray, nulval)
    fits_read_pix(f, ones(Int64, fits_get_img_dim(f)), length(data), nulval, data)
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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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

    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)

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
    fits_read_pixnull(f, ones(Int64, fits_get_img_dim(f)), length(data), data, nullarray)
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

    nelements = prod(((f,l,i),) -> length(f:i:l), zip(fpixel, lpixel, inc))
    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)
    checkndims(lpixel, ndim)
    checkndims(inc, ndim)

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

    nelements = prod(((l,f,i),) -> length(f:i:l), zip(lpixel, fpixel, inc))
    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)
    checkndims(lpixel, ndim)
    checkndims(inc, ndim)

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

    nelements = prod(((l,f,i),) -> length(f:i:l), zip(lpixel, fpixel, inc))
    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)
    checkndims(lpixel, ndim)
    checkndims(inc, ndim)

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

    nelements = prod(((l,f,i),) -> length(f:i:l), zip(lpixel, fpixel, inc))
    check_contiguous_and_length(data, nelements)
    fits_assert_open(f)
    fits_assert_nonempty(f)
    ndim = fits_get_img_dim(f)
    checkndims(fpixel, ndim)
    checkndims(lpixel, ndim)
    checkndims(inc, ndim)

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

    status = Ref{Cint}(0)
    ccall(
        (:fits_copy_image_section, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ref{Cint}),
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
                           coldefs::Union{Array{NTuple{3,String}}, Array{NTuple{2,String}}},
                           extname::Union{String, Nothing} = nothing)

Append a new HDU containing a binary table. The meaning of the parameters is the same
as in a call to [`fits_create_ascii_tbl`](@ref).

In general, one should pick this function for creating tables in a new HDU,
as binary tables require less space on the disk and are more efficient to read and write.
(Moreover, a few datatypes are not supported in ASCII tables).
"""
function fits_create_binary_tbl end

"""
    fits_create_ascii_tbl(f::FITSFile, numrows::Integer,
                          coldefs::Union{Array{NTuple{3,String}}, Array{NTuple{2,String}}},
                          extname::Union{String, Nothing} = nothing)

Append a new HDU containing an ASCII table.

The table will have `numrows` rows (this parameter can be set to zero), each
initialized with the default value. In order to create a table, the programmer
must specify the characteristics of each column. The columns are specified by the
`coldefs` variable, which is an array of tuples.
Each tuple must have two or three string fields:

1. The name of the column.
2. The data type and the repetition count. It must be a string made by a number
   (the repetition count) followed by a letter specifying the type (in the example
   above, `D` stands for `Float64`, `E` stands for `Float32`, `A` stands for `Char`).
   Refer to the CFITSIO documentation for more information about the syntax of this
   parameter.
3. The unit of this field. This is used to set the corresponding `TUNITn` keywords.
   If `coldefs` is a two-tuple, the unit keywords are left unset.
   If the third field of a tuple is an empty string,
   the corresponding unit keyword is also left unset.

The value of `extname` sets the "extended name" of the table, i.e., a string
that in some situations can be used to refer to the HDU itself. This may be omitted by setting
`extname` to `nothing` (which is the default behavior).

Note that, unlike for binary tables, CFITSIO puts some limitations to the
types that can be used in an ASCII table column. Refer to the CFITSIO manual
for further information.

See also [`fits_create_binary_tbl`](@ref) for a similar function which
creates binary tables. In general, one should pick this function for creating tables in a new HDU,
as binary tables require less space on the disk and are more efficient to read and write.
(Moreover, a few datatypes are not supported in ASCII tables).
"""
function fits_create_ascii_tbl end

for (a, b) in ((:fits_create_binary_tbl, BINARY_TBL), (:fits_create_ascii_tbl, ASCII_TBL))
    @eval begin
        function ($a)(
            f::FITSFile,
            numrows::Integer,
            coldefs::Union{Array{NTuple{3,String}}, Array{NTuple{2,String}}},
            extname::Union{String, Nothing} = nothing,
            )

            ttype = getindex.(coldefs, 1)
            tform = getindex.(coldefs, 2)
            tunit = coldefs isa Array{NTuple{3,String}} ? getindex.(coldefs, 3) : nothing
            $a(f, numrows, ttype, tform, tunit, extname)
        end
        function ($a)(
                f::FITSFile,
                numrows::Integer,
                ttype::Vector{String},
                tform::Vector{String},
                tunit::Union{Vector{String}, Nothing} = nothing,
                extname::Union{String, Nothing} = nothing,
                )

            fits_create_tbl(
                f,
                $b,
                numrows,
                ttype,
                tform,
                tunit,
                extname,
            )
        end
    end
end

function fits_create_tbl(f::FITSFile, tbltype, numrows::Integer,
        ttype::Vector{String}, tform::Vector{String},
        tunit::Union{Vector{String}, Nothing} = nothing,
        extname::Union{String, Nothing} = nothing)

    Int(tbltype) in (Int(ASCII_TBL), Int(BINARY_TBL)) ||
        throw(ArgumentError("table type must be one of CFITSIO.ASCII_TBL or CFITSIO.BINARY_TBL"))
    tfields = length(ttype)
    if tfields != length(tform) || !(isnothing(tunit) || tfields == length(tunit))
        throw(ArgumentError("length of tform and tunit must match number of columns"))
    end
    fits_assert_open(f)
    # Ensure that extension name, column names and units are
    # ASCII, as these get written to the file. We don't check
    # need to check that tform is ASCII because presumably
    # cfitsio will thrown an appropriate error if it doesn't
    # recognize the tform string.
    all(fits_assert_isascii, ttype)
    isnothing(tunit) || all(fits_assert_isascii, tunit)
    isnothing(extname) || fits_assert_isascii(extname)
    status = Ref{Cint}(0)
    ccall(
        ("ffcrtb", libcfitsio),
        Cint,
        (
            Ptr{Cvoid},
            Cint,
            Clonglong,
            Cint,
            Ptr{Cstring},
            Ptr{Cstring},
            Ptr{Cstring},
            Cstring,
            Ref{Cint},
        ),
        f.ptr,
        tbltype,
        numrows,
        tfields,
        ttype,
        tform,
        ifelse(isnothing(tunit), C_NULL, tunit),
        ifelse(isnothing(extname), C_NULL, extname),
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_get_num_hdus(f::FITSFile)

Return the number of HDUs in the file.
"""
function fits_get_num_hdus end

"""
    fits_get_num_cols(f::FITSFile)

Return the number of columns in the current HDU.
"""
function fits_get_num_cols end

"""
    fits_get_rowsize(f::FITSFile)

Return the size of a row in the current HDU.
"""
function fits_get_rowsize end

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
        (Ptr{Cvoid}, Cint, Cstring, Ref{Cint}, Ref{Cint}),
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
        repcnt = Ref{$Clong_or_Clonglong}(0)
        width = Ref{$Clong_or_Clonglong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgtcl, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ref{Cint}, Ref{$Clong_or_Clonglong}, Ref{$Clong_or_Clonglong}, Ref{Cint}),
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
        repcnt = Ref{$Clong_or_Clonglong}(0)
        width = Ref{$Clong_or_Clonglong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffeqty, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ref{Cint}, Ref{$Clong_or_Clonglong}, Ref{$Clong_or_Clonglong}, Ref{Cint}),
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
        naxes = Vector{$Clong_or_Clonglong}(undef, ndim)
        status = Ref{Cint}(0)
        ccall(
            ($ffgisz, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ptr{$Clong_or_Clonglong}, Ref{Cint}),
            f.ptr,
            ndim,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        naxes
    end

    function fits_get_img_size(f::FITSFile, ::Val{N}) where {N}
        naxes = Ref(zerost($Clong_or_Clonglong, N))
        status = Ref{Cint}(0)
        ccall(
            ($ffgisz, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Ptr{NTuple{N,$Clong_or_Clonglong}}, Ref{Cint}),
            f.ptr,
            N,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        naxes[]
    end

    function fits_get_img_param(f::FITSFile)
        fits_assert_open(f)
        status = Ref{Cint}(0)
        bitpix = Ref{Cint}(0)
        ndim = fits_get_img_dim(f)
        naxes = Vector{$Clong_or_Clonglong}(undef, ndim)
        ccall(
            ($ffgipr, libcfitsio),
            Cint,
            (
                Ptr{Cvoid},
                Cint,
                Ref{Cint},
                Ptr{Cvoid},
                Ptr{Clonglong},
                Ref{Cint},
            ),
            f.ptr,
            ndim,
            bitpix,
            C_NULL,
            naxes,
            status,
        )
        fits_assert_ok(status[])
        return Int(bitpix[]), ndim, convert(Vector{Int}, naxes)
    end

    function fits_get_num_rows(f::FITSFile)
        fits_assert_open(f)
        result = Ref{$Clong_or_Clonglong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgnrw, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Ref{$Clong_or_Clonglong}, Ref{Cint}),
            f.ptr,
            result,
            status,
        )
        fits_assert_ok(status[])
        return Int(result[])
    end

    fits_read_tdim_buffer() = (; naxes = Vector{$Clong_or_Clonglong}(undef, 99))  # 99 is the maximum allowed number of axes
    # `fits_read_tdim` returns the dimensions of a table column in a
    # binary table. Normally this information is given by the TDIMn
    # keyword, but if this keyword is not present then this routine
    # returns `[r]` with `r` equals to the repeat count in the TFORM
    # keyword.
    function fits_read_tdim(ff::FITSFile, colnum::Integer;
            naxes::Vector{$Clong_or_Clonglong} = fits_read_tdim_buffer().naxes,
            )
        fits_assert_open(ff)
        naxis = Ref{Cint}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgtdm, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Cint, Ref{Cint}, Ptr{$Clong_or_Clonglong}, Ref{Cint}),
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

    function fits_write_tdim(ff::FITSFile, colnum::Integer, naxes::Array{$Clong_or_Clonglong})
        fits_assert_open(ff)
        status = Ref{Cint}(0)
        ccall(
            ($ffptdm, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Cint, Ptr{$Clong_or_Clonglong}, Ref{Cint}),
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
        repeat = Ref{$Clong_or_Clonglong}(0)
        offset = Ref{$Clong_or_Clonglong}(0)
        status = Ref{Cint}(0)
        ccall(
            ($ffgdes, libcfitsio),
            Cint,
            (Ptr{Cvoid}, Cint, Int64, Ref{$Clong_or_Clonglong}, Ref{$Clong_or_Clonglong}, Ref{Cint}),
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

    for i in eachindex(data, buffers)
        data[i] = tostring(buffers[i])
    end
    return data
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
    return data
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
function fits_insert_rows end

"""
    fits_delete_rows(f::FITSFile, firstrow::integer, nrows::Integer)

Delete `nrows` rows, starting from the one at position `firstrow`. The index of
the first row is 1.
"""
function fits_delete_rows end

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

function fits_delete_rowlist(f::FITSFile, rowlist::Vector{<:Integer})
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffdrwsll, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ptr{Clonglong}, Clonglong, Ref{Cint}),
        f.ptr,
        convert(Vector{Clonglong}, rowlist),
        length(rowlist),
        status,
    )
    fits_assert_ok(status[])
end

function fits_insert_col(f::FITSFile, colnum::Integer, ttype::String, tform::String)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:fficol, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Int64, Cstring, Cstring, Ref{Cint}),
        f.ptr,
        colnum,
        ttype,
        tform,
        status,
    )
    fits_assert_ok(status[])
end

function fits_insert_cols(f::FITSFile, colnum::Integer,
        ttype::Vector{String}, tform::Vector{String})
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ncols = length(ttype)
    if ncols != length(tform)
        throw(ArgumentError("length of ttype and tform must match"))
    end
    ccall(
        (:fficls, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Cint, Ptr{Cstring}, Ptr{Cstring}, Ref{Cint}),
        f.ptr,
        colnum,
        ncols,
        ttype,
        tform,
        status,
    )
    fits_assert_ok(status[])
end

function fits_delete_col(f::FITSFile, colnum::Integer)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffdcol, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ref{Cint}),
        f.ptr,
        colnum,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_flush_file(f::FITSFile)

Flush the file to disk. This is equivalent to closing the file and reopening it.

!!! note
    In most cases, this function should not be needed,
    as the library automatically flushes the file when it is closed.
"""
function fits_flush_file(f::FITSFile)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffflus, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_flush_buffer(f::FITSFile)

Flush the buffer to disk without updating and closing the current HDU.
This is faster than [`fits_flush_file`](@ref), and may be used to
write the state of the file to disk after each row of a table is written.

!!! note
    In most cases, this function should not be needed,
    as the library automatically flushes the file when it is closed.
"""
function fits_flush_buffer(f::FITSFile)
    fits_assert_open(f)
    status = Ref{Cint}(0)
    ccall(
        (:ffflsh, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Cint, Ref{Cint}),
        f.ptr,
        0,
        status,
    )
    fits_assert_ok(status[])
end

function fits_free_memory(longstr::Ptr{UInt8})
    status = Ref{Cint}(0)
    ccall(
        (:fffree, libcfitsio),
        Cint,
        (Ptr{UInt8}, Ref{Cint}),
        longstr,
        status,
    )
    fits_assert_ok(status[])
end

# verification
"""
    fits_write_chksum(f::FITSFile)

Compute and write the `DATASUM` and `CHECKSUM` keyword values for the CHDU into the
current header. If the keywords already exist, their values will be updated only if necessary
(i.e., if the file has been modified since the original keyword values were computed).
"""
function fits_write_chksum(f::FITSFile)
    status = Ref{Cint}(0)
    ccall(
        (:ffpcks, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        status,
    )
    fits_assert_ok(status[])
end

"""
    fits_update_chksum(f::FITSFile)

Update the `CHECKSUM` keyword value in the CHDU, assuming that the `DATASUM` keyword
exists and already has the correct value.
"""
function fits_update_chksum(f::FITSFile)
    status = Ref{Cint}(0)
    ccall(
        (:ffupck, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}),
        f.ptr,
        status,
    )
    fits_assert_ok(status[])
end

@enum ChecksumVerificationStatus MISMATCH=-1 MISSING=0 VERIFIED=1
"""
    fits_verify_chksum(f::FITSFile)

Verify if the checksum of the data and the HDU matches the stored values.
Returns a tuple of `CFITSIO.ChecksumVerificationStatus` values,
indicating the status of the data and HDU checksums.
For either value, a status of `MISSING` indicates that the corresponding keyword is not present,
while a status of `MISMATCH` indicates that the keyword is present but the value is incorrect.
Finally, a value of `VERIFIED` indicates that the checksum was validated successfully.
"""
function fits_verify_chksum(f::FITSFile)
    status = Ref{Cint}(0)
    dataok = Ref{Cint}(0)
    hduok = Ref{Cint}(0)
    ccall(
        (:ffvcks, libcfitsio),
        Cint,
        (Ptr{Cvoid}, Ref{Cint}, Ref{Cint}, Ref{Cint}),
        f.ptr,
        dataok,
        hduok,
        status,
    )
    fits_assert_ok(status[])
    ChecksumVerificationStatus(dataok[]), ChecksumVerificationStatus(hduok[])
end

"""
    libcfitsio_version()::VersionNumber

Return the version of the underlying CFITSIO library

# Example

```julia-repl
julia> libcfitsio_version()
v"4.4.0"
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
