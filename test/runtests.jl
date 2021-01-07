using CFITSIO
using Test


# `create_test_file` : Create a simple FITS file for testing, with the
# given header string added after the required keywords. The length of
# `header` must be a multiple of 80.  The purpose of creating such
# files is to test the parsing of non-standard FITS keyword records
# (non-standard files can't be created with cfitsio).

function create_test_file(fname::AbstractString, header::String)
    if length(header) % 80 != 0
        error("length of header must be multiple of 80")
    end

    f = open(fname, "w")

    stdhdr = "SIMPLE  =                    T / file does conform to FITS standard             BITPIX  =                  -64 / number of bits per data pixel                  NAXIS   =                    2 / number of data axes                            NAXIS1  =                   10 / length of data axis 1                          NAXIS2  =                   10 / length of data axis 2                          EXTEND  =                    T / FITS dataset may contain extensions            "
    endline = "END                                                                             "
    data = fill(0., (10, 10))  # 10x10 array of big-endian Float64 zeros

    # write header
    write(f, stdhdr)
    write(f, header)
    write(f, endline)

    # add padding
    block_position = (length(stdhdr) + length(header) + length(endline)) % 2880
    padding = (block_position == 0) ? 0 : 2880 - block_position
    write(f, " "^padding)

    # write data
    write(f, data)

    # add padding
    block_position = sizeof(data) % 2880
    padding = (block_position == 0) ? 0 : 2880 - block_position
    write(f, fill(0x00, (padding,)))

    close(f)
end

function writehealpix(filename, pixels, nside, ordering, coordsys)
    if eltype(pixels) == Float32
        tform = "1E"
    elseif eltype(pixels) == Float64
        tform = "1D"
    end 

    file = fits_create_file("!"*filename)
    try
        fits_create_img(file, Int16, Int[])
        fits_write_date(file)
        fits_movabs_hdu(file, 1)
        fits_create_binary_tbl(file, length(pixels), [("SIGNAL", tform, "")], "BINTABLE")
        fits_write_key(file, "PIXTYPE",  "HEALPIX", "HEALPIX pixelization")
        fits_write_key(file, "ORDERING", ordering,  "Pixel ordering scheme (either RING or NESTED)")
        fits_write_key(file, "NSIDE",    nside,     "Resolution parameter for HEALPIX")
        fits_write_key(file, "COORDSYS", coordsys,  "Pixelization coordinate system")
        fits_write_comment(file, "G = galactic, E = ecliptic, C = celestial = equatorial")
        fits_write_col(file, 1, 1, 1, pixels)
    finally
        fits_close_file(file)
    end 
end

function readhealpix(filename)
    file = fits_open_file(filename)
    try
        hdutype = fits_movabs_hdu(file, 2)
        tform, tform_comment = fits_read_key_str(file, "TFORM1")
        if tform == "1E"
            T = Float32
        elseif tform == "1D"
            T = Float64
        end

        naxes, naxis_comment = fits_read_key_lng(file, "NAXIS")
        naxis, nfound = fits_read_keys_lng(file, "NAXIS", 1, naxes)
        nside, nside_comment = fits_read_key_lng(file, "NSIDE")
        npix = 12*nside*nside

        ordering, ordering_comment = fits_read_key_str(file, "ORDERING")
        coordsys, coordsys_comment = fits_read_key_str(file, "COORDSYS")

        pixels = zeros(T, npix)
        fits_read_col(file, 1, 1, 1, pixels)

        return pixels, nside, ordering, coordsys

    finally
        fits_close_file(file)
    end
end


@testset "CFITSIO.jl" begin

    @testset "types" begin
        for (T, code) in (
            (UInt8, 11),
            (Int8, 12),
            (Bool, 14),
            (String, 16),
            (Cushort, 20),
            (Cshort, 21),
            (Cuint, 30),
            (Cint, 31),
            (Int64, 81),
            (Float32, 42),
            (Float64, 82),
            (ComplexF32, 83),
            (ComplexF64, 163),
        )
            @test cfitsio_typecode(T) == Cint(code)
        end

        for (T, code) in ((UInt8,     8), # BYTE_IMG
                        (Int16,    16), # SHORT_IMG
                        (Int32,    32), # LONG_IMG
                        (Int64,    64), # LONGLONG_IMG
                        (Float32, -32), # FLOAT_IMG
                        (Float64, -64), # DOUBLE_IMG
                        (Int8,     10), # SBYTE_IMG
                        (UInt16,   20), # USHORT_IMG
                        (UInt32,   40)) # ULONG_IMG
            @test bitpix_from_type(T) == code
            @test type_from_bitpix(Cint(code)) == type_from_bitpix(Val(Cint(code))) == T
            @test type_from_bitpix(Int16(code)) == T
        end
        @test_throws MethodError type_from_bitpix(7)
        @test_throws MethodError type_from_bitpix("BITPIX")
    end


    # test reading/writing Healpix maps as FITS binary tables using the Libcfitsio interface
    for T in (Float32, Float64)
        nside = 4
        npix = 12*nside*nside
        pixels = rand(T, npix)
        ordering = "NESTED"
        coordsys = "G"

        filename = tempname() * ".fits"
        try
            writehealpix(filename, pixels, nside, ordering, coordsys)
            @test readhealpix(filename) == (pixels, nside, ordering, coordsys)
        finally
            ispath(filename) && rm(filename)
        end
    end

    @testset "Miscellaneous" begin
        # test that this function works and returns the right type.
        @test typeof(libcfitsio_version()) === VersionNumber
        # test it parses a number as intended.
        @test libcfitsio_version(3.341)  === VersionNumber(3, 34, 1)
        @test libcfitsio_version(3.41f0) === VersionNumber(3, 41, 0)
    end

    @testset "error message" begin
        mktempdir() do dir
            filename = joinpath(dir, "temp.fits")
            try
                f = fits_create_file("!$filename")
                fits_close_file(f)
            catch e
                io = IOBuffer()
                Base.showerror(io, e)
                errstr = String(take!(io))
                @test occursin(r"Error code"i, errstr)
                @test occursin(r"Error message"i, errstr)
            end
        end
    end

end
