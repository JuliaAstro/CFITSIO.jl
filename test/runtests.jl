using CFITSIO
using Test
using Aqua

function tempfitsfile(fn)
    mktempdir() do dir
        filename = joinpath(dir, "temp.fits")
        fitsfile = fits_clobber_file(filename)
        try
            fn(fitsfile)
        finally
            if fitsfile.ptr != C_NULL
                # write some data to file to avoid errors on closing
                fits_create_empty_img(fitsfile)
            end
            fits_close_file(fitsfile)
            fits_delete_file(fitsfile)
        end
    end
end

using Documenter
DocMeta.setdocmeta!(CFITSIO, :DocTestSetup, :(using CFITSIO); recursive=true)
doctest(CFITSIO, manual=false)

@testset "project quality" begin
    Aqua.test_all(CFITSIO)
end

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

    file = fits_clobber_file(filename)
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

    @testset "file name and mode" begin
        mktempdir() do dir
            filename = joinpath(dir, "temp.fits")
            f = fits_clobber_file(filename)
            @test fits_file_name(f) == filename
            @test fits_file_mode(f) == 1

            # Write some data to the file
            a = ones(2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            close(f)

            f = fits_open_file(filename, 0)
            @test fits_file_mode(f) == 0 == Int(CFITSIO.READONLY) == Int(CFITSIO.R)
            close(f)

            f = fits_open_file(filename, CFITSIO.READONLY)
            @test fits_file_mode(f) == 0 == Int(CFITSIO.READONLY) == Int(CFITSIO.R)
            close(f)

            f = fits_open_file(filename, 1)
            @test fits_file_mode(f) == 1 == Int(CFITSIO.READWRITE) == Int(CFITSIO.RW)
            close(f)

            f = fits_open_file(filename, CFITSIO.READWRITE)
            @test fits_file_mode(f) == 1 == Int(CFITSIO.READWRITE) == Int(CFITSIO.RW)
            close(f)

            @test_throws ArgumentError fits_file_name(f, filename=Vector{UInt8}(undef, 0))
        end
    end

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
            (UInt64, 80),
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
                          (UInt32,   40), # ULONG_IMG
                          (UInt64,   80)) # ULONGLONG_IMG
            @test bitpix_from_type(T) == code
            @test type_from_bitpix(Cint(code)) == type_from_bitpix(Val(Cint(code))) == T
            @test type_from_bitpix(Int16(code)) == T
        end
        @test_throws MethodError type_from_bitpix(7)
        @test_throws MethodError type_from_bitpix("BITPIX")
    end


    # test reading/writing Healpix maps as FITS binary tables using the Libcfitsio interface
    mktempdir() do dir
        filename = joinpath(dir, "temp.fits")
        for T in (Float32, Float64)
            nside = 4
            npix = 12*nside*nside
            pixels = rand(T, npix)
            ordering = "NESTED"
            coordsys = "G"

            writehealpix(filename, pixels, nside, ordering, coordsys)
            @test readhealpix(filename) == (pixels, nside, ordering, coordsys)
        end
    end

    @testset "Miscellaneous" begin
        # test that this function works and returns the right type.
        @test typeof(libcfitsio_version()) === VersionNumber
        # test it parses a number as intended.
        @test libcfitsio_version(3.341)  === VersionNumber(3, 34, 1)
        @test libcfitsio_version(3.41f0) === VersionNumber(3, 41, 0)
    end

    @testset "hdu operations" begin
        tempfitsfile() do f

            # Create a few HDUs
            a = ones(2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            @test fits_get_img_size(f) == [2, 2]
            @test fits_get_img_param(f) == (CFITSIO.bitpix_from_type(eltype(a)), 2, [2,2])

            a = ones(3,3)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            @test fits_get_img_size(f) == [3, 3]

            a = ones(4,4)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            @test fits_get_img_size(f) == [4, 4]

            @test fits_get_num_hdus(f) == 3

            for i in 1:3
                fits_movabs_hdu(f, i)
                @test fits_get_hdu_num(f) == i
                @test fits_get_hdu_type(f) == :image_hdu
            end

            for i = 1:2
                fits_movrel_hdu(f, -1)
                @test fits_get_hdu_num(f) == 3 - i
            end

            fits_movabs_hdu(f, 2)
            fits_delete_hdu(f)
            @test fits_get_num_hdus(f) == 2
            @test fits_get_hdu_num(f) == 2
            @test fits_get_img_size(f) == [4,4]

            fits_movabs_hdu(f, 1)
            fits_delete_hdu(f)
            @test fits_get_num_hdus(f) == 2
            @test fits_get_hdu_num(f) == 1
            @test fits_get_img_dim(f) == 0
        end

        @testset "empty hdu" begin
            tempfitsfile() do f
                fits_create_empty_img(f)
                filename = fits_file_name(f)
                close(f)
                f = fits_open_file(filename)
                @test fits_get_num_hdus(f) == 1
                @test fits_get_img_dim(f) == 0
                @test fits_get_img_size(f) == Int[]
            end
        end

        @testset "insert image" begin
            tempfitsfile() do f
                a = ones(2,2); b = similar(a)
                fits_insert_img(f, a)
                fits_write_pix(f, a)
                fits_read_pix(f, b)
                @test b == a
                @test fits_get_num_hdus(f) == 1
                a .*= 2
                fits_insert_img(f, eltype(a), [size(a)...])
                fits_write_pix(f, a)
                fits_read_pix(f, b)
                @test b == a
                @test fits_get_num_hdus(f) == 2
                fits_movabs_hdu(f, 1)
                a .*= 2
                fits_insert_img(f, a)
                fits_write_pix(f, a)
                fits_read_pix(f, b)
                @test b == a
                @test fits_get_num_hdus(f) == 3
                # test that the HDU is added in the middle
                fits_movabs_hdu(f, 1)
                fits_read_pix(f, b)
                @test b == ones(2,2)
                fits_movabs_hdu(f, 3)
                fits_read_pix(f, b)
                @test b == ones(2,2) .* 2
                fits_movabs_hdu(f, 2)
                fits_read_pix(f, b)
                @test b == ones(2,2) .* 4
                # Insert new primary image
                for sz in Any[[1,2,3], (2,4)]
                    fits_movabs_hdu(f, 1)
                    sz_exist = fits_get_img_size(f)
                    fits_insert_img(f, Int, sz, prepend_primary=true)
                    @test fits_get_hdu_num(f) == 1
                    @test fits_get_img_size(f) == [sz...]
                    # test that the primary HDU is converted to an image
                    fits_movabs_hdu(f, 2)
                    @test fits_get_img_size(f) == sz_exist
                end
            end
        end
    end

    @testset "copy from one file to another" begin
        tempfitsfile() do fin
            for a in [ones(1,1), ones(2,2), ones(3,3)]
                fits_create_img(fin, a)
                fits_write_pix(fin, a)
            end

            @testset "copy file" begin
                tempfitsfile() do fout
                    fits_movabs_hdu(fin, 1)
                    fits_copy_file(fin, fout, false, false, false)
                    @test fits_get_num_hdus(fout) == 0

                    fits_copy_file(fin, fout, true, true, false)
                    @test fits_get_num_hdus(fout) == 1
                    sz = fits_get_img_size(fout)
                    @test sz == [1,1]

                    # prev makes no difference when we're on the first HDU of the input
                    for (ind, prev) in enumerate([false, true])
                        fits_copy_file(fin, fout, prev, true, true)
                        @test fits_get_num_hdus(fout) == 1 + 3ind
                        sz = fits_get_img_size(fout)
                        @test sz == [3,3]
                    end

                    fits_movabs_hdu(fin, 2)
                    fits_copy_file(fin, fout, false, false, false)
                    @test fits_get_num_hdus(fout) == 7
                    fits_copy_file(fin, fout, false, true, false)
                    @test fits_get_num_hdus(fout) == 8
                    sz = fits_get_img_size(fout)
                    @test sz == [2,2]
                    fits_copy_file(fin, fout, true, true, false)
                    @test fits_get_num_hdus(fout) == 10
                    sz = fits_get_img_size(fout)
                    @test sz == [2,2]
                    fits_copy_file(fin, fout, true, true, true)
                    @test fits_get_num_hdus(fout) == 13
                    sz = fits_get_img_size(fout)
                    @test sz == [3,3]
                    fits_copy_file(fin, fout, false, false, true)
                    @test fits_get_num_hdus(fout) == 14
                    sz = fits_get_img_size(fout)
                    @test sz == [3,3]

                    fits_movabs_hdu(fin, 3)
                    fits_copy_file(fin, fout, false, false, false)
                    @test fits_get_num_hdus(fout) == 14
                    fits_copy_file(fin, fout, false, true, false)
                    @test fits_get_num_hdus(fout) == 15
                    sz = fits_get_img_size(fout)
                    @test sz == [3,3]
                    fits_copy_file(fin, fout, true, true, false)
                    @test fits_get_num_hdus(fout) == 18
                    sz = fits_get_img_size(fout)
                    @test sz == [3,3]
                    fits_copy_file(fin, fout, true, true, true)
                    @test fits_get_num_hdus(fout) == 21
                    sz = fits_get_img_size(fout)
                    @test sz == [3,3]
                    fits_copy_file(fin, fout, true, false, false)
                    @test fits_get_num_hdus(fout) == 23
                    sz = fits_get_img_size(fout)
                    @test sz == [2,2]
                end
            end
            @testset "copy hdu" begin
                tempfitsfile() do fout
                    fits_movabs_hdu(fin, 2)
                    fits_copy_hdu(fin, fout)
                    @test fits_get_num_hdus(fout) == 1
                    @test fits_get_img_size(fout) == [2,2]

                    fits_movabs_hdu(fin, 1)
                    fits_copy_hdu(fin, fout, 3)
                    @test fits_get_num_hdus(fout) == 2
                    @test fits_get_img_size(fout) == [1,1]
                end
            end
            @testset "copy header" begin
                tempfitsfile() do fout
                    fits_movabs_hdu(fin, 2)
                    fits_copy_header(fin, fout)
                    @test fits_get_img_size(fout) == [2,2]
                    @test fits_get_num_hdus(fout) == 1
                    fits_copy_hdu(fin, fout)
                    fits_movabs_hdu(fin, 1)
                    fits_copy_header(fin, fout)
                    @test fits_get_img_size(fout) == [1,1]
                    @test fits_get_num_hdus(fout) == 3
                    # test that we write to the same HDU
                    @test fits_get_hdu_num(fout) == 3
                    bout = ones(1,1)*3
                    fits_write_pix(fout, bout)
                    b = zero(bout)
                    fits_read_pix(fout, b)
                    @test b == bout
                    @test fits_get_num_hdus(fout) == 3
                end
            end
            @testset "copy data" begin
                tempfitsfile() do fout
                    fits_movabs_hdu(fin, 2)
                    # copy header first
                    fits_copy_header(fin, fout)
                    fits_copy_data(fin, fout)
                    b = zeros(2,2)
                    fits_read_pix(fout, b)
                    @test all(x -> x == 1, b)
                end
            end
        end
    end

    @testset "read image header" begin
        tempfitsfile() do f
            fits_create_img(f, Float64, [1,2])
            fits_create_img(f, Float64, [2,3])
            fits_movabs_hdu(f, 1)
            simple, bitpix, naxis, naxes, pcount, gcount, extend = fits_read_imghdr(f)
            @test simple
            @test bitpix == -64
            @test naxis == 2
            @test naxes == [1, 2]
            @test pcount == 0
            @test gcount == 1
            @test extend
            fits_movabs_hdu(f, 2)
            simple, bitpix, naxis, naxes, pcount, gcount, extend = fits_read_imghdr(f)
            @test simple
            @test bitpix == -64
            @test naxis == 2
            @test naxes == [2, 3]
            @test pcount == 0
            @test gcount == 1
            @test !extend
            @test_throws ArgumentError fits_read_imghdr(f, naxes=CFITSIO.fits_read_imghdr_buffer(0).naxes)
            @testset "null arguments" begin
                allvals = fits_read_imghdr(f)
                somevals = fits_read_imghdr(f; naxes=nothing)
                @test length(somevals) == length(allvals)
                @test all(((x,y),) -> isnothing(y) || x == y, zip(allvals, somevals))
            end
        end
    end

    @testset "image type/size" begin
        tempfitsfile() do f
            a = ones(2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            @test fits_get_img_dim(f) == 2
            @test fits_get_img_size(f) == [2, 2]
            @test fits_get_img_type(f) == -64
            @test fits_get_img_equivtype(f) == -64

            a = ones(Int64, 2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            @test fits_get_img_dim(f) == 2
            @test fits_get_img_size(f) == [2, 2]
            @test fits_get_img_type(f) == 64
            @test fits_get_img_equivtype(f) == 64

            @test fits_read_keys_lng(f, "NAXIS", 1, 1) == ([2], 1)
            @test fits_read_keys_lng(f, "NAXIS", 1, 2) == ([2, 2], 2)
            @test fits_read_keys_lng(f, "NAXIS", 1, 10) == ([2, 2], 2)
        end
    end

    @testset "read/write strided" begin
        tempfitsfile() do f
            a = ones(4,4); b = similar(a)
            fits_create_img(f, a)
            fits_write_pix(f, view(a, :, :))
            fits_read_pix(f, view(b, :, :))
            @test b == a
            fits_read_pix(f, vec(b))
            @test b == a
            fits_read_pix(f, view(b, :, :, :, :))
            @test b == a
            @test_throws ArgumentError fits_write_pix(f, @view a[1:2:3, :])
            @test_throws ArgumentError fits_read_pix(f, view(b, 1:1, :))
            @test_throws ArgumentError fits_read_pix(f, [1,1], length(a), view(b, :, 1:1))
            @test_throws ArgumentError fits_read_pix(f, (1,1), length(a), view(b, :, 1:1))
        end
    end

    @testset "size checks in read" begin
        tempfitsfile() do f
            a = ones(2,2); b = similar(a)
            fits_create_img(f, a)
            fits_write_pix(f, a)
            @test_throws ArgumentError fits_read_pix(f, [1], length(a), b)
            @test_throws ArgumentError fits_read_pix(f, (1,), length(a), b)
            @test_throws ArgumentError fits_read_pix(f, [1], length(a), NaN, b)
            @test_throws ArgumentError fits_read_pix(f, (1,), length(a), NaN, b)
            @test_throws ArgumentError fits_read_pixnull(f, [1], length(a), b, similar(b, UInt8))
            @test_throws ArgumentError fits_read_pixnull(f, (1,), length(a), b, similar(b, UInt8))
            @test_throws ArgumentError fits_read_subset(f, [1], [2], [1], b)
            @test_throws ArgumentError fits_read_subset(f, (1,), (2,), (1,), b)
            @test_throws ArgumentError fits_read_subset(f, [1], [2], [1], NaN, b)
            @test_throws ArgumentError fits_read_subset(f, (1,), (2,), (1,), NaN, b)
        end
    end

    @testset "read/write subset" begin
        tempfitsfile() do f
            a = rand(10,10)
            b = similar(a)
            fits_create_img(f, a)
            fits_write_pix(f, a)

            fits_read_subset(f, [1,1], [size(a)...], [1,1], b)
            @test a == b

            fits_write_subset(f, [1,1], [size(a,1),4], a)
            c = zeros(eltype(a), size(a,1), 4)
            fits_read_subset(f, [1,1], [size(a,1),4], [1,1], c)
            @test a[:, 1:4] == c

            a[:,1] .= NaN
            fits_write_subset(f, [1,1], [size(a,1),4], a)
            c .= 0
            fits_read_subset(f, [1,1], [size(a,1),4], [1,1], 100.0, c)
            @test all(==(100), c[:, 1])
            @test c[:, 2:4] == a[:,2:4]

            fits_read_subset(f, [1,1], [size(a,1),4], [1,1], 0.0, c)
            @test all(isnan, c[:, 1])
            @test c[:, 2:4] == a[:,2:4]

            tempfitsfile() do f2
                a = rand(20,20)
                fits_create_img(f, a)
                fits_write_pix(f, a)
                fits_copy_image_section(f, f2, "1:20,1:10")
                b = similar(a, 20, 10)
                fits_read_pix(f2, b)
                close(f2)
                @test a[1:20, 1:10] == b
            end
        end
    end

    @testset "error message" begin
        mktempdir() do dir
            filename = joinpath(dir, "temp.fits")
            try
                f = fits_clobber_file(filename)
                fits_close_file(f)
            catch e
                @test e isa CFITSIO.CFITSIOError
                @test e isa Exception # bugfix test as CFITSIOError didn't subtype Exception in #3
                io = IOBuffer()
                Base.showerror(io, e)
                errstr = String(take!(io))
                @test occursin(r"Error code"i, errstr)
                @test occursin(r"Error message"i, errstr)
            finally
                rm(filename, force=true)
            end
        end
        @testset "error on write without image creation" begin
            a = ones(2,2)
            tempfitsfile() do f
                @test_throws CFITSIO.CFITSIOError fits_write_pix(f, a)
            end
            tempfitsfile() do f
                @test_throws CFITSIO.CFITSIOError fits_write_pix(f, [1,1], length(a), a)
            end
            tempfitsfile() do f
                @test_throws CFITSIO.CFITSIOError fits_write_pixnull(f, a, NaN)
            end
            tempfitsfile() do f
                @test_throws CFITSIO.CFITSIOError fits_write_pixnull(f, [1,1], 4, a, NaN)
            end
            tempfitsfile() do f
                fits_create_empty_img(f)
                @test_throws ArgumentError fits_write_pix(f, a)
            end
            tempfitsfile() do f
                fits_create_img(f, eltype(a), [size(a,1), 1])
                @test_throws ArgumentError fits_write_pix(f, a)
            end
        end
        @testset "write beyond data" begin
            tempfitsfile() do f
                a = ones(2,2)
                fits_create_img(f, a)
                @test_throws ArgumentError fits_write_pix(f, [1,1,2], length(a), a)
                @test_throws BoundsError fits_write_pix(f, [1,1], length(a)+1, a)
                @test_throws BoundsError fits_write_pix(f, (1,1), length(a)+1, a)
                @test_throws BoundsError fits_write_pix(f, [size(a)...], 2, a)
                @test_throws BoundsError fits_write_pix(f, size(a), 2, a)
                @test_throws BoundsError fits_write_pixnull(f, [1,1], length(a)+1, a, NaN)
                @test_throws BoundsError fits_write_pixnull(f, (1,1), length(a)+1, a, NaN)
                @test_throws BoundsError fits_write_pixnull(f, [size(a)...], 2, a, NaN)
                @test_throws BoundsError fits_write_pixnull(f, size(a), 2, a, NaN)
                @test_throws BoundsError fits_write_subset(f, [1,1], [3,3], a)
                @test_throws BoundsError fits_write_subset(f, (1,1), (3,3), a)
            end
        end
    end

    @testset "closed file errors" begin
        tempfitsfile() do f
            # write arbitrary data to the file
            a = ones(2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)
            close(f)

            # check that closed files throw a julia error and don't segfault
            for fn in [fits_file_name, fits_file_mode, fits_get_hdrspace, fits_write_date,
                fits_hdr2str, fits_get_hdu_num, fits_get_hdu_type, fits_get_img_type,
                fits_get_img_equivtype, fits_get_img_dim, fits_get_num_cols, fits_get_num_hdus,
                fits_get_rowsize, fits_get_img_size, fits_get_img_size, fits_get_num_rows,
                fits_delete_hdu,
                ]

                @test_throws ArgumentError fn(f)
            end

            for fn in [fits_read_key_str, fits_read_key_lng, fits_read_keyword, fits_write_comment,
                fits_write_history, fits_write_record, fits_delete_key, fits_movnam_hdu, fits_get_colnum,
                ]

                @test_throws ArgumentError fn(f, "abc")
            end

            for fn in [fits_read_record, fits_read_keyn, fits_delete_record,
                fits_movabs_hdu, fits_movrel_hdu, fits_get_coltype,
                fits_get_eqcoltype, fits_read_tdim, ]

                @test_throws ArgumentError fn(f, 1)
            end

            for fn in [fits_insert_rows, fits_delete_rows, fits_read_descript, CFITSIO.fits_write_null_img]
                @test_throws ArgumentError fn(f, 1, 2)
            end

            for fn in [fits_write_pix, fits_read_pix]
                @test_throws ArgumentError fn(f, a)
            end
            for fn in [fits_write_pix, fits_read_pix]
                @test_throws ArgumentError fn(f, [1,1], length(a), a)
            end

            @test_throws ArgumentError fits_read_pix(f, ones(Int, ndims(a)), length(a), zero(eltype(a)),  a)

            @test_throws ArgumentError fits_read_keys_lng(f, "a", 1, 2)

            for fn in [fits_write_key, fits_update_key]
                @test_throws ArgumentError fn(f, "a", 1, "b")
            end

            for fn in [fits_read_col, fits_write_col]
                @test_throws ArgumentError fn(f, 1, 1, 1, ["abc"])
                @test_throws ArgumentError fn(f, 1, 1, 1, ["abc", 1])
            end

            for fn in [fits_create_binary_tbl, fits_create_ascii_tbl]
                @test_throws ArgumentError fn(f, 1, [("name", "3D", "c")], "extname")
            end

            @test_throws ArgumentError fits_update_key(f, "a", 1.0, "b")
            @test_throws ArgumentError fits_update_key(f, "a", nothing, "b")
            @test_throws ArgumentError fits_write_tdim(f, 1, [1, 2])

            @test_throws ArgumentError fits_read_subset(f, [1,1], [2,2], [1,1], a)
            @test_throws ArgumentError fits_write_subset(f, [1,1], [2,2], a)

            @test_throws ArgumentError fits_create_img(f, Int64, [2,3])

            tempfitsfile() do f2
                fits_create_img(f2, eltype(a), [size(a)...])
                fits_write_pix(f2, a)
                close(f2)

                @test_throws ArgumentError fits_copy_image_section(f, f2, "1:2")
                @test_throws ArgumentError fits_copy_image_section(f2, f, "1:2")
            end
        end
    end

    @testset "read table header" begin
        @testset "ascii table" begin
            tempfitsfile() do f
                fits_create_ascii_tbl(f, 0, [("A", "I4", "counts"), ("B", "F10.2", "K")], "test")
                rowlen, nrows, tfields, ttype, tbcol, tform, tunit, extname = fits_read_atblhdr(f, 1)
                @test rowlen == 15
                @test nrows == 0
                @test tfields == 2
                @test extname == "test"
                @test tbcol == [1]
                @test ttype == ["A"]
                @test tform == ["I4"]
                @test tunit == ["counts"]
                rowlen, nrows, tfields, ttype, tbcol, tform, tunit, extname = fits_read_atblhdr(f, 3)
                @test tbcol == [1, 6]
                @test ttype == ["A", "B"]
                @test tform == ["I4", "F10.2"]
                @test tunit == ["counts", "K"]
                buf = CFITSIO.fits_read_atblhdr_buffer(3)
                @test fits_read_atblhdr(f, 3) == fits_read_atblhdr(f, 3; buf...)
                buf = CFITSIO.fits_read_atblhdr_buffer(0)
                @test_throws ArgumentError fits_read_atblhdr(f, 3, ttype=buf.ttype)
                @test_throws ArgumentError fits_read_atblhdr(f, 3, tform=buf.tform)
                @test_throws ArgumentError fits_read_atblhdr(f, 3, tunit=buf.tunit)
                @test_throws ArgumentError fits_read_atblhdr(f, 3, tbcol=buf.tbcol)
                @test_throws ArgumentError fits_read_atblhdr(f, 3, extname=Vector{UInt8}(undef, 0))
                @testset "skip units and extname" begin
                    fits_create_ascii_tbl(f, 0, [("A", "I4"), ("B", "F10.2")])
                    rowlen, nrows, tfields, ttype, tbcol, tform, tunit, extname = fits_read_atblhdr(f)
                    @test all(==(""), tunit)
                    @test extname == ""
                    fits_create_ascii_tbl(f, 0, ["A", "B"], ["I4", "F10.2"])
                    rowlen, nrows, tfields, ttype, tbcol, tform, tunit, extname = fits_read_atblhdr(f)
                    @test all(==(""), tunit)
                    @test extname == ""
                end
                @testset "null arguments" begin
                    allvals = fits_read_atblhdr(f)
                    @testset for kw in Any[(; ttype=nothing),
                                    (; ttype=nothing, tform=nothing),
                                    (; ttype=nothing, tform=nothing, tunit=nothing),
                                    (; ttype=nothing, tform=nothing, tunit=nothing, extname=nothing),
                                    (; ttype=nothing, tform=nothing, tunit=nothing, extname=nothing, tbcol=nothing)
                                    ]
                        somevals = fits_read_atblhdr(f; kw...)
                        @test length(somevals) == length(allvals)
                        @test all(((x,y),) -> isnothing(y) || x == y, zip(allvals, somevals))
                    end
                end
            end
        end
        @testset "binary table" begin
            tempfitsfile() do f
                fits_create_binary_tbl(f, 0, [("A", "J", "counts"), ("B", "D", "K")], "test")
                nrows, tfields, ttype, tform, tunit, extname, pcount = fits_read_btblhdr(f, 1)
                @test nrows == 0
                @test tfields == 2
                @test extname == "test"
                @test ttype == ["A"]
                @test tform == ["J"]
                @test tunit == ["counts"]
                nrows, tfields, ttype, tform, tunit, extname, pcount = fits_read_btblhdr(f, 3)
                @test ttype == ["A", "B"]
                @test tform == ["J", "D"]
                @test tunit == ["counts", "K"]
                buf = CFITSIO.fits_read_btblhdr_buffer(3)
                @test fits_read_btblhdr(f, 3) == fits_read_btblhdr(f, 3; buf...)
                buf = CFITSIO.fits_read_btblhdr_buffer(0)
                @test_throws ArgumentError fits_read_btblhdr(f, 3, ttype=buf.ttype)
                @test_throws ArgumentError fits_read_btblhdr(f, 3, tform=buf.tform)
                @test_throws ArgumentError fits_read_btblhdr(f, 3, tunit=buf.tunit)
                @test_throws ArgumentError fits_read_btblhdr(f, 3, extname=Vector{UInt8}(undef, 0))
                @testset "skip units and extname" begin
                    fits_create_binary_tbl(f, 0, [("A", "J"), ("B", "D")])
                    nrows, tfields, ttype, tform, tunit, extname, pcount = fits_read_btblhdr(f)
                    @test all(==(""), tunit)
                    @test extname == ""
                    fits_create_binary_tbl(f, 0, ["A", "B"], ["J", "D"])
                    nrows, tfields, ttype, tform, tunit, extname, pcount = fits_read_btblhdr(f)
                    @test all(==(""), tunit)
                    @test extname == ""
                end
                @testset "null arguments" begin
                    allvals = fits_read_btblhdr(f)
                    @testset for kw in Any[(; ttype=nothing),
                                    (; ttype=nothing, tform=nothing),
                                    (; ttype=nothing, tform=nothing, tunit=nothing),
                                    (; ttype=nothing, tform=nothing, tunit=nothing, extname=nothing)]
                        somevals = fits_read_btblhdr(f; kw...)
                        @test length(somevals) == length(allvals)
                        @test all(((x,y),) -> isnothing(y) || x == y, zip(allvals, somevals))
                    end
                end
            end
        end
    end

    @testset "header operations" begin
        tempfitsfile() do f
            fits_create_img(f, Float32, (100, 100))
            fits_write_key(f, "TESTKEY", 2, "Number of photons")
            @test fits_read_key_lng(f, "TESTKEY") == (2, "Number of photons")
            fits_write_key_unit(f, "TESTKEY", "counts")
            @test fits_read_key_lng(f, "TESTKEY") == (2, "[counts] Number of photons")
            @test fits_read_key_unit(f, "TESTKEY") == "counts"
            @test_throws ArgumentError CFITSIO.fits_read_key_unit(f, "DUMMYKEY",
                unit=Vector{UInt8}(undef,0))
        end
    end

    @testset "null values" begin
        tempfitsfile() do f

            # all values are nullified
            a = ones(2,2)
            nullarray = similar(a, UInt8)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pixnull(f, ones(Int, ndims(a)), length(a), a, 1.0)
            fits_read_pix(f, a)
            @test all(isnan, a)

            # one values is nullified
            a .= [1 2; 3 4]
            fits_write_pixnull(f, a, 3.0)
            fits_read_pix(f, a)
            @test isnan(a[2,1])
            @test !isnan(a[2,2]) && all(!isnan, a[1,:])

            # one values is nullified, but replaced while being read back in
            a .= [1 2; 3 4]
            fits_write_pixnull(f, a, 3.0)
            fits_read_pix(f, a, 3.0)
            @test !any(isnan, a)
            @test a[2,1] == 3.0

            fits_write_pixnull(f, a, 3.0)
            fits_read_pix(f, a, 0.0)
            @test isnan(a[2,1])
            fits_read_pix(f, a, C_NULL)
            @test isnan(a[2,1])

            # get the indices of null values
            nullarray .= 0
            fits_read_pixnull(f, a, nullarray)
            @test nullarray[2,1] == 1
            @test iszero(nullarray[2,2]) && all(iszero, nullarray[1,:])

            nullarray .= 0
            fits_read_pixnull(f, a, vec(nullarray))
            @test nullarray[2,1] == 1
            @test iszero(nullarray[2,2]) && all(iszero, nullarray[1,:])

            @test_throws Exception fits_read_pixnull(f, a, similar(nullarray, 1))

            # don't treat null values as special while writing out
            a .= [1 2; 3 4]
            fits_write_pixnull(f, a, C_NULL)
            fits_read_pix(f, a)
            @test !any(isnan, a)
            a .= [1 2; NaN 4]
            fits_write_pixnull(f, a, C_NULL)
            fits_read_pix(f, a)
            @test isnan(a[2,1])
            @test !isnan(a[2,2]) && all(!isnan, a[1,:])

            # test that tuples and vectors of pixels behave identically
            a .= [1 2; NaN 4]
            nullarray .= 0
            nullarray2 = similar(nullarray)
            fits_write_pixnull(f, a, C_NULL)
            b = similar(a)
            fits_read_pixnull(f, [1,1], length(b), b, nullarray)
            fits_read_pixnull(f, (1,1), length(b), b, nullarray2)
            @test nullarray == nullarray2

            # Read in data by replacing null values with the specified value
            for nullval in Any[7, 7.0], fpixel in Any[[1,1], (1,1)]
                b .= 0
                fits_read_pix(f, fpixel, length(b), nullval, b)
                @test b[2,1] == nullval
            end

            for fpixel in Any[[1,1], (1,1)]
                # Write data first by treating the value 2 as null
                fits_write_pixnull(f, fpixel, length(b), a, 2)
                b .= 0
                fits_read_pix(f, fpixel, length(b), b)
                @test isnan(b[1,2])
                @test isnan(b[2,1])
                # replace the null value by a specified one
                for nullval in Any[7, 7.0]
                    b .= 0
                    fits_read_pix(f, fpixel, length(b), nullval, b)
                    @test b[2,1] == nullval
                    @test b[1,2] == nullval
                end
            end

            # set a stretch of values to null (NaN in this case)
            # for the test we set all values to null
            fits_write_null_img(f, 1, length(a))
            fits_read_pix(f, a)
            @test all(isnan, a)
        end
    end

    @testset "empty file" begin
        a = zeros(2,2)
        tempfitsfile() do f
            @test_throws CFITSIO.CFITSIOError fits_read_pix(f, a)
        end
        tempfitsfile() do f
            @test_throws CFITSIO.CFITSIOError fits_read_pix(f, a, 1)
        end
        tempfitsfile() do f
            @test_throws CFITSIO.CFITSIOError fits_read_pixnull(f, a, similar(a, UInt8))
        end
    end

    @testset "non-Int64 types" begin
        tempfitsfile() do f
            # write arbitrary data to the file
            a = ones(2,2)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, Int32[1,1], big(length(a)),  a)
            b = similar(a)
            fits_read_pix(f, Int32[1,1], Int128(length(b)), b)
            @test b == a
        end
    end

    @testset "resize image" begin
        tempfitsfile() do f
            a = [1 2; 3 4];
            fits_create_img(f, a);
            fits_write_pix(f, a);
            fits_resize_img(f, [3,3]);
            @test fits_get_img_size(f) == [3,3]
            fits_resize_img(f, (4,4));
            @test fits_get_img_size(f) == [4,4]
            fits_resize_img(f, (7,));
            @test fits_get_img_size(f) == [7]
            fits_resize_img(f, Float64);
            @test type_from_bitpix(fits_get_img_type(f)) == Float64
            fits_write_pix(f, Float64.(1:7))
            b = zeros(Float64, 7)
            fits_read_pix(f, b)
            @test b == 1:7
        end
    end

    @testset "tuples vs vectors" begin
        tempfitsfile() do f
            @testset "create" begin
                a = Float64[1 3; 2 4]
                b = similar(a); c = similar(a);
                fits_create_img(f, eltype(a), size(a))
                fits_write_pix(f, a)
                fits_read_pix(f, b)
                fits_create_img(f, eltype(a), [size(a)...])
                fits_write_pix(f, a)
                fits_read_pix(f, c)
                @test b == c
            end
            @testset "write" begin
                a = Float64[1 3; 2 4]
                b = similar(a); c = similar(a);
                fits_write_pix(f, [1,1], length(a), a)
                fits_read_pix(f, b)
                fits_write_pix(f, (1,1), length(a), a)
                fits_read_pix(f, c)
                @test b == c
            end

            @testset "size" begin
                @test fits_get_img_size(f, Val(2)) == (2,2)
            end
            @testset "read" begin
                @testset "full image" begin
                    a = Float64[1 3; 2 4]
                    b = similar(a); c = similar(a);
                    fits_write_pix(f, (1,1), length(a), a)
                    fits_read_pix(f, b)
                    @test b == a
                    # test that vectors and tuples of pixels behave identically
                    fits_read_pix(f, [1,1], length(b), b)
                    fits_read_pix(f, (1,1), length(c), c)
                    @test b == c
                end
                @testset "subset" begin
                    a = Float64[1 3; 2 4]
                    b = similar(a); c = similar(a);
                    b .= 0
                    fits_write_pix(f, (1,1), length(a), a)
                    # test that vectors and tuples of pixels behave identically
                    fits_read_subset(f, (1,1), (2,1), (1,1), @view b[:,1])
                    fits_read_subset(f, [1,1], [2,1], [1,1], @view b[:,2])
                    @test @views b[:,1] == b[:,2]
                end
            end
            @testset "subset" begin
                a = Float64[1 3; 2 4]
                b = similar(a); c = similar(a);
                fits_create_img(f, eltype(a), (size(a,1),))
                fits_write_subset(f, [1,1], [2,1], a)
                fits_read_pix(f, @view b[:,1])
                fits_write_subset(f, (1,1), (2,1), a)
                fits_read_pix(f, @view b[:, 2])
                @test @views b[:,1] == b[:,2]

                b .= 0
                fits_write_subset(f, [1,1], [2,1], a)
                fits_read_subset(f, [1,1], [2,1], [1,1], @view b[:,1])
                fits_read_subset(f, (1,1), (2,1), (1,1), @view b[:,2])
                @test @views b[:,1] == b[:,2]
                b .= 0
                fits_write_subset(f, [1,1], [2,1], replace(a, 2 => NaN))
                fits_read_subset(f, [1,1], [2,1], [1,1], 4, @view b[:,1])
                fits_read_subset(f, (1,1), (2,1), (1,1), 4, @view b[:,2])
                @test @views b[:,1] == b[:,2]
                @test b[2,1] == b[2,2] == 4
            end
        end
    end


    @testset "extended filename parser" begin
        filename = tempname()
        a = [1 3; 2 4]
        b = similar(a)
        try
            # for filenames that don't contain extended format specifications,
            # the diskfile functions are equivalent to the file ones
            for createfn in [fits_create_file, fits_create_diskfile]
                f = createfn(filename)
                fits_create_img(f, a)
                fits_write_pix(f, a)
                close(f)
                # Extended format: flipping the image
                f = fits_open_file(filename*"[*,-*]")
                fits_read_pix(f, b)
                @test a[:, [2,1]] == b
                close(f)
                f = fits_open_file(filename*"[-*,*]")
                fits_read_pix(f, b)
                @test a[[2,1], :] == b
                close(f)
                # without extended format
                f = fits_open_diskfile(filename)
                fits_read_pix(f, b)
                @test a == b
                close(f)
                rm(filename)
            end
        finally
            rm(filename, force = true)
        end

        # the diskfile functions may include [] in the filenames
        filename2 = filename * "[abc].fits"
        @test_throws CFITSIO.CFITSIOError fits_create_file(filename2)
        try
            f = fits_create_diskfile(filename2)
            fits_create_img(f, a)
            fits_write_pix(f, a)
            fits_read_pix(f, b)
            @test a == b
            close(f)
            f = fits_open_diskfile(filename2)
            fits_read_pix(f, b)
            @test a == b
            close(f)
            @test_throws CFITSIO.CFITSIOError fits_open_file(filename2)
        finally
            rm(filename2, force = true)
        end
    end

    @testset "stdout/stdin streams" begin
        # We redirect the output streams to a temp file to avoid cluttering the output
        # At present this doesn't work completely, as there is some output from fits_create_img
        # that is not captured
        mktemp() do _, io
            redirect_stdout(io) do
                for fname in ["-", "stdout.gz"]
                    f = fits_create_file(fname);
                    for a in Any[[1 2; 3 4], Float64[1 2; 3 4]]
                        b = similar(a)
                        fits_create_img(f, a)
                        fits_write_pix(f, a)
                        fits_read_pix(f, b)
                        @test a == b
                    end
                    close(f)
                end
            end
        end
    end

    @testset "Table string columns (#34)" begin
        tempfitsfile() do f
            # Create a simple image in the primary HDU (required by FITS standard)
            fits_create_img(f, Int32, [0])

            # Define our table columns: name, tform, unit
            # tform: 1J = single 32-bit integer, 1D = single double precision, 10A = string with 10 chars
            cols = [
                ("ID", "1J", ""),
                ("VALUE", "1D", "meters"),
                ("NAME", "10A", "")
            ]

            # Number of rows to create
            nrows = 5

            # Create a binary table
            fits_create_binary_tbl(f, nrows, cols, "TEST_TABLE")

            # Prepare some test data
            ids = Int32[1, 2, 3, 4, 5]
            values = Float64[1.1, 2.2, 3.3, 4.4, 5.5]
            names = ["alpha", "beta", "gamma", "delta", "epsilon"]

            # Write data to the columns
            fits_write_col(f, 1, 1, 1, ids)
            fits_write_col(f, 2, 1, 1, values)
            fits_write_col(f, 3, 1, 1, names)

            # Get number of rows
            nrows = fits_get_num_rows(f)

            # Read back the data
            ids_read = Array{Int32}(undef, nrows)
            values_read = Array{Float64}(undef, nrows)
            names_read = Array{String}(undef, nrows)

            fits_read_col(f, 1, 1, 1, ids_read)
            fits_read_col(f, 2, 1, 1, values_read)
            fits_read_col(f, 3, 1, 1, names_read)

            # Check data was read correctly
            @test ids_read == ids
            @test values_read == values
            @test names_read == names

            typecode, repcount, width = fits_get_coltype(f, 1) # 1J
            @test repcount == 1
            @test width == 4
            @test fits_get_coltype(f, 1) == fits_get_eqcoltype(f, 1)
            typecode, repcount, width = fits_get_coltype(f, 2) # 1D
            @test repcount == 1
            @test width == 8
            @test fits_get_coltype(f, 2) == fits_get_eqcoltype(f, 2)
            typecode, repcount, width = fits_get_coltype(f, 3) # 10A
            @test repcount == 10
            @test width == 10
            @test fits_get_coltype(f, 3) == fits_get_eqcoltype(f, 3)
            # Close the file
            close(f)
        end
    end

    @testset "read keyword" begin
        tempfitsfile() do f
            CFITSIO.fits_create_img(f, Int16, (10,10))
            CFITSIO.fits_write_key(f, "DUMMYKEY", "DummyValue", "This is a test keyword")
            val, comment = CFITSIO.fits_read_keyword(f, "DUMMYKEY")
            @test occursin("DummyValue", val)
            @test comment == "This is a test keyword"
            val, comment = CFITSIO.fits_read_keyword(f, "DUMMYKEY", comment=nothing)
            @test occursin("DummyValue", val)
            @test isnothing(comment)
            @test_throws ArgumentError CFITSIO.fits_read_keyword(f, "DUMMYKEY",
                value=Vector{UInt8}(undef,0))
            @test_throws ArgumentError CFITSIO.fits_read_keyword(f, "DUMMYKEY",
                comment=Vector{UInt8}(undef,0))
            CFITSIO.fits_update_key(f, "DUMMYKEY", "NewValue")
            val, comment = CFITSIO.fits_read_key_str(f, "DUMMYKEY")
            @test val == "NewValue"
            @test comment == "This is a test keyword"
            val, comment = CFITSIO.fits_read_key_str(f, "DUMMYKEY", comment=nothing)
            @test val == "NewValue"
            @test isnothing(comment)
            @test_throws ArgumentError CFITSIO.fits_read_key_str(f, "DUMMYKEY",
                comment=Vector{UInt8}(undef,0))
            @test_throws ArgumentError CFITSIO.fits_read_key_str(f, "DUMMYKEY",
                value=Vector{UInt8}(undef,0))

            CFITSIO.fits_write_key(f, "INTKEY", 2)
            val, comment = CFITSIO.fits_read_key_lng(f, "INTKEY")
            @test val == 2
            @test comment == ""
            val, comment = CFITSIO.fits_read_key_lng(f, "INTKEY", comment=nothing)
            @test val == 2
            @test isnothing(comment)
            @test_throws ArgumentError CFITSIO.fits_read_key_lng(f, "INTKEY",
                comment=Vector{UInt8}(undef,0))

            CFITSIO.fits_write_key(f, "FLTKEY", 2.0)
            val, comment = CFITSIO.fits_read_key_str(f, "FLTKEY")
            @test parse(Float64, val) == 2.0
            @test comment == ""
            CFITSIO.fits_update_key(f, "FLTKEY", 3.0)
            val, comment = CFITSIO.fits_read_key_str(f, "FLTKEY")
            @test parse(Float64, val) == 3.0
            @test comment == ""

            hdr_str = CFITSIO.fits_hdr2str(f)
            @test occursin("DUMMYKEY", hdr_str)
            @test occursin("NewValue", hdr_str)
            @test occursin("This is a test keyword", hdr_str)
            @test occursin("INTKEY", hdr_str)
            @test occursin("2", hdr_str)
            @test occursin("FLTKEY", hdr_str)
            @test occursin("3.", hdr_str)
            @test occursin("COMMENT", hdr_str)
            hdr_str = CFITSIO.fits_hdr2str(f, true)
            @test !occursin("COMMENT", hdr_str)

            # reset keyword
            CFITSIO.fits_update_key(f, "DUMMYKEY", "DummyValue", "This is a test keyword")

            # Read the 9th header record — first 8 are standard FITS keywords
            record_str = CFITSIO.fits_read_record(f, 9)
            @test occursin("DUMMYKEY", record_str)
            @test occursin("DummyValue", record_str)
            @test occursin("This is a test keyword", record_str)
            @test_throws ArgumentError CFITSIO.fits_read_record(f, 9, card=Vector{UInt8}(undef,0))

            keyname, value, comment = CFITSIO.fits_read_keyn(f, 9)
            @test keyname == "DUMMYKEY"
            @test occursin("DummyValue", value)
            @test comment == "This is a test keyword"
            @test_throws ArgumentError CFITSIO.fits_read_keyn(f, 9, keyname=Vector{UInt8}(undef,0))
            @test_throws ArgumentError CFITSIO.fits_read_keyn(f, 9, value=Vector{UInt8}(undef,0))
            @test_throws ArgumentError CFITSIO.fits_read_keyn(f, 9, comment=Vector{UInt8}(undef,0))

            keyname, value, comment = CFITSIO.fits_read_keyn(f, 9, comment=nothing)
            @test keyname == "DUMMYKEY"
            @test occursin("DummyValue", value)
            @test isnothing(comment)
        end
    end

    @testset "memfile" begin
        tempfitsfile() do f
            CFITSIO.fits_create_img(f, Int16, (2,2))
            CFITSIO.fits_write_key(f, "EXAMPLE", 42, "Memory test")
            filename = CFITSIO.fits_file_name(f)
            close(f)

            file_bytes = read(filename)
            nbytes = length(file_bytes)
            buffer = Vector{UInt8}(file_bytes)  # Mutable buffer for CFITSIO

            fmem, handle = CFITSIO.fits_open_memfile(buffer, CFITSIO.R)
            val, comment = CFITSIO.fits_read_keyword(fmem, "EXAMPLE")
            @test val == "42"
            @test comment == "Memory test"

            close(fmem)
        end
    end

    @testset "buffers" begin
        buf = CFITSIO.fits_read_key_str_buffer()
        @test :value in keys(buf)
        @test :comment in keys(buf)

        buf = CFITSIO.fits_read_keyword_buffer()
        @test :value in keys(buf)
        @test :comment in keys(buf)

        buf = CFITSIO.fits_read_keyn_buffer()
        @test :keyname in keys(buf)
        @test :value in keys(buf)
        @test :comment in keys(buf)

        @testset "incorrect types error" begin
            tempfitsfile() do f
                @test_throws TypeError fits_file_name(f, filename = Int[2])
                @test_throws TypeError fits_read_keyn(f, 1, keyname = Int[2])
                @test_throws TypeError fits_read_keyn(f, 1, value = Int[2])
                @test_throws TypeError fits_read_keyn(f, 1, comment = Int[2])
                @test_throws TypeError fits_read_record(f, 1, card = Int[2])
                @test_throws TypeError fits_read_keyword(f, "DUMMYKEY", value = Int[2])
                @test_throws TypeError fits_read_keyword(f, "DUMMYKEY", comment = Int[2])
                @test_throws TypeError fits_read_keys_lng(f, "DUMMYKEY", 1, 2, value = Float64[2])
                @test_throws TypeError fits_read_key_lng(f, "DUMMYKEY", comment = Float64[2])
                @test_throws TypeError fits_read_tdim(f, 1, naxes = Float64[2])
                @test_throws TypeError CFITSIO.fits_read_errmsg(err_msg = Int[2])
                @test_throws TypeError CFITSIO.fits_get_errstatus(1, err_text = Int[2])
                @test_throws TypeError fits_read_key_str(f, "DUMMYKEY", value = Int[2])
                @test_throws TypeError fits_read_key_str(f, "DUMMYKEY", comment = Int[2])
            end
        end
    end

    @testset "write to and read from ascii table" begin
        tempfitsfile() do f
            # Create an ASCII table with 3 columns
            cols = [
                ("ID", "I6", ""),
                ("VALUE", "F10.2", "meters"),
                ("NAME", "A10", "")
            ]
            nrows = 5
            fits_create_ascii_tbl(f, nrows, cols, "TEST_TABLE")

            # Prepare some test data
            ids = Int32[1, 2, 3, 4, 5]
            values = Float64[1.1, 2.2, 3.3, 4.4, 5.5]
            names = ["alpha", "beta", "gamma", "delta", "epsilon"]

            # Write data to the columns
            fits_write_col(f, 1, 1, 1, ids)
            fits_write_col(f, 2, 1, 1, values)
            fits_write_col(f, 3, 1, 1, names)

            # Read back the data
            ids_read = Array{Int32}(undef, nrows)
            values_read = Array{Float64}(undef, nrows)
            names_read = Array{String}(undef, nrows)

            fits_read_col(f, 1, 1, 1, ids_read)
            fits_read_col(f, 2, 1, 1, values_read)
            fits_read_col(f, 3, 1, 1, names_read)

            # Check data was read correctly
            @test ids_read == ids
            @test values_read == values
            @test names_read == names

            # Check that mismatched lengths throw an error
            @test_throws ArgumentError CFITSIO.fits_create_tbl(
                f, CFITSIO.ASCII_TBL, 0, String[], String["a"], String[], "TEST_TABLE")
            @test_throws ArgumentError CFITSIO.fits_create_tbl(
                f, CFITSIO.ANY_HDU, 0, String[], String[], String[], "TEST_TABLE")

            @testset "insert/delete column" begin
                ncols = fits_get_num_cols(f)
                fits_insert_col(f, 1, "NEWCOL", "F12.2")
                fits_write_col(f, 1, 1, 1, Float32[10, 20, 30, 40, 50])
                @test fits_get_num_cols(f) == ncols + 1
                nrows = fits_get_num_rows(f)
                data = Vector{Float32}(undef, nrows)
                fits_read_col(f, 1, 1, 1, data)
                @test data == 10:10:50
                fits_delete_col(f, 1)
                @test fits_get_num_cols(f) == ncols
                fits_read_col(f, 1, 1, 1, ids_read)
                @test ids_read == ids

                fits_insert_cols(f, 1, ["NEWCOL1", "NEWCOL2"], ["F12.2", "F12.2"])
                fits_write_col(f, 1, 1, 1, Float32[10, 20, 30, 40, 50])
                fits_write_col(f, 2, 1, 1, Float32[100, 200, 300, 400, 500])
                @test fits_get_num_cols(f) == ncols + 2
                fits_read_col(f, 1, 1, 1, data)
                @test data == 10:10:50
                fits_read_col(f, 2, 1, 1, data)
                @test data == 100:100:500
                fits_delete_col(f, 1)
                fits_delete_col(f, 1)
                @test fits_get_num_cols(f) == ncols
                fits_read_col(f, 1, 1, 1, ids_read)
                @test ids_read == ids
            end

            @testset "fits_delete_rowlist" begin
                # Delete rows 2 and 4
                rowlist = [2, 4]
                fits_delete_rowlist(f, rowlist)

                nrows = fits_get_num_rows(f)
                # Read back the data
                ids_read = Array{Int32}(undef, nrows)
                values_read = Array{Float64}(undef, nrows)
                names_read = Array{String}(undef, nrows)

                fits_read_col(f, 1, 1, 1, ids_read)
                fits_read_col(f, 2, 1, 1, values_read)
                fits_read_col(f, 3, 1, 1, names_read)

                # Check data was read correctly
                @test ids_read == [1, 3, 5]
                @test values_read == [1.1, 3.3, 5.5]
                @test names_read == ["alpha", "gamma", "epsilon"]
            end
        end
    end

    @testset "variable length column in binary table" begin
        tempfitsfile() do f
            # Create a binary table with variable-length column
            cols = [
                ("col1", "1PJ", ""),
            ]
            fits_create_binary_tbl(f, 0, cols, "VARLEN")

            data = Int32[10, 20, 30]
            CFITSIO.fits_write_col(f, 1, 1, 1, data)

            nelem, heap_offset = CFITSIO.fits_read_descript(f, 1, 1)

            # Step 4: Assertions
            @test nelem == 3
            @test heap_offset ≥ 0  # Offset in heap should be non-negative
        end
    end

    @testset "flush" begin
        tempfitsfile() do f
            # Create a simple image in the primary HDU (required by FITS standard)
            fits_create_img(f, Int32, [0])

            # Define our table columns: name, tform, unit
            # tform: 1J = single 32-bit integer, 1D = single double precision, 10A = string with 10 chars
            cols = [
                ("ID", "1J", ""),
                ("VALUE", "1D", "meters"),
                ("NAME", "10A", "")
            ]

            # Create a binary table
            fits_create_binary_tbl(f, 0, cols, "TEST_TABLE")

            # Prepare some test data
            ids = Int32[1, 2]
            values = Float64[1.1, 2.2]
            names = ["alpha", "beta"]

            # Write data to the columns
            fits_write_col(f, 1, 1, 1, ids[1:1])
            fits_write_col(f, 2, 1, 1, values[1:1])
            fits_write_col(f, 3, 1, 1, names[1:1])

            # Flush the file
            CFITSIO.fits_flush_file(f)
            @test fits_get_num_rows(f) == 1

            fits_write_col(f, 1, 2, 1, ids[2:2])
            fits_write_col(f, 2, 2, 1, values[2:2])
            fits_write_col(f, 3, 2, 1, names[2:2])

            CFITSIO.fits_flush_buffer(f)
            fits_update_key(f, "NAXIS2", 2, "Number of rows")
            data = zero(ids)
            fits_read_col(f, 1, 1, 1, data)
            @test data == ids
        end
    end

    @testset "fits_get_hdrspace" begin
        tempfitsfile() do f
            # Create a simple image in the primary HDU (required by FITS standard)
            fits_create_img(f, Int32, [0])

            # Write some keywords to the header
            fits_write_key(f, "KEY1", 42, "First keyword")
            fits_write_key(f, "KEY2", 3.14, "Second keyword")

            # Get the header space
            nkeys, morekeys = fits_get_hdrspace(f)

            # Step 4: Assertions
            @test nkeys >= 2  # Number of keywords written
            @test morekeys == -1  # Header not closed yet
        end
    end

    @testset "fits_read_tdim & fits_write_tdim" begin
        tempfitsfile() do f
            fits_create_binary_tbl(f, 0, [("col1", "6J", "")], "TEST_TABLE")

            # Manually set TDIM1 = '(2,3)' to specify 2x3 array
            fits_write_tdim(f, 1, [2, 3])

            fits_write_col(f, 1, 1, 1, Int64[1, 2, 3, 4, 5, 6])

            @test fits_read_tdim(f, 1) == [2,3]
        end
    end

    @testset "verification" begin
        tempfitsfile() do f
            fits_create_img(f, Int, (2,2))
            fits_write_pix(f, rand(2,2))
            fits_write_chksum(f)
            @test fits_verify_chksum(f) == (CFITSIO.VERIFIED, CFITSIO.VERIFIED)
            fits_delete_key(f, "CHECKSUM")
            fits_update_chksum(f)
            @test fits_verify_chksum(f) == (CFITSIO.VERIFIED, CFITSIO.VERIFIED)
        end
    end
end
