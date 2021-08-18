using CFITSIO
using Test
using Aqua

function tempfitsfile(fn)
    mktempdir() do dir
        filename = joinpath(dir, "temp.fits")
        fitsfile = fits_clobber_file(filename)
        fn(fitsfile)

        if fitsfile.ptr != C_NULL
            # write some data to file to avoid errors on closing
            data = ones(1)
            fits_create_img(fitsfile, data)
            fits_write_pix(fitsfile, data)
            fits_delete_file(fitsfile)
        end
    end
end

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
            @test fits_file_mode(f) == 0 == Int(CFITSIO.R)
            close(f)

            f = fits_open_file(filename, CFITSIO.R)
            @test fits_file_mode(f) == 0 == Int(CFITSIO.R)
            close(f)

            f = fits_open_file(filename, 1)
            @test fits_file_mode(f) == 1 == Int(CFITSIO.RW)
            close(f)

            f = fits_open_file(filename, CFITSIO.RW)
            @test fits_file_mode(f) == 1 == Int(CFITSIO.RW)
            close(f)
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

            a = ones(3,3)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)

            a = ones(4,4)
            fits_create_img(f, eltype(a), [size(a)...])
            fits_write_pix(f, a)

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

                @test_throws Exception fn(f)
            end

            for fn in [fits_read_key_str, fits_read_key_lng, fits_read_keyword, fits_write_comment,
                fits_write_history, fits_write_record, fits_delete_key, fits_movnam_hdu, fits_get_colnum,
                ]

                @test_throws Exception fn(f, "abc")
            end

            for fn in [fits_read_record, fits_read_keyn, fits_delete_record,
                fits_movabs_hdu, fits_movrel_hdu, fits_get_coltype,
                fits_get_eqcoltype, fits_read_tdim, ]

                @test_throws Exception fn(f, 1)
            end

            for fn in [fits_insert_rows, fits_delete_rows, fits_read_descript, CFITSIO.fits_write_null_img]
                @test_throws Exception fn(f, 1, 2)
            end

            for fn in [fits_write_pix, fits_read_pix]
                @test_throws Exception fn(f, a)
            end
            for fn in [fits_write_pix, fits_read_pix]
                @test_throws Exception fn(f, [1,1], length(a), a)
            end

            @test_throws Exception fits_read_pix(f, ones(Int, ndims(a)), length(a), zero(eltype(a)),  a)

            @test_throws Exception fits_read_keys_lng(f, "a", 1, 2)

            for fn in [fits_write_key, fits_update_key]
                @test_throws Exception fn(f, "a", 1, "b")
            end

            for fn in [fits_read_col, fits_write_col]
                @test_throws Exception fn(f, 1, 1, 1, ["abc"])
                @test_throws Exception fn(f, 1, 1, 1, ["abc", 1])
            end

            for fn in [fits_create_binary_tbl, fits_create_ascii_tbl]
                @test_throws Exception fn(f, 1, [("name", "3D", "c")], "extname")
            end

            @test_throws Exception fits_update_key(f, "a", 1.0, "b")
            @test_throws Exception fits_update_key(f, "a", nothing, "b")
            @test_throws Exception fits_write_tdim(f, 1, [1, 2])

            @test_throws Exception fits_read_subset(f, [1,1], [2,2], [1,1], a)
            @test_throws Exception fits_write_subset(f, [1,1], [2,2], a)

            @test_throws Exception fits_create_img(f, Int64, [2,3])

            tempfitsfile() do f2
                fits_create_img(f2, eltype(a), [size(a)...])
                fits_write_pix(f2, a)
                close(f2)

                @test_throws Exception fits_copy_image_section(f, f2, "1:2")
                @test_throws Exception fits_copy_image_section(f2, f, "1:2")
            end
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
        tempfitsfile() do f
            a = zeros(2,2)
            @test_throws ErrorException fits_read_pix(f, a)
            @test_throws ErrorException fits_read_pix(f, a, 1)
            @test_throws ErrorException fits_read_pixnull(f, a, similar(a, UInt8))
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
            a = Float64[1 3; 2 4]
            b = similar(a); c = similar(a);

            @testset "create" begin
                fits_create_img(f, eltype(a), size(a))
                fits_write_pix(f, a)
                fits_read_pix(f, b)
                fits_create_img(f, eltype(a), [size(a)...])
                fits_write_pix(f, a)
                fits_read_pix(f, c)
                @test b == c
            end
            @testset "write" begin
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
                    fits_read_pix(f, b)
                    @test b == a
                    # test that vectors and tuples of pixels behave identically
                    fits_read_pix(f, [1,1], length(b), b)
                    fits_read_pix(f, (1,1), length(c), c)
                    @test b == c
                end
                @testset "subset" begin
                    b .= 0
                    # test that vectors and tuples of pixels behave identically
                    fits_read_subset(f, (1,1), (2,1), (1,1), @view b[:,1])
                    fits_read_subset(f, [1,1], [2,1], [1,1], @view b[:,2])
                    @test @views b[:,1] == b[:,2]
                end
            end
            @testset "subset" begin
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
end
