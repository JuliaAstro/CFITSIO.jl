using CFITSIO
using Documenter
DocMeta.setdocmeta!(CFITSIO, :DocTestSetup, :(using CFITSIO); recursive=true)

include("pages.jl")
makedocs(;
    modules=[CFITSIO],
    authors="JuliaAstro",
    repo="https://github.com/JuliaAstro/CFITSIO.jl/blob/{commit}{path}#L{line}",
    sitename="CFITSIO.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://juliaastro.github.io/CFITSIO.jl",
        assets=String[],
    ),
    pages=pages
)

deploydocs(;
    repo="github.com/JuliaAstro/CFITSIO.jl",
)
