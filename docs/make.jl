using CFITSIO
using Documenter
using Documenter.Remotes: GitHub
DocMeta.setdocmeta!(CFITSIO, :DocTestSetup, :(using CFITSIO); recursive=true)

include("pages.jl")
makedocs(;
    modules=[CFITSIO],
    authors="JuliaAstro",
    repo=GitHub("JuliaAstro/CFITSIO.jl"),
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
