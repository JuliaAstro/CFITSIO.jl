using CFITSIO
using Documenter

makedocs(;
    modules=[CFITSIO],
    authors="Miles Lucas <mdlucas@hawaii.edu> and contributors",
    repo="https://github.com/mileslucas/CFITSIO.jl/blob/{commit}{path}#L{line}",
    sitename="CFITSIO.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://mileslucas.github.io/CFITSIO.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/mileslucas/CFITSIO.jl",
)
