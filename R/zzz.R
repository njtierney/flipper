.onLoad <- function (libname, pkgname)
{
    packageStartupMessage ("setting up data on CRAN packages ... ",
                           appendLF = FALSE)
    pkgs <- tools::CRAN_package_db ()
    save (pkgs, file = file.path (tempdir (), "pkgs.rda"))

    pkg_txts <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txts <- gsub ("\n", " ", pkg_txts)
    names (pkg_txts) <- pkgs$Package
    save (pkg_txts, file = file.path (tempdir (), "pkg_txts.rda"))

    qpkgs <- quanteda::char_tolower (pkg_txts)
    qpkgs <- quanteda::corpus (qpkgs)
    qpkgs <- quanteda::texts (qpkgs)
    qpkgs <- quanteda::tokens (qpkgs)
    qpkgs <- quanteda::tokens_wordstem (qpkgs)
    save (qpkgs, file = file.path (tempdir (), "qpkgs.rda"))

    pkg_dfm <- quanteda::dfm (pkg_txts,
                              remove = quanteda::stopwords ("english"),
                              stem = TRUE,
                              remove_punct = TRUE,
                              verbose = FALSE)
    save (pkg_dfm, file = file.path (tempdir (), "pkg_dfm.rda"))

    packageStartupMessage ("\rokay, now we're set to flip          ")
}

.onAttach <- function (libname, pkgname)
{
}

.onUnload <- function (libname, pkgname)
{
    file.remove (file.path (tempdir (), "pkgs.rda"))
}
