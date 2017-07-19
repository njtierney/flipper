#' pkg_simil_mat
#'
#' Generate a similarity matrix between packages based on their textual
#' descriptions
#'
#' @export
pkg_simil_mat <- function ()
{
    pkgs <- tools::CRAN_package_db()
    pkg_txt <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txt <- gsub ("\n", " ", pkg_txt)
    names (pkg_txt) <- pkgs$Package
    pkg_corpus <- quanteda::corpus (pkg_txt)
    #summary (pkg_corpus)
    pkg_dfm <- quanteda::dfm (pkg_corpus,
                              remove = quanteda::stopwords ("english"),
                              stem = TRUE,
                              remove_punct = TRUE,
                              verbose = TRUE)
    quanteda::textstat_simil (pkg_dfm, names (pkg_txt))
}
