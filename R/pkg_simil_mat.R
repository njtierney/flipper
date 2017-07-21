#' pkg_simil_mat
#'
#' Generate a similarity matrix between packages based on their textual
#' descriptions
#'
#' @param verbose If \code{TRUE}, display progress messages
#'
#' @export
pkg_simil_mat <- function (verbose = FALSE)
{
    if (verbose)
        message ('Extracting package information from CRAN ...')
    pkgs <- tools::CRAN_package_db()
    pkg_txt <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txt <- gsub ("\n", " ", pkg_txt)
    names (pkg_txt) <- pkgs$Package
    pkg_corpus <- quanteda::corpus (pkg_txt)
    pkg_dfm <- quanteda::dfm (pkg_corpus,
                              remove = quanteda::stopwords ("english"),
                              stem = TRUE,
                              remove_punct = TRUE,
                              verbose = verbose)
    if (verbose)
        message (paste0 ('Generating similarity matrix between ',
                         'package descriptions ...'))
    quanteda::textstat_simil (pkg_dfm, names (pkg_txt))
    #1 - quanteda::textstat_dist (pkg_dfm, names (pkg_txt))
    #quanteda::textstat_simil (pkg_dfm, names (pkg_txt),
    #                          method = "cosine")
}
