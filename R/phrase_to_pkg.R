#' phrase_to_pkg
#'
#' Finds the package that most closely matches a given text phrase.
#'
#' @param phrase of one or more words.
#' @param screen_dump If \code{TRUE}, package titles and descriptions are output to
#' screen in a nicely formatted manner.
#' @param exact If \code{TRUE}, only packages with titles or descriptions which
#' exactly match the given phrase are returned; otherwise the \code{n} best
#' matches are returned.
#' @param n For \code{exact = FALSE}, the number of best matches to be returned.
#' @param open_url If \code{TRUE}, open CRAN web pages of matching packages.
#'
#' @return A \code{tibble} containing package names, titles, and descriptions.
#'
#' @note This function is *not* intended to be used like \link{phrase_to_pkg},
#' rather it is just a helper function that dumps results to screen.
#'
#' @export
phrase_to_pkgs <- function (phrase, screen_dump = TRUE, exact = TRUE, n = 10,
                            open_url = FALSE)
{
    pkgs <- tools::CRAN_package_db()
    pkg_txt <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txt <- gsub ("\n", " ", pkg_txt)
    names (pkg_txt) <- pkgs$Package

    # exact = FALSE not yet implemented
    indx <- which (grepl (phrase, pkg_txt, ignore.case = TRUE))
    pkgs <- cbind (pkgs$Package, pkgs$Title,
                   pkgs$Description) [indx, , drop = FALSE] #nolint
    if (length (indx) > 0)
    {
        pkgs <- tibble::as.tibble (pkgs)
        names (pkgs) <- c ("Package", "Title", "Description")

        if (screen_dump)
        {
            col_black <- "\033[30m"
            col_blue <- "\033[34m"
            col0 <- "\033[39m\033[49m"

            for (i in seq (nrow (pkgs)))
            {
                message (paste0 (col_blue, "-----", pkgs$Package [i], "-----",
                                 col0))
                message (highlight_phrase (phrase, pkgs$Title [i],
                                           col = "blue"))
                d1 <- "------"
                d2 <- paste0 (rep ("-", nchar (pkgs$Package [i])),
                              collapse = "")
                message (paste0 (col_blue, d1, d2, d1, col0))
                message (highlight_phrase (phrase, pkgs$Description [i],
                                           col = "black"))
                message (paste0 (col_black, d1, d2, d1, col0, "\n"))

                if (open_url)
                {
                    pkg_url <- paste0 ("https://cran.r-project.org/package=",
                                       pkgs$Package [i])
                    browseURL (pkg_url)
                }
            }
        }
    }

    invisible (pkgs)
}

#' highlight_phrase
#'
#' highlights specified \code{phrase} in blue while remining text is printed in
#' \code{col}
#' @noRd
highlight_phrase <- function (phrase, txt, col = "black")
{
    if (col == "black")
        col <- "\033[30m\033[47m"
    else if (col == "red")
        col <- "\033[31m\033[47m"
    else if (col == "green")
        col <- "\033[32m\033[47m"
    else if (col == "blue")
        col <- "\033[34m\033[47m"

    col_red <- "\033[31m\033[1m\033[43m" # 1m = bold; 43m = Yellow BG
    col0 <- "\033[22m\033[39m\033[49m" # 22m = normal weight; 49m = normal BG

    # subsitute case of phrase exactly as given:
    txt <- gsub (phrase, phrase, txt, ignore.case = TRUE)
    txt <- strsplit (txt, phrase) [[1]]
    txt_out <- paste0 (col, txt [1], col0)
    for (i in seq (txt) [-1])
    {
        txt_out <- paste0 (txt_out, col_red, phrase, col0,
                           col, txt [i])
    }
    txt_out <- paste0 (txt_out, col0)

    return (txt_out)
}

#' phrase_to_pkg
#'
#' Finds the package that most closely matches a given text phrase.
#'
#' @param phrase of one or more words.
#'
#' @return At present nothing; just prints package title and description to
#' screen.
#'
#' @note This function is intended to extract a single package from which to
#' start *flipping*.
#'
#' @export
phrase_to_pkg <- function (phrase)
{
    pkgs <- tools::CRAN_package_db()
    pkg_txt <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txt <- gsub ("\n", " ", pkg_txt)
    names (pkg_txt) <- pkgs$Package

    pkg_corpus <- pkg_txt %>% quanteda::char_tolower () %>%
                    quanteda::corpus ()
    pkg_dfm <- quanteda::dfm (pkg_corpus,
                              remove = quanteda::stopwords ("english"),
                              stem = TRUE,
                              remove_punct = TRUE,
                              verbose = FALSE)

    phrase <- quanteda::tokens (phrase, remove_punct = TRUE,
                                remove_symbols = TRUE) %>%
                quanteda::tokens_wordstem () %>%
                as.character ()

    phrase <- phrase [which (phrase %in% quanteda::featnames (pkg_dfm))]
    pkg_dfm2 <- pkg_dfm [, phrase]
    indx <- apply (pkg_dfm2, 1, function (i) sum (i > 0))
    pkg_dfm2 <- pkg_dfm2 [which (indx == max (indx)), ]

    pkg_corpus2 <- pkg_corpus [which (indx == max (indx))]

    pkg_names <- quanteda::docnames (pkg_dfm2)
    s <- rep (NA, length (pkg_names))
    for (i in seq (pkg_names))
    {
        pos <- sapply (phrase, function (j)
                       quanteda::kwic (pkg_corpus2 [[i]], j,
                                       valuetype = "regex")$from)
        npos <- which (lapply (pos, length) == 0)
        pos [npos] <- NULL

        combs <- combn (length (pos), 2)
        dmin <- rep (NA, ncol (combs))
        for (j in seq (ncol (combs)))
        {
            pj1 <- pos [[combs [1, j] ]]
            pj2 <- pos [[combs [2, j] ]]
            dj1 <- matrix (pj1, nrow = length (pj1), ncol = length (pj2))
            dj2 <- t (matrix (pj2, nrow = length (pj2), ncol = length (pj1)))
            dmin [j] <- min (abs (dj1 - dj2))
        }
        s [i] <- max (dmin) + 1
    }

    nm <- pkg_names [which.min (s)]

    i <- which (pkgs$Package == nm)
    message ("-----", nm, "-----")
    message (pkgs$Title [i])
    message ("-----", rep ("-", nchar (nm)), "-----")
    message (pkgs$Description [i])
}
