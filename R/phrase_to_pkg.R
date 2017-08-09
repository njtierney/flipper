#' get_rpkgs
#'
#' Extract package titles and descriptions from the CRAN package db
#'
#' @param pkgs Result of \code{tools::CRAN_package_db}
#' @return A list of texts, one for each package
#' @noRd
get_pkg_txt <- function (pkgs)
{
    pkg_txt <- apply (cbind (pkgs$Title, pkgs$Description), 1, paste,
                      collapse = " ")
    pkg_txt <- gsub ("\n", " ", pkg_txt)
    names (pkg_txt) <- pkgs$Package

    return (pkg_txt)
}

#' get_rcorpus
#'
#' Convert result of \code{get_pkg_txt} to a \code{quanteda::corpus} object
#' 
#' @param pkgs Result of \code{get_pkg_txt}
#' @return Equivalent object as a \code{quanteda::corpus}
#' @noRd
get_rcorpus <- function (pkgs)
{
    pkgs %>% quanteda::char_tolower () %>%
        quanteda::corpus ()
}

#' tokenize_phrase
#'
#' Convert a phrase to a vector of wordstem tokens, minus English-language
#' stopwords.
#'
#' @param aphase A single character string
#' @return Vector of tokens
#' @noRd
tokenize_phrase <- function (aphrase)
{
    tks <- quanteda::tokens (aphrase, remove_punct = TRUE,
                             remove_symbols = TRUE) %>%
                quanteda::tokens_wordstem (language = "english") %>%
                as.character ()
    tks [which (!tks %in% stopwords ("english"))]
}

#' phrase_in_dfm
#' 
#' Get index into documents of those which contain terms in phrase
#'
#' @param mycorpus \code{quanteda::corpus} of package texts
#' @param aphrase tokenized phrase resulting from \code{tokenize_phrase}
#' @return Numberic index of documents in corpus which contain at least two of
#' the tokens in phrase
#' @noRd
phrase_in_dfm <- function (acorpus, aphrase)
{
    adfm <- quanteda::dfm (acorpus,
                           remove = quanteda::stopwords ("english"),
                           stem = TRUE,
                           remove_punct = TRUE,
                           verbose = FALSE)

    aphrase <- aphrase [which (aphrase %in% quanteda::featnames (adfm))]
    indx <- apply (adfm [, aphrase], 1, function (i) sum (i > 0))
    which (indx > 1)
}


#' phrase_to_pkgs
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
#' @note This function is *not* intended to be used like \link{textsearch},
#' rather it is just a helper function that dumps results to screen.
#'
#' @export
phrase_to_pkgs <- function (phrase, screen_dump = TRUE, exact = TRUE, n = 10,
                            open_url = FALSE)
{
    pkgs <- get_pkg_txt ()

    # exact = FALSE not yet implemented
    indx <- which (grepl (phrase, pkgs, ignore.case = TRUE))
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
highlight_phrase <- function (aphrase, txt, col = "black")
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
    txt <- gsub (aphrase, aphrase, txt, ignore.case = TRUE)
    txt <- strsplit (txt, aphrase) [[1]]
    txt_out <- paste0 (col, txt [1], col0)
    for (i in seq (txt) [-1])
    {
        txt_out <- paste0 (txt_out, col_red, aphrase, col0,
                           col, txt [i])
    }
    txt_out <- paste0 (txt_out, col0)

    return (txt_out)
}

#' textsearch
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
textsearch <- function (phrase)
{
    # punctutation and stop words are kept here in order to accurately estimate
    # positions:
    pkgs <- tools::CRAN_package_db ()
    pkg_txts <- get_pkg_txt (pkgs) %>%
                get_rcorpus() %>%
                quanteda::texts () %>%
                quanteda::tokens () %>%
                quanteda::tokens_wordstem ()

    aphrase <- tokenize_phrase (phrase)

    indx <- phrase_in_dfm (pkg_txts, aphrase)
    pkg_txts <- pkg_txts [indx]

    pos <- quanteda::kwic (pkg_txts, aphrase, join = FALSE)
    pos <- data.frame (docname = pos$docname,
                       pos = pos$from,
                       kw  = pos$keyword,
                       stringsAsFactors = FALSE)

    pkg_names <- names (pkg_txts)
    s <- nkw <- rep (NA, length (pkg_names))
    for (i in seq (pkg_names))
    {
        indx <- which (pos$docname == pkg_names [i])
        posi <- split (pos$pos [indx], pos$kw [indx])
        nkw [i] <- length (posi)

        combs <- combn (length (posi), 2)
        dmin <- rep (NA, ncol (combs))
        for (j in seq (ncol (combs)))
        {
            pj1 <- posi [[combs [1, j] ]]
            pj2 <- posi [[combs [2, j] ]]
            dj1 <- matrix (pj1, nrow = length (pj1), ncol = length (pj2))
            dj2 <- t (matrix (pj2, nrow = length (pj2), ncol = length (pj1)))
            dmin [j] <- min (abs (dj1 - dj2))
        }
        s [i] <- max (dmin) + 1
    }

    indx <- order (-nkw, s) # highest #keywords; lowest s
    s <- s [indx]
    nkw <- nkw [indx]
    pkg_names <- pkg_names [indx]

    i <- which (nkw == max (nkw))
    j <- which (s [i] == min (s [i]))
    nm <- pkg_names [i] [j] [sample (length (j), 1)]

    i <- which (pkgs$Package == nm)
    message ("-----", nm, "-----")
    message (pkgs$Title [i])
    message ("-----", rep ("-", nchar (nm)), "-----")
    message (pkgs$Description [i])

    invisible (pkg_names)
}
