#' memoised CRAN package database
#' @noRd
m_get_cran_pkgs <- memoise::memoise (function () tools::CRAN_package_db ())

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
m_get_pkg_txt <- memoise::memoise (get_pkg_txt)

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
m_get_rcorpus <- memoise::memoise (get_rcorpus)

#' get_corpus_tokens
#'
#' Convert corpus to tokens
#'
#' @param pkg_txts Result of code{get_pkg_txt}
#' @return Equivalent corpus in tokenized form
#' @noRd
get_corpus_tokens <- function (pkg_txts)
{
    pkg_txts %>%
        quanteda::char_tolower () %>%
        quanteda::corpus () %>%
        quanteda::texts () %>%
        quanteda::tokens () %>%
        quanteda::tokens_wordstem ()
}
m_get_corpus_tokens <- memoise::memoise (get_corpus_tokens)

#' get_package_dfm
#'
#' Get code{quanteta} Document Frequency Matrix (\code{dfm}) for all R packages
#'
#' @param pkg_txts Result of code{get_pkg_txt}
#' @return Equivalent \code{dfm}
#' @noRd
get_package_dfm <- function (pkg_txts)
{
    quanteda::dfm (pkg_txts,
                   remove = quanteda::stopwords ("english"),
                   stem = TRUE,
                   remove_punct = TRUE,
                   verbose = FALSE)
}
m_get_package_dfm <- memoise::memoise (get_package_dfm)


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
phrase_in_dfm <- function (pkg_dfm, aphrase)
{
    aphrase <- aphrase [which (aphrase %in% quanteda::featnames (pkg_dfm))]
    indx <- apply (pkg_dfm [, aphrase], 1, function (i) sum (i > 0))
    which (indx > 1 | indx >= length (aphrase))
}



#' highlight_phrase
#'
#' highlights specified \code{phrase} in blue while remining text is printed in
#' \code{col}
#' @note Not used at present
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

#' text_to_pkgs
#'
#' Order names of CRAN packages according to best matches with \code{phrase}
#'
#' @param phrase A text phrase of arbitrary length
#' @return \code{data.frame} containing minimal lengths of phrases containing
#' word stems of phrase; numbers of tokens (\code{key_words}) matched; and names
#' of corresponding packages. Results are sorted according to (potentially
#' equal) best matches first.
#' @noRd
text_to_pkgs <- function (phrase)
{
    if (!memoise::has_cache (m_get_cran_pkgs)())
        message (paste0 ("The first textsearch takes a short while to ",
                         "set up; subsequent calls will be much quicker"))
    pkgs <- m_get_cran_pkgs ()
    pkg_txts <- m_get_pkg_txt (pkgs)
    pkg_dfm <- m_get_package_dfm (pkg_txts)
    pkg_tokens <- m_get_corpus_tokens (pkg_txts)

    aphrase <- tokenize_phrase (phrase)

    indx <- phrase_in_dfm (pkg_dfm, aphrase)
    pkg_tokens <- pkg_tokens [indx]

    pos <- quanteda::kwic (pkg_tokens, aphrase, join = FALSE)
    pos <- data.frame (docname = pos$docname,
                       pos = pos$from,
                       kw  = pos$keyword,
                       stringsAsFactors = FALSE)

    pkg_names <- names (pkg_tokens)
    phrase_len <- nkw <- rep (NA, length (pkg_names))
    for (i in seq (pkg_names))
    {
        indx <- which (pos$docname == pkg_names [i])
        posi <- split (pos$pos [indx], pos$kw [indx])
        nkw [i] <- length (posi)

        if (length (posi) == 1)
        {
            # For single-token phrases, use first position as substitute for
            # minimal phrase length, so earlier positions are preferred. Keyword
            # can still occur multiple times, so min is necessary
            phrase_len [i] <- min (posi [[1]])
        } else
        {
            combs <- combn (length (posi), 2)
            dmin <- rep (NA, ncol (combs))
            for (j in seq (ncol (combs)))
            {
                pj1 <- posi [[combs [1, j] ]]
                pj2 <- posi [[combs [2, j] ]]
                dj1 <- matrix (pj1, nrow = length (pj1), ncol = length (pj2))
                dj2 <- t (matrix (pj2, nrow = length (pj2),
                                  ncol = length (pj1)))
                dmin [j] <- min (abs (dj1 - dj2))
            }
            phrase_len [i] <- max (dmin) + 1
        }
    }

    indx <- order (-nkw, phrase_len) # highest #keywords; lowest s

    data.frame (min_phrase_len = phrase_len [indx],
                num_key_words = nkw [indx],
                pkg_names = pkg_names [indx],
                stringsAsFactors = FALSE)
}

#' one_random_pkg
#'
#' @param packages Result of \code{text_to_pkgs} function containing sorted
#' \code{data.frame} of minimal phrase lengths, numbers of matched key words,
#' and package names.
#' @return Single random package chosen from potentially multiple best matches.
#' @noRd
one_random_pkg <- function (packages)
{

    i <- which (packages$num_key_words == max (packages$num_key_words))
    j <- which (packages$min_phrase_len [i] ==
                min (packages$min_phrase_len [i]))
    packages$pkg_names [i] [j] [sample (length (j), 1)]
}

#' print_pkg
#'
#' Dump title and description of one R package to screen
#'
#' @param pkg name of package
#' @noRd
print_pkg <- function (pkg)
{
    pkgs <- m_get_cran_pkgs () # memoised here
    i <- which (pkgs$Package == pkg)
    if (length (i) == 0)
        stop (paste0 ("R Package ", pkg, " does not exist"))

    pkgs <- cbind (pkgs$Package, pkgs$Title,
                   pkgs$Description) [i, , drop = FALSE] #nolint
    pkgs <- tibble::as.tibble (pkgs)
    names (pkgs) <- c ("Package", "Title", "Description")

    col_blue <- "\033[34m"
    col_green <- "\033[32m"
    col0 <- "\033[39m\033[49m" # 49m = normal BG

    message (paste0 (col_green, "-----", pkgs$Package, "-----",
                     col0))
    message (paste0 (col_green, "Title: ", pkgs$Title, col0))
    d1 <- "------"
    d2 <- paste0 (rep ("-", nchar (pkgs$Package)), collapse = "")
    message (paste0 (col_green, d1, d2, d1, col0))
    message (paste0 (col_blue, "Description: ", pkgs$Description, col0))
    message (paste0 (col_green, d1, d2, d1, col0, "\n"))
}


#' textsearch
#'
#' Finds the package that most closely matches a given text phrase.
#'
#' @param phrase of one or more words.
#' @param open_url If \code{TRUE}, open CRAN web pages of matching packages.
#'
#' @return At present nothing; just prints package title and description to
#' screen.
#'
#' @note This function is intended to extract a single package from which to
#' start *flipping*.
#'
#' @export
textsearch <- function (phrase, open_url = FALSE)
{
    packages <- text_to_pkgs (phrase)
    pkg_name <- one_random_pkg (packages)

    print_pkg (pkg_name)

    pkgs <- m_get_cran_pkgs () # memoised here
    i <- which (pkgs$Package == pkg_name)
    pkgs <- cbind (pkgs$Package, pkgs$Title,
                   pkgs$Description) [i, , drop = FALSE] #nolint
    if (open_url)
    {
        pkg_url <- paste0 ("https://cran.r-project.org/package=",
                           pkgs$Package)
        browseURL (pkg_url)
    }
}
