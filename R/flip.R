#' flip
#'
#' Console-only sketch of eventual shiny flipper functionality
#'
#' @param phrase of one or more words to get things started
#'
#' @export
flip <- function (phrase)
{
    packages <- text_to_pkgs (phrase)
    print_pkg (packages$pkg_names [1])
    y <- menu( c("Yes", "No", "Maybe"), title = "Good package?")

    pout <- NULL
    if (y == 1)
        pout <- packages$pkg_names [1]

    i <- 2
    while (y %in% c (1, 3) & i <= nrow (packages))
    {
        print_pkg (packages$pkg_names [i])
        y <- menu( c("Yes", "No", "Maybe"), title = "Good package?")
        if (y == 1)
            pout <- c (pout, packages$pkg_names [i])
        i <- i + 1
    }

    pkgs <- m_get_cran_pkgs () # memoised here
    pkgs <- cbind (pkgs$Package, pkgs$Title, pkgs$Description)
    pkgs <- tibble::as.tibble (pkgs)
    names (pkgs) <- c ("Package", "Title", "Description")

    invisible (pkgs [match (pout, pkgs$Package), ])
}
