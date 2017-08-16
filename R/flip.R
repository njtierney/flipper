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
    y <- menu( c("Yes", "No", "Maybe"), title="Good package?")

    i <- 2
    while (y %in% c (1, 3) & i <= nrow (packages))
    {
        print_pkg (packages$pkg_names [i])
        y <- menu( c("Yes", "No", "Maybe"), title="Good package?")
        i <- i + 1
    }
}
