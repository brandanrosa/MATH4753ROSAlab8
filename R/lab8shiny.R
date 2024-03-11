#' lab8shiny
#'
#' A shiny app which takes a chosen number of iterations, and simulates an objective function B, where B~Gamma(2,1)
#'
#' @return A web-browser app to simulate the density of the objective function compared to the theoretical where the number of iterations are user-chosen
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#' \dontrun{lab8shiny()}
lab8shiny <- function(){
  runApp(system.file("lab8shiny",
                            package = "MATH4753ROSAlab8"),
                launch.browser = TRUE)
}
