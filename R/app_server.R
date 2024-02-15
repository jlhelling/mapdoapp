#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbDisconnect
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  # track session
  track_session(session = session)

  # set database connection
  con <- db_con()

  # Your application server logic
  mod_explore_server("explore_1", con)
  mod_documentation_server("documentation_1")

  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
