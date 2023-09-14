#' Connection to postgresql database
#'
#' @return connection
#' @export
#'
#' @examples
#' data <- st_read(dsn = db_con(), query = query)
db_con <- function(){
  con <- DBI::dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv("DBMAPDO_HOST"),
                 port = Sys.getenv("DBMAPDO_PORT"),
                 dbname = Sys.getenv("DBMAPDO_NAME"),
                 user      = Sys.getenv("DBMAPDO_USER"),
                 password  = Sys.getenv("DBMAPDO_PASS"))
  return(con)
}
