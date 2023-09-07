#' Connection to postgresql database
#'
#' @return connection
#' @export
#'
#' @examples
#' data <- st_read(dsn = db_con(), query = query)
db_con <- function(){
  con <- DBI::dbConnect(RPostgres::Postgres(),
                 host   = "localhost",
                 dbname = "dbmapdo",
                 user      = Sys.getenv("DBMAPDO_LOCALHOST_USER"),
                 password  = Sys.getenv("DBMAPDO_LOCALHOST_PASS"),
                 port     = 5432)
  return(con)
}
