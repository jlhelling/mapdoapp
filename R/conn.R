con <- DBI::dbConnect(RPostgres::Postgres(),
            host   = "localhost",
            dbname = "dbmapdo",
            user      = Sys.getenv("DBMAPDO_LOCALHOST_USER"),
            password  = Sys.getenv("DBMAPDO_LOCALHOST_PASS"),
            port     = 5432)
