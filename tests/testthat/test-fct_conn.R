test_that("db_con is a Postgresql connection", {
  con <- db_con()

  expect_true(inherits(con, "PqConnection"),
              info = "Connection object is of class PqConnection")

  expect_true(DBI::dbIsValid(con),
              info = "Connection is open")

  DBI::dbDisconnect(con)
})
