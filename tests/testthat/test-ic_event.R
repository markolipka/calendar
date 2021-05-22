context("test-test-ic_event")

test_that("error if start and end is NA", {
  expect_error(ic_event(start_time = NA, end_time = NA))
})

test_that("error if start_time is not date, datetime or character", {
  expect_error(ic_event(start_time = 1))
  expect_error(ic_event(start_time = TRUE))
})


test_that("warning if start is missing or wrong format", {
  expect_warning(ic_event(start_time = ""))
  expect_warning(ic_event(start_time = NA))
})


test_that("warning if start or end is empty string", {
  expect_warning(ic_event(start_time = Sys.time(), end_time = ""))
  expect_warning(ic_event(start_time = "", end_time = Sys.time()))
})

test_that("warning if start or end string does not match date format", {
  expect_warning(ic_event(start_time = "2021-NA-NA"))
  expect_warning(ic_event(end_time = "2021-NA-NA"))
})


test_that("default values work", {
  event <- ic_event()
  expect_equal(class(event), c("ical", "tbl_df", "tbl", "data.frame"))
  expect_equal(class(event$DTSTART), c("POSIXct", "POSIXt"))
  expect_equal(class(event$DTEND), c("POSIXct", "POSIXt"))
  expect_equal(event$SUMMARY, "ical event")
})

test_that("2018-10-12 15:00, end_time = 2018-10-13 15:00 matches default format", {
  event <- ic_event(start_time = "2018-10-12 15:00", end_time = "2018-10-13 15:00")
  expect_equal(class(event), c("ical", "tbl_df", "tbl", "data.frame"))
  expect_equal(event$DTSTART, as.POSIXct("2018-10-12 15:00:00 CEST"))
  expect_equal(event$DTEND,   as.POSIXct("2018-10-13 15:00:00 CEST"))
})

test_that("end_time is set to 1 hour after start_time (datetime) if NA", {
  expect_warning(event <- ic_event(start_time = "2018-10-12 15:00", end_time = NA))
  expect_equal(event$DTEND, event$DTSTART + lubridate::hours(1))
})

test_that("event creation works via number of hours over midnight", {
  event <- ic_event(start_time = "2018-10-12 23:00", end_time = 2)
  expect_equal(event$DTEND, event$DTSTART + lubridate::hours(2))
})

### all day events:

test_that("end_time is set to same day as start_time (date) if NA", {
  event <- ic_event(start_time = as.Date("2018-10-12"), end_time = NA)
  expect_equal(event$`DTEND;VALUE=DATE`, event$`DTSTART;VALUE=DATE`)
})

test_that("all day event creation works (single day case)", {
  event <- ic_event(start_time = as.Date("2018-10-12"))
  expect_equal(event$`DTEND;VALUE=DATE`, event$`DTSTART;VALUE=DATE`)
})

test_that("all day event creation works (multi-day case via number of days)", {
  event <- ic_event(start_time = as.Date("2018-10-12"), end_time = 3)
  expect_equal(event$`DTEND;VALUE=DATE`, event$`DTSTART;VALUE=DATE` + lubridate::days(2))
})

test_that("all day event creation works (multi-day case via end date definition)", {
  event <- ic_event(start_time = as.Date("2018-10-12"), end_time = as.Date("2018-10-14"))
  expect_equal(event$`DTEND;VALUE=DATE`, event$`DTSTART;VALUE=DATE` + lubridate::days(2))
})
