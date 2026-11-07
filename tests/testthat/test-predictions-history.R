# Dated prediction history (issue #32): one new file per publish, never rewritten.

preds <- function() data.frame(
  verse = "citius", event = "Men 100m final", market = "win",
  selection = c("Lyles", "Kerley"), probability = c(0.4, 0.2),
  stringsAsFactors = FALSE
)
t1 <- as.POSIXct("2026-11-05 03:04:05", tz = "UTC")

test_that("file name is the UTC publish time to the second", {
  expect_identical(prediction_history_name(t1), "predictions_20261105T030405Z.parquet")
  # same instant expressed in another zone gives the same name
  t_ny <- as.POSIXct(format(t1, tz = "America/New_York"), tz = "America/New_York")
  expect_identical(prediction_history_name(t_ny), prediction_history_name(t1))
})

test_that("table has the minimum columns first, stamped, extras kept", {
  p <- preds(); p$note <- "x"; p$model_version <- "stale"
  dt <- prediction_history_table(p, "v9", t1)
  expect_identical(names(dt), c("verse", "event", "market", "selection",
                                "probability", "published_utc", "model_version", "note"))
  expect_identical(unique(dt$model_version), "v9")
  expect_equal(as.numeric(unique(dt$published_utc)), as.numeric(t1))
  expect_identical(attr(dt$published_utc, "tzone"), "UTC")
  expect_identical(dt$selection, c("Lyles", "Kerley"))
  expect_identical(names(p)[1], "verse")  # input untouched
  expect_identical(p$model_version[1], "stale")
})

test_that("bad input is refused, not repaired", {
  expect_error(prediction_history_table(preds()[, -4], "v", t1), "selection")
  p <- preds(); p$probability[1] <- 1.2
  expect_error(prediction_history_table(p, "v", t1), "probability")
  p <- preds(); p$probability[1] <- NA
  expect_error(prediction_history_table(p, "v", t1), "probability")
  p <- preds(); p$event[2] <- ""
  expect_error(prediction_history_table(p, "v", t1), "event")
  expect_error(prediction_history_table(preds()[0, ], "v", t1), "no rows")
})

test_that("publishing refuses to overwrite an existing dated file", {
  skip_if_not_installed("arrow"); skip_if_not_installed("piggyback")
  skip_if_not_installed("gh"); skip_if_not_installed("digest")
  uploaded <- 0L
  local_mocked_bindings(
    vb_list_assets = function(...) data.frame(name = prediction_history_name(t1), size = 1),
    vb_publish = function(...) { uploaded <<- uploaded + 1L; invisible(NULL) }
  )
  expect_error(publish_prediction_history(preds(), "o/r", "v", published = t1),
               class = "vb_error_integrity")
  expect_identical(uploaded, 0L)
})

test_that("a new publish uploads one new parquet with the required columns", {
  skip_if_not_installed("arrow"); skip_if_not_installed("piggyback")
  skip_if_not_installed("gh"); skip_if_not_installed("digest")
  seen <- NULL
  local_mocked_bindings(
    vb_list_assets = function(...) data.frame(name = "predictions_20261104T000000Z.parquet", size = 1),
    vb_publish = function(paths, repo, tag, rows, ...) {
      seen <<- list(name = basename(paths), tag = tag, rows = rows,
                    data = arrow::read_parquet(paths))
      invisible(NULL)
    }
  )
  out <- publish_prediction_history(preds(), "o/r", "v1", published = t1)
  expect_identical(out, "predictions_20261105T030405Z.parquet")
  expect_identical(seen$name, out)
  expect_identical(seen$tag, "predictions-history")
  expect_identical(seen$rows[[out]], 2L)
  expect_true(all(c("verse", "event", "market", "selection", "probability",
                    "published_utc", "model_version") %in% names(seen$data)))
})

test_that("an absent release is created, and dry_run uploads nothing", {
  skip_if_not_installed("arrow"); skip_if_not_installed("piggyback")
  skip_if_not_installed("gh"); skip_if_not_installed("digest")
  created <- 0L; uploaded <- 0L
  local_mocked_bindings(
    vb_list_assets = function(...) stop(structure(class = c("vb_error_absent", "vb_error", "error", "condition"),
                                                  list(message = "no tag", call = NULL))),
    vb_publish = function(...) { uploaded <<- uploaded + 1L; invisible(NULL) }
  )
  local_mocked_bindings(pb_new_release = function(...) { created <<- created + 1L; invisible(NULL) },
                        .package = "piggyback")
  publish_prediction_history(preds(), "o/r", "v", published = t1, dry_run = TRUE)
  expect_identical(c(created, uploaded), c(0L, 0L))
  publish_prediction_history(preds(), "o/r", "v", published = t1)
  expect_identical(c(created, uploaded), c(1L, 1L))
})

test_that("publish_with_history records history only after a successful live upload", {
  calls <- character()
  hist <- function(...) { calls <<- c(calls, "history"); "predictions_x.parquet" }
  res <- publish_with_history(function() { calls <<- c(calls, "live"); "ok" },
                              data.frame(), repo = "o/r", history_fn = hist)
  expect_identical(calls, c("live", "history"))
  expect_identical(attr(res, "history"), "predictions_x.parquet")
})

test_that("publish_with_history skips history when the live upload fails", {
  calls <- character()
  hist <- function(...) { calls <<- c(calls, "history"); "x" }
  expect_error(publish_with_history(function() stop("upload failed"),
                                    data.frame(), repo = "o/r", history_fn = hist),
               "upload failed")
  expect_identical(calls, character())
})

test_that("a history failure does not fail the live publish", {
  res <- publish_with_history(function() "live-ok", data.frame(), repo = "o/r",
                              history_fn = function(...) stop("release down"))
  expect_identical(as.character(res), "live-ok")
  expect_null(attr(res, "history"))
})
