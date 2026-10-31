# vb_publish must fail at the upload, not 95s later at the listing check, when
# GitHub refuses the upload. citiusdata run 38487753749 (2026-10-14): the
# workflow set only GH_TOKEN, piggyback uploaded with an empty token, and
# piggyback 0.1.5 returned the 401 response as if it had worked.

local_asset <- function(env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".rds", .local_envir = env)
  saveRDS(1:3, p)
  p
}

fake_response <- function(status) structure(list(status_code = status), class = "response")

test_that("vb_publish refuses to upload without a token", {
  skip_if_not_installed("digest")
  uploads <- 0L
  local_mocked_bindings(
    .vb_token = function() "",
    .vb_safe_upload = function(...) { uploads <<- uploads + 1L; invisible(1) }
  )
  expect_error(
    vb_publish(local_asset(), repo = "o/r", tag = "t", carry_forward = FALSE),
    class = "vb_error_auth"
  )
  expect_identical(uploads, 0L)
})

test_that("a refused upload fails as an upload failure, before any listing check", {
  skip_if_not_installed("digest")
  listed <- 0L
  local_mocked_bindings(
    .vb_token = function() "ghp_test",
    vb_list_assets = function(...) { listed <<- listed + 1L; data.frame(name = character(), size = numeric()) }
  )
  # versebus 1.2.0: the refusal is caught inside .vb_safe_upload(), right
  # after piggyback returns it (pb_upload returns a list of responses).
  local_mocked_bindings(pb_upload = function(...) list(fake_response(401L)), .package = "piggyback")
  err <- expect_error(
    vb_publish(local_asset(), repo = "o/r", tag = "t", carry_forward = FALSE, max_retries = 0),
    class = "vb_error_transient"
  )
  expect_match(conditionMessage(err), "upload\\(s\\) failed")
  expect_identical(listed, 0L)
})

test_that(".vb_refused_status reads single responses, lists and non-responses", {
  expect_identical(.vb_refused_status(fake_response(201L)), integer(0))
  expect_identical(.vb_refused_status(fake_response(422L)), 422L)
  expect_identical(.vb_refused_status(list(fake_response(200L), fake_response(403L))), 403L)
  # piggyback returns NULL when it skips a file on purpose; that is not a refusal
  expect_identical(.vb_refused_status(NULL), integer(0))
  expect_identical(.vb_refused_status(list(NULL)), integer(0))
})

# --- 2026-10-25: gh's own HTTP cache (port of torp#271) ----------------------
# gh caches every GET for GitHub's max-age=60, so the post-upload verify could
# re-read a pre-upload listing for up to a minute. vb_publish turns it off via
# .vb_with_fresh_listing() and restores the caller's setting.

test_that(".vb_with_fresh_listing turns gh's HTTP cache off and restores it", {
  withr::local_options(gh_cache = TRUE)
  restore <- .vb_with_fresh_listing()
  expect_false(getOption("gh_cache"))
  restore()
  expect_true(getOption("gh_cache"))
})

test_that("vb_publish's post-upload verify reads the listing with gh's cache off", {
  skip_if_not_installed("digest")
  withr::local_options(gh_cache = TRUE)
  p <- local_asset()
  seen <- logical(0)
  local_mocked_bindings(
    .vb_token = function() "ghp_test",
    .vb_safe_upload = function(...) invisible(1),
    vb_list_assets = function(...) {
      seen <<- c(seen, isFALSE(getOption("gh_cache")))
      data.frame(name = basename(p), size = as.numeric(file.size(p)), stringsAsFactors = FALSE)
    }
  )
  suppressMessages(vb_publish(p, repo = "o/r", tag = "t", carry_forward = FALSE))
  expect_true(length(seen) > 0L)
  expect_true(all(seen))
  expect_true(getOption("gh_cache"))
})
