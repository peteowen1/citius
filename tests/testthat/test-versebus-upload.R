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
    .vb_pb_upload = function(...) { uploads <<- uploads + 1L; fake_response(201L) }
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
    .vb_pb_upload = function(...) list(fake_response(401L)),  # pb_upload returns a list
    vb_list_assets = function(...) { listed <<- listed + 1L; data.frame(name = character(), size = numeric()) }
  )
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
