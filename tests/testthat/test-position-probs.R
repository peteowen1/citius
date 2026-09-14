# Per-position finishing probabilities.
#
# The properties pinned are the ones that would let a wrong implementation
# look right: that the columns are a probability DISTRIBUTION over positions
# (so they agree with medal_probs() by construction rather than by luck), that
# a field smaller than k still emits k columns (so races of different sizes
# bind without a fill rule), and that the cap loses nothing recoverable.

fake_sim <- function(rank_mat) {
  # medal_probs() takes a median over `perf` to report median_mark, so the
  # fixture has to carry a performance matrix even for tests that only read
  # ranks. Deriving it as -rank keeps the two consistent: higher perf is
  # better, which is what rank 1 means.
  p <- -as.numeric(rank_mat)
  dim(p) <- dim(rank_mat)
  dimnames(p) <- dimnames(rank_mat)
  structure(list(rank = rank_mat, perf = p, perf_std = p, orientation = -1L),
            class = "citius_sim")
}

# Three athletes, four simulations, known ranks.
rm3 <- function() {
  m <- rbind(c(1L, 2L, 3L),
             c(1L, 3L, 2L),
             c(2L, 1L, 3L),
             c(3L, 1L, 2L))
  colnames(m) <- c("a", "b", "c")
  m
}

test_that("positions are a distribution that agrees with medal_probs", {
  p <- position_probs(fake_sim(rm3()), k = 3L)
  # a wins 2 of 4, is 2nd once, 3rd once
  pa <- p[athlete_id == "a"]
  expect_equal(pa$p_pos_1, 0.5)
  expect_equal(pa$p_pos_2, 0.25)
  expect_equal(pa$p_pos_3, 0.25)
  # with k = field size, every athlete's row must sum to exactly 1
  sums <- rowSums(as.matrix(p[, -1]))
  expect_equal(unname(sums), rep(1, 3))
})

test_that("p_pos_1 equals p_gold, and the cumulative sum equals p_medal", {
  sim <- fake_sim(rm3())
  mp <- medal_probs(sim, top_n = 3L)
  pp <- position_probs(sim, k = 3L)
  m <- merge(mp[, .(athlete_id, p_gold, p_medal)], pp, by = "athlete_id")
  expect_equal(m$p_pos_1, m$p_gold)
  # p_medal is rank <= 3, which must be the first three position columns summed
  expect_equal(m$p_pos_1 + m$p_pos_2 + m$p_pos_3, m$p_medal)
})

test_that("a field smaller than k still emits k columns, zero-filled", {
  # Two athletes, k = 8: positions 3..8 are impossible and must be 0, not absent
  m <- rbind(c(1L, 2L), c(2L, 1L))
  colnames(m) <- c("a", "b")
  p <- position_probs(fake_sim(m), k = 8L)
  expect_equal(ncol(p), 9L)                       # athlete_id + 8
  expect_true(all(paste0("p_pos_", 1:8) %in% names(p)))
  expect_equal(unname(unlist(p[1, paste0("p_pos_", 3:8), with = FALSE])), rep(0, 6))
})

test_that("the cap loses nothing: the shortfall is p(worse than k)", {
  # Five athletes ranked deterministically; k = 2 keeps only the top two
  m <- matrix(rep(1:5, each = 2), nrow = 2, byrow = TRUE)
  colnames(m) <- letters[1:5]
  p <- position_probs(fake_sim(m), k = 2L)
  e <- p[athlete_id == "e"]
  expect_equal(e$p_pos_1 + e$p_pos_2, 0)          # always finishes 5th
  # so 1 - sum == 1, i.e. certainly worse than 2nd
  expect_equal(1 - (e$p_pos_1 + e$p_pos_2), 1)
})

test_that("k is validated and the object type is enforced", {
  expect_error(position_probs(fake_sim(rm3()), k = 0L), "positive integer")
  expect_error(position_probs(list(rank = rm3())), "citius_sim")
})
