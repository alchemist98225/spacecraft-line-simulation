# spacecraft_expected.R (test-only helper)
# Expected cycle time of one unit with no queueing, built at runtime from the
# params list as an absorbing Markov chain and solved with solve(). Nothing
# here is a worked number: every value comes from the params you pass in.
#
# Transient states, per fail_count k = 0 .. max_fails:
#   I_k  Integration + Functional test   E_k  Environmental test   M_k  MRB
#   A    Final acceptance                (ship is the absorbing exit)
# Assumes MRB never scraps (the test sets p_scrap to zero).

sc_expected_cycle_h <- function(params) {
  cap <- params$max_fails
  m <- params$mrb
  per_variant <- purrr::pmap_dbl(
    list(params$variants$route, params$variants$func_test, params$variants$env),
    function(route, ft, en) {
      ks <- 0:cap
      st <- c(paste0("I", ks), paste0("E", ks), paste0("M", ks), "A")
      n <- length(st)
      Q <- matrix(0, n, n, dimnames = list(st, st))
      r <- setNames(numeric(n), st)
      go <- function(from, to, p) Q[from, to] <<- Q[from, to] + p
      for (k in ks) {
        I <- paste0("I", k); E <- paste0("E", k); M <- paste0("M", k)
        r[I] <- sum(route$mean_h) + ft$mean_h
        nxt <- min(k + 1, cap)
        if (k + 1 >= cap) go(I, paste0("M", nxt), params$p_fail_func) else go(I, paste0("I", nxt), params$p_fail_func)
        go(I, E, 1 - params$p_fail_func)
        r[E] <- en$vibe_mean_h + en$tvac_mean_h
        go(E, paste0("M", nxt), params$p_fail_env)
        go(E, "A", 1 - params$p_fail_env)
        r[M] <- m$mean_h
        p_rework <- if (k >= cap) 0 else m$p_rework
        p <- c(m$p_use_as_is, p_rework) / (m$p_use_as_is + p_rework)
        go(M, "A", p[[1]])
        go(M, I, p[[2]])
      }
      r["A"] <- params$acceptance$mean_h
      solve(diag(n) - Q, r)[["I0"]]
    }
  )
  sum(params$variants$mix * per_variant)
}
