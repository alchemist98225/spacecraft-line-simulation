# spacecraft_expected.R (test-only helper)
# Expected cycle time of one unit with no queueing, built at runtime from the
# params list as an absorbing Markov chain and solved with solve(). Nothing
# here is a worked number: every value comes from the params you pass in.
#
# Transient states, per fail_count k = 0 .. max_fails:
#   S    Subassembly + optional late-parts delay (once, at the start)
#   I_k  Integration + post-Integration QA + Functional test
#   E_k  Environmental test + post-chamber QA   M_k  MRB
#   A    Pre-acceptance QA + Final acceptance   (ship is the absorbing exit)
# Assumes MRB never scraps (the test sets p_scrap to zero).

sc_expected_cycle_h <- function(params) {
  cap <- params$max_fails
  m <- params$mrb
  qa <- params$qa %||% list()
  qa_int <- qa$after_integration$mean_h %||% 0
  qa_ch  <- qa$after_chamber$mean_h %||% 0
  qa_acc <- qa$before_acceptance$mean_h %||% 0
  kit_mean <- if (isTRUE(params$kit$late_parts)) (params$kit$delay_mean_h %||% 0) +
    (params$kit$p_late %||% 0) * (params$kit$late_extra_mean_h %||% 0) else 0
  per_variant <- purrr::pmap_dbl(
    list(params$variants$route, params$variants$func_test, params$variants$env,
         params$variants$subasm %||% replicate(nrow(params$variants), list(mean_h = 0, cv = 0), simplify = FALSE)),
    function(route, ft, en, sa) {
      ks <- 0:cap
      st <- c("S", paste0("I", ks), paste0("E", ks), paste0("M", ks), "A")
      n <- length(st)
      Q <- matrix(0, n, n, dimnames = list(st, st))
      r <- setNames(numeric(n), st)
      go <- function(from, to, p) Q[from, to] <<- Q[from, to] + p
      r["S"] <- (sa$mean_h %||% 0) + kit_mean
      go("S", "I0", 1)
      for (k in ks) {
        I <- paste0("I", k); E <- paste0("E", k); M <- paste0("M", k)
        r[I] <- sum(route$mean_h) + qa_int + ft$mean_h
        nxt <- min(k + 1, cap)
        if (k + 1 >= cap) go(I, paste0("M", nxt), params$p_fail_func) else go(I, paste0("I", nxt), params$p_fail_func)
        go(I, E, 1 - params$p_fail_func)
        r[E] <- en$vibe_mean_h + en$tvac_mean_h + qa_ch
        go(E, paste0("M", nxt), params$p_fail_env)
        go(E, "A", 1 - params$p_fail_env)
        r[M] <- m$mean_h
        p_rework <- if (k >= cap) 0 else m$p_rework
        p <- c(m$p_use_as_is, p_rework) / (m$p_use_as_is + p_rework)
        go(M, "A", p[[1]])
        go(M, I, p[[2]])
      }
      r["A"] <- qa_acc + params$acceptance$mean_h
      solve(diag(n) - Q, r)[["S"]]
    }
  )
  sum(params$variants$mix * per_variant)
}
