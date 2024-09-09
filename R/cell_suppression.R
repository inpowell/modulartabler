#' Determine cells to suppress in a contingency table
#'
#' These functions implement primary and secondary suppression in a contingency
#' table according to the algorithm from Fischetti and Salazar (2001). Given a
#' table in long format and the margins the table is summed over, this function
#' gives a logical vector of the cells that need to be suppressed to prevent
#' back-calculation.
#'
#' `determine_cell_suppression` performs primary suppression of cells between
#' the `small_min` and `small_max` thresholds (inclusive), and then performs
#' secondary suppression to prevent back-calculation. `suppress_secondary` takes
#' a logical vector that represents a primary suppression, and only performs
#' secondary suppression on this. This allows more flexibility in the
#' suppression requirements (e.g. by allowing small cells to be visible if they
#' belong to "Unknown" values).
#'
#' @param N The variable containing counts.
#' @param suppress A logical vector of the same size as `N` that is `TRUE` for
#'   cells that should be primary suppressed, and `FALSE` otherwise.
#' @param nullspace A matrix representation of the table total and subtotal
#'   relationships. For any vector N that has valid totals and subtotals,
#'   `nullspace %*% N` should be a zero vector.
#' @param ... For `suppress_secondary`, passed to [ROI::ROI_solve]. For
#'   `determine_cell_suppression`: Passed to `suppress_secondary`.
#' @param small_min,small_max The bounds for small cells which must be
#'   suppressed. Cells will be suppressed if they lie between these two bounds,
#'   inclusive.
#' @param LPL,UPL,SPL Lower, upper and sliding protection limits, respectively,
#'   which determine back-calculation sensitivity. See references for more
#'   details. The default prevents exact recalculation.
#' @param LB,UB The relative lower and upper bounds of each cell assumed known
#'   to the attacker. A vector of the same size as `N`. By default, the attacker
#'   knows that any cell is between 0 and double its true value.
#' @param w The "weight" we assign to each cell to prevent suppression. A vector
#'   of the same size as `N`. By default, we minimise total count suppressed.
#'   Other options include `log(N)`, which prioritises smaller cells.
#' @param solver The solver used for [ROI::ROI_solve].
#' @param max_iter The maximum number of times to attack each cell requiring
#'   suppression.
#'
#' @return Both these functions return a logical vector of cells that need to be
#'   suppressed
#'
#' @references Fischetti M, Salazar JJ. Solving the cell suppression problem on
#'   tabular data with linear constraints. Management Science.
#'   2001;47(7):1008-1027.
#'   <https://www.proquest.com/scholarly-journals/solving-cell-suppression-problem-on-tabular-data/docview/213175964/se-2?accountid=147118>
#'
#' @export
determine_cell_suppression <- function(
    N,
    nullspace,
    small_min = 1L, small_max = 4L,
    ...) {
  suppress <- small_min <= N & N <= small_max
  suppress_secondary(N, suppress, nullspace, ...)
}

#' @rdname determine_cell_suppression
#' @importFrom ROI OP constraints constraints<- L_objective L_constraint V_bound
#'   ROI_solve
#' @export
suppress_secondary <- function(
    N,
    suppress,
    nullspace,
    LPL = 0L, UPL = 0L, SPL = 1L,
    w = N,
    LB = N, UB = N,
    ...,
    solver = 'highs',
    max_iter = 100L) {
  if (nrow(nullspace) == 0) return(suppress)

  if (!ROI::ROI_require_solver(solver, warn = -1L)) {
    cli::cli_abort(c(
      "Requested {.pkg ROI} solver {.val {solver}} but it is not available.",
      'i' = "Is the {.pkg ROI.plugin.{solver}} package available and installed?"
    ))
  }

  n <- length(N)

  if (length(LPL) != n) {
    LPL <- rep_len(LPL, n)
  }
  if (length(UPL) != n) {
    UPL <- rep_len(UPL, n)
  }
  if (length(SPL) != n) {
    SPL <- rep_len(SPL, n)
  }

  # Check if protection required is stronger than what attacker knows
  if (any(LPstrong <- LB < LPL & suppress)) {
    idx <- which(LPstrong)
    egidx <- idx[[1]]
    egval <- N[egidx]
    eglpl <- LPL[egidx]
    eglb <- N[egidx] - LB[egidx]
    egub <- N[egidx] + UB[egidx]
    info <- c(
      "Requested lower protection limit {.var LPL} is too strong for given attacker lower bound {.var LB}",
      'v' = 'Cells indexed {.val {idx}} must have {.var LPL} decreased or {.var LB} increased.',
      'i' = paste(
        'E.g. suppressed cell indexed {.val {egidx}} has true value',
        '{.val {egval}}, and an attacker knows it is between {.val {eglb}} and',
        '{.val {egub}}. This is incompatible with {.var LPL} = {.val {eglpl}}.'
      )
    )
    cli::cli_abort(info, class = c('LPL-error', 'bounds-error'))
  }

  if (any(UPstrong <- UB < UPL & suppress)) {
    idx <- which(UPstrong)
    egidx <- idx[[1]]
    egval <- N[egidx]
    egupl <- UPL[egidx]
    eglb <- N[egidx] - LB[egidx]
    egub <- N[egidx] + UB[egidx]
    info <- c(
      "Requested upper protection limit {.var UPL} is too strong for given attacker upper bound {.var UB}",
      'v' = 'Cells indexed {.val {idx}} must have {.var UPL} decreased or {.var UB} increased.',
      'i' = paste(
        'E.g. suppressed cell indexed {.val {egidx}} has true value',
        '{.val {egval}}, and an attacker knows it is between {.val {eglb}} and',
        '{.val {egub}}. This is incompatible with {.var UPL} = {.val {egupl}}.'
      )
    )
    cli::cli_abort(info, class = c('UPL-error', 'bounds-error'))
  }

  if (any(SPstrong <- UB + LB < SPL & suppress)) {
    idx <- which(SPstrong)
    egidx <- idx[[1]]
    egval <- N[egidx]
    egspl <- SPL[egidx]
    eglb <- N[egidx] - LB[egidx]
    egub <- N[egidx] + UB[egidx]
    info <- c(
      "Requested sliding protection limit {.var SPL} is too strong for given attacker bounds {.var LB} and {.var UB}",
      'v' = 'Cells indexed {.val {idx}} must have {.var SPL} decreased, or {.var LB} or {.var UB} increased.',
      'i' = paste(
        'E.g. suppressed cell indexed {.val {egidx}} has true value',
        '{.val {egval}}, and an attacker knows it is between {.val {eglb}} and',
        '{.val {egub}}. This is incompatible with {.var SPL} = {.val {egspl}}.'
      )
    )
    cli::cli_abort(info, class = c('SPL-error', 'bounds-error'))
  }

    # Determine cells requiring primary suppression
  ik <- which(suppress)
  p <- length(ik)

  if (identical(p, 0L))
    return(rep(FALSE, n))

  if (is.null(nullspace))
    return(suppress)

  M <- nullspace
  m <- nrow(nullspace)
  z <- rep(0, m)
  stopifnot(identical(M %*% N, matrix(z, ncol = 1)))

  ### Start algorithm --------------------------------------

  ### Attacker sub-problem.

  # Let:
  #  - N be a vector of size n that contains the vector of cell counts
  #  - w be the "importance" weight assigned to suppressing each cell
  #  - M be the table nullspace, M %*% y == z for any and all vectors y that
  #    satisfy the table relationships, where z is the zero vector.
  #  - k be the index of the cell we want to suppress (between 1 and n)
  #  - e_k be the n-dimensional vector with 1 in the k^th position and 0
  #    everywhere else.

  # Suppose:
  #  - a, b and g are unknown real vectors of size n
  #  - x is our candidate suppression pattern: x = 1 if suppressed, x = 0 if not
  #   suppressed

  # Then find (ref, eqn 18-19)
  #     y^UPL_k = min{ z'g + \sum_{i=1}^n a_i (N_i + UB_i x_i) - b_i (N_i - LB_i x_i) }
  # subject to
  #     a - b + M * g = e_k
  #     a >= 0
  #     b >= 0

  UPL_problem <- function(x, k) OP(
    objective = L_objective(
      L = c(
        alpha = N + UB * x,
        beta = -(N - LB * x),
        gamma = z
      )
    ),
    constraints = L_constraint(
      L = cbind(alpha = diag(1, nrow = n), beta = diag(-1, nrow = n), gamma = t(M)),
      dir = rep('==', n),
      rhs = diag(1, nrow = n)[, k],
      names = c(paste0('alpha', 1:n), paste0('beta', 1:n), paste0('gamma', 1:m))
    ),
    bounds = V_bound(
      # g lower bounds are -Inf
      li = n + n + 1:m,
      lb = rep(-Inf, m)
    )
  )

  # The LPL problem is similar, but we just switch the sign of the RHS
  # constraint (ref, eqn 22-23)
  LPL_problem <- function(x, k) OP(
    objective = L_objective(
      L = c(
        alpha = N + UB * x,
        beta = -(N - LB * x),
        gamma = z
      )
    ),
    constraints = L_constraint(
      L = cbind(alpha = diag(1, nrow = n), beta = diag(-1, nrow = n), gamma = t(M)),
      dir = rep('==', n),
      rhs = -diag(1, nrow = n)[, k],
      names = c(paste0('alpha', 1:n), paste0('beta', 1:n), paste0('gamma', 1:m))
    ),
    bounds = V_bound(
      # g lower bounds are -Inf
      li = n + n + 1:m,
      lb = rep(-Inf, m)
    )
  )

  ### Master LP

  # The master problem starts simply, trying to minimise the weight w of
  # suppressed cells. Then, we iteratively add constraints to this problem as we
  # find them from the attacker subproblems.
  #
  # This setup is from ref, eqn (24)
  master_lp <- OP(
    objective = L_objective(w),
    constraints = L_constraint(
      diag(nrow = n)[suppress, , drop = FALSE],
      dir = rep('==', sum(suppress)),
      rhs = rep(1, sum(suppress))
    ),
    types = rep('B', n)
  )

  # Initial suppression requirement
  candidate_suppression <- suppress
  i <- 1L

  repeat {

    attacker_success <- FALSE

    for (attack.ik in ik) { # Attack all cells to be suppressed

      # Calculate known bounds
        if (SPL[attack.ik] > 0L || UPL[attack.ik] > 0L) {
          attacker.max <- ROI_solve(UPL_problem(candidate_suppression, attack.ik), solver = solver, ...)
        }

        if (SPL[attack.ik] > 0L || LPL[attack.ik] > 0L) {
          attacker.min <- ROI_solve(LPL_problem(candidate_suppression, attack.ik), solver = solver, ...)
        }

        # Evaluate UPL subproblem
        if (UPL[attack.ik] > 0L &&
            identical(attacker.max$status$code, 0L) &&
            attacker.max$objval < N[attack.ik] + UPL[attack.ik]) {
          # Successful solution
          alpha <- attacker.max$solution[seq_len(n)]
          beta <- attacker.max$solution[n + seq_len(n)]

          constraints(master_lp) <- c(
            constraints(master_lp),
            # ref, eqn (25)
            L_constraint(
              pmin(alpha * UB + beta * LB, UPL[attack.ik]), # Sec 4.2.1
              dir = '>=',
              rhs = UPL[attack.ik]
            )
          )

          attacker_success <- TRUE
        }

        # Evaluate LPL subproblem
        if (LPL[attack.ik] > 0L &&
            identical(attacker.min$status$code, 0L) &&
            -attacker.min$objval > N[attack.ik] - LPL[attack.ik]) {
          # Successful solution
          alpha <- attacker.min$solution[seq_len(n)]
          beta <- attacker.min$solution[n + seq_len(n)]

          constraints(master_lp) <- c(
            constraints(master_lp),
            # ref, eqn (26)
            L_constraint(
              pmin(alpha * UB + beta * LB, LPL[attack.ik]), # Sec 4.2.1
              dir = '>=',
              rhs = LPL[attack.ik]
            )
          )

          attacker_success <- TRUE
        }

        # Evaluate SPL subproblem
        if (SPL[attack.ik] > 0L &&
            identical(attacker.max$status$code, 0L) &&
            identical(attacker.min$status$code, 0L) &&
            # Note negation in eqn (23)
            attacker.max$objval + attacker.min$objval < SPL[attack.ik]) {

          alpha <- attacker.min$solution[seq_len(n)] +
            attacker.max$solution[seq_len(n)]

          beta <- attacker.min$solution[n + seq_len(n)] +
            attacker.max$solution[n + seq_len(n)]

          constraints(master_lp) <- c(
            constraints(master_lp),
            # ref, eqn (27)
            L_constraint(
              pmin(alpha * UB + beta * LB, SPL[attack.ik]), # Sec 4.2.1
              dir = '>=',
              rhs = SPL[attack.ik]
            )
          )

          attacker_success <- TRUE

        }

        if (!attacker_success) break

    }

    # If there is no successful avenue of attack, break out
    if (!attacker_success) break

    # Exit with error if iteration limit exceeded
    if (identical(i, max_iter) && attacker_success)
      stop("Maximum attacker iterations reached. Consider increasing the max_iter parameter")

    # Re-solve master LP with new constraints to feed next cycle
    i <- i + 1L
    master_soln <- ROI_solve(master_lp, ...)
    candidate_suppression <- as.logical(master_soln$solution)

  }

  # Any primary suppressed cell should be suppressed in output
  if (!all(candidate_suppression[suppress]))
    stop("Optimal solution resulted in primary suppression failures")

  return(candidate_suppression)

}

#' Convenience function to convert matrix to dense representation
#'
#' This function is to facilitate work with `lpSolve::lp`, particularly with the
#' `dense.const` argument.
#'
#' @param M matrix to convert to dense form
#' @param zapsmall Whether to zap small values to zero; see `?base::zapsmall`
#' @param digits Number of digits to zap to zero
#'
#' @return `to_dense` returns a matrix with three columns. The first column is
#'   the row number, the second is the column number, and the third is the
#'   value.
#' @noRd
to_dense <- function(M, zapsmall = TRUE,
                     digits = ceiling(-log10(.Machine$double.eps) / 2)) {
  if (zapsmall)
    M <- base::zapsmall(M, digits = digits)

  nonzeros <- M != 0

  cbind(
    rows = row(M)[nonzeros],
    cols = col(M)[nonzeros],
    vals = M[nonzeros]
  )
}
