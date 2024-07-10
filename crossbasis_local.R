crossbasis_local <- function(x, x_to_match, round_by, lag, argvar = list(), arglag = list(), group = NULL,
                             ...) {
  
  # ********
  # x = x_for_basis
  # x_to_match = obs$tmean
  # round_by = 1
  # lag =  maxlag
  # argvar = argvar
  # arglag = arglag
  # group = NULL
  # *********
  
  dlnm:::checkcrossbasis(argvar, arglag, list(...))
  
  lag <- if (missing(lag)) {
    c(0, NCOL(x) - 1)
  } else {
    dlnm:::mklag(lag)
  }
  
  x <- as.matrix(x)
  x_to_match <- as.matrix(x_to_match)
  
  #dim <- dim(x)
  dim <- dim(x_to_match)
  
  if (!dim[2] %in% c(1L, diff(lag) + 1L)) {
    stop(
      "NCOL(x) must be equal to 1 (if x is a time series vector), ",
      "otherwise to the lag period (for x as a matrix of lagged occurrences)"
    )
  }
  ## ------------------
  ##
  x <- round(x, round_by)
  ##
  basisvar_simple <- do.call("onebasis", modifyList(argvar, list(x = as.numeric(x))))
  
  ## ******
  ## HERE, ADD A MATCHING STEP
  ## WHERE YOU MATCH ELEMENTS OF THE BASIS MATRIX TO THE ACTUAL VECTOR
  ## THIS ASSUMES ROUNDING TO 0.1
  rounded_x_to_match <- round(x_to_match, round_by)
  stopifnot(all(rounded_x_to_match %in% x))
  
  basis_df <- data.frame(basisvar_simple)
  basis_df$x <- x
  
  basisvar_df <- data.frame(x = rounded_x_to_match, x_true = x_to_match, id = 1:length(x_to_match))
  basisvar_df <- merge(basisvar_df, basis_df)
  basisvar_df <- dplyr::arrange(basisvar_df, id)
  stopifnot(dim(basisvar_df)[1] == length(rounded_x_to_match))
  stopifnot(dim(basisvar_df)[2] == (ncol(basisvar_simple) + 3))
  # hist(basisvar_df[, 1], xlim = range(x))
  
  basisvar <- as.matrix(basisvar_df[, -c(1, 2, 3)])
  new_attr <- attributes(basisvar_simple)
  new_attr$dim = dim(basisvar)
  attributes(basisvar) = new_attr
  
  basisvar
  
  ### ******
  
  ## ------------------
  if (length(arglag) == 0L || diff(lag) == 0L) {
    arglag <- list(fun = "strata", df = 1, intercept = TRUE)
  }
  if ((is.null(arglag$fun) || "intercept" %in% names(formals(arglag$fun))) &&
      sum(pmatch(names(arglag), "intercept", nomatch = 0)) ==
      0) {
    arglag$intercept <- TRUE
  }
  arglag$cen <- NULL
  ### GET BASIS FOR LAG
  basislag <- do.call("onebasis", modifyList(arglag, list(x = dlnm:::seqlag(lag))))
  dim(basislag)
  ### CHECK GROUP
  if (!is.null(group)) {
    dlnm:::checkgroup(group, x, basisvar, lag)
  }
  ### ASSIGN CROSSBASIS
  crossbasis <- matrix(0, nrow = dim[1], ncol = ncol(basisvar) * ncol(basislag)) 
  dim(crossbasis)
  
  for (v in seq(length = ncol(basisvar))) {
    if (dim[2] == 1L) {
      mat <- as.matrix(tsModel:::Lag(basisvar[, v], dlnm:::seqlag(lag), group = group))
    } else {
      mat <- matrix(basisvar[, v], ncol = diff(lag) + 1)
    }
    for (l in seq(length = ncol(basislag))) {
      crossbasis[, ncol(basislag) * (v - 1) + l] <- mat %*% (basislag[, l])
    }
  }
  ### NAMES AND OUTPUT ATTRIBUTES
  cn <- paste0(
    "v", rep(seq(ncol(basisvar)), each = ncol(basislag)),
    ".l", rep(seq(ncol(basislag)), ncol(basisvar))
  )
  dimnames(crossbasis) <- list(rownames(x), cn)
  ind <- match(names(formals(attributes(basisvar)$fun)), names(attributes(basisvar)),
               nomatch = 0
  )
  argvar <- c(attributes(basisvar)["fun"], attributes(basisvar)[ind])
  ind <- match(names(formals(attributes(basislag)$fun)), names(attributes(basislag)),
               nomatch = 0
  )
  arglag <- c(attributes(basislag)["fun"], attributes(basislag)[ind])
  argvar$cen <- attributes(basisvar)$cen
  attributes(crossbasis) <- c(attributes(crossbasis), list(
    df = c(
      ncol(basisvar),
      ncol(basislag)
    ), range = range(x, na.rm = T), lag = lag,
    argvar = argvar, arglag = arglag
  ))
  if (!is.null(group)) {
    attributes(crossbasis)$group <- length(unique(group))
  }
  class(crossbasis) <- c("crossbasis", "matrix")
  return(crossbasis)
}