library(parallel)

tr = function(X) {
  if (!is.matrix(X)) {
    stop("tr: X should be a matrix.")
  }
  if (dim(X)[1] != dim(X)[2]) {
    stop("tr: X should be a square matrix.")
  }
  return(sum(diag(X)))
}

matlist_prod = function(indices, mlst1, mlst2, dims) {
  mat_prod = function(idx, mlst1, mlst2, dims) {
    if (dims[idx] == 1) {
      return(mlst1[[idx]] * mlst2[[idx]])
    } else {
      return(mlst1[[idx]] %*% mlst2[[idx]])
    }
  }
  
  res = lapply(indices, mat_prod, mlst1, mlst2, dims)
  
  return(Reduce('+', res))
}

comp_ip = function(u, X, u_dim, x_dim, L) {
  if (u_dim == 1) {
    u = as.vector(u)
  }
  if (x_dim == 1) {
    X = as.vector(X)
  }
  u_x = t(u) %*% X
  
  if (u_dim == 1 && x_dim == 1) {
    if (length(u_x) > 1) {
      stop(paste0("comp_ip: u_x should be scalar; length(u_x) = ", length(u_x)))
    }
  } else if (u_dim == 1 && x_dim > 1) {
    if (length(u_x) != x_dim) {
      stop(paste0("comp_ip: u_x should be of length ", x_dim, "; length(u_x) = ", 
                  length(u_x)))
    }
  } else if (u_dim > 1 && x_dim == 1) {
    if (length(u_x) != u_dim) {
      stop(paste0("comp_ip: u_x should be of length ", u_dim, "; length(u_x) = ",
                  length(u_x)))
    }
  } else {
    if (!is.matrix(u_x)) {
      stop("comp_ip: u_x shuould be a matrix.")
    }
    if (dim(u_x)[1] != u_dim || dim(u_x)[2] != x_dim) {
      stop(paste0("comp_ip: u_x incorrect dimensions: ", dim(u_x)[1], ", ", 
                  dim(u_x)[2]))
    }
  }
  
  if (u_dim == 1 && x_dim == 1) {
    return(list("IP" = L * abs(u_x), "B" = L * sign(u_x)))
  } else if (u_dim == 1 && x_dim > 1) {
    u_x_norm = sqrt(sum(u_x^2))
    return(list("IP" = L * u_x_norm, "B" = L * t(u_x) / u_x_norm))
  } else if (u_dim > 1 && x_dim == 1) {
    u_x_norm = sqrt(sum(u_x^2))
    return(list("IP" = L * u_x_norm, "B" = L * t(u_x) / u_x_norm))
  } else {
    dc = svd(u_x, nu = 1, nv = 1)
    return(list("IP" = L * dc$d[1], "B" = L * outer(as.vector(dc$v), 
                                                    as.vector(dc$u))))
  }
}

comp_lambda = function(u, X, B, G) {
  XB = X %*% B
  C = XB - G
  
  num = tr(t(u) %*% C)
  denum = sum(C^2)
  lambda_uc = num / denum

  return(max(min(lambda_uc, 1), 0))
}

data_mean = function(dim, data) {
  if (dim == 1) {
    return(mean(data))
  } else {
    return(colMeans(data))
  }
}

rga_core = function(y, X, dims, L, Kn, B_init = NULL, mc_cores = 1, 
                    parallel = FALSE, verbose = FALSE) {
  ###
  # rga_core: relaxed greedy algorithm core function
  # inputs:
  # y: an n by M matrix of the response variables, where M = number of tasks.
  # X: a list of covariate observation matrices. X should have p elements, each 
  #    of which is a matrix representing the measurements.
  # dims: a vector of integers indicating the number of tasks and the dimensions
  #       of each of the predictors
  # L: user-prescribed parameter
  # Kn: number of desired iterations.
  # w_init: initial values for the coefficient matrices; default is zero.
  # mc_cores: number of cores available for parallel computing
  # parallel: whether to perform parallel computing for the p variables.
  #
  # output:
  # B: a list of coefficient matrices. Zero matrix is represented by NULL.
  # J_hat: a vector of indices corresponding to selected variables.
  # path: a vector of indices representing the selected variables in each step.
  # G: fitted values matrix.
  # loss: a sequence of training losses.
  # lambda_seq: lambda's computed along the algorithm's path.
  ###
  
  p = length(dims) - 1
  if (dims[1] == 1) {
    n = length(y)
  } else {
    n = dim(y)[1]
  }
  
  # Initialization
  if (is.null(B_init)) {
    B = vector(mode = "list", length = p)
    G = 0
  } else {
    B = B_init
    G = 0
    for (j in 1:p) {
      if (!is.null(B[[j]])) {
        G = G + X[[j]] %*% B[[j]]
      }
    }
  }

  u = y - G
  loss = rep(NA, Kn)
  lambda_seq = rep(NA, Kn)
  J_hat = NULL
  
  rga_search = function(j) {
    return(comp_ip(u, X[[j]], u_dim = dims[1],
                   x_dim = dims[j + 1], L = L)$IP)
  }
  
  for (i in 1:Kn) {
    if (parallel) {
      ips = unlist(mclapply(1:p, rga_search, mc.cores = mc_cores))
    } else {
      ips = unlist(lapply(1:p, rga_search))
    }
    j_star = which.max(ips)
    J_hat = c(J_hat, j_star)
    B_tilde = comp_ip(u, X[[j_star]], u_dim = dims[1], x_dim = dims[j_star + 1],
                      L = L)$B
    lambda = comp_lambda(u, X[[j_star]], B_tilde, G)
    lambda_seq[i] = lambda

    for (j in 1:p) {
      if (j == j_star) {
        if (is.null(B[[j]])) {
          B[[j]] = lambda * B_tilde
        } else {
          B[[j]] = (1 - lambda) * B[[j]] + lambda * B_tilde
        }
      } else {
        if (!is.null(B[[j]])) {
          B[[j]] = (1 - lambda) * B[[j]]
        }
      }
    }
    
    G = (1 - lambda) * G + lambda * (X[[j_star]] %*% B_tilde)
    u = y - G
    loss[i] = sum(u^2) / n
    
    if (verbose) {
      cat("iteration", i, "\n")
    }
  }
  
  return(list("B" = B, "J_hat" = unique(J_hat), "path" = J_hat,
              "G" = G, "loss" = loss, "lambda" = lambda_seq))
}
  
rga = function(y, X, dims, L, Kn, B_init = NULL, mc_cores = 1, 
               parallel = FALSE, scale = TRUE, verbose = FALSE) {
  ###
  # rga: wrapper to implement RGA
  ###
  
  # Pre-processing
  p = length(dims) - 1
  y_means = data_mean(dim = dims[1], data = y)
  x_means = vector(mode = "list", length = p)
  
  if (dims[1] == 1) {
    y = y - y_means
  } else {
    y = t(t(y) - y_means)
  }

  for (i in 1:p) {
    x_means[[i]] = data_mean(dim = dims[i + 1], data = X[[i]])
    if (dims[i + 1] > 1) {
      if (scale) {
        X[[i]] = t((t(X[[i]]) - x_means[[i]])) # / sqrt(dims[i + 1])
      } else {
        X[[i]] = t(t(X[[i]]) - x_means[[i]])
      }
    } else {
      X[[i]] = X[[i]] - x_means[[i]]
    }
  }
  
  # Run RGA using core function
  model = rga_core(y = y, X = X, dims = dims, L = L, Kn = Kn, B_init = B_init,
                   mc_cores = mc_cores, parallel = parallel, verbose = verbose)
  
  # Calculate intercept term
  fitted_val = t(t(model$G) + y_means)

  return(list("B" = model$B, "J_hat" = model$J_hat, 
              "path" = model$path, "fitted_values" = fitted_val, 
              "loss" = model$loss, "y_means" = y_means, "x_means" = x_means))
}

tsrga = function(y, X, dims, L, Kn1, Kn2, mc_cores = 1, 
                 parallel = FALSE, verbose = FALSE) {
  ###
  # tsrga: wrapper for two-stage relaxed greedy algorithm
  ###
  
  res1 = rga(y, X, dims, L, Kn1, mc_cores = mc_cores, parallel = parallel)
  selected = sort(res1$J_hat)
  selected_X = X[selected]
  B_init = res1$B[!sapply(res1$B, is.null)]
  new_dims = c(dims[1], dims[1 + selected])
  res2 = rga(y, selected_X, new_dims, L, Kn2, B_init = B_init,
             mc_cores, parallel)
  
  p = length(dims) - 1
  B_hat = vector(mode = "list", length = p)
  for (j in 1:p) {
    if (j %in% selected) {
      B_hat[j] = res2$B[which(selected == j)]
    }
  }
  
  return(list("B" = B_hat, "J_hat" = selected, 
              "path" = c(res1$path, selected[res2$path]), 
              "fitted_values" = res2$fitted_values,
              "y_means" = res2$y_means,
              "x_means" = res1$x_means))
}

rga_eval = function(rga_model, new_X, new_Y = NULL, dims) {
  
  p = length(new_X)
  B0 = rga_model$y_means - matlist_prod(1:p, rga_model$x_means, rga_model$B, dims)
  pred = matlist_prod(1:p, new_X, rga_model$B, dims)
  pred = t(t(pred) + c(B0))
    
  if (is.null(new_Y)) {
    return(list("pred" = pred))
  } else {
    n = ifelse(dims[1] == 1, length(new_Y), nrow(new_Y))
    return(list("pred" = pred, "mse" = sum((new_Y - pred)^2)/ n))
  }
}
