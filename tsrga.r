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
      stop(paste0("u_x should be scalar; length(u_x) = ", length(u_x)))
    }
  } else if (u_dim == 1 && x_dim > 1) {
    if (length(u_x) != x_dim) {
      stop(paste0("u_x should be of length ", x_dim, "; length(u_x) = ", 
                  length(u_x)))
    }
  } else if (u_dim > 1 && x_dim == 1) {
    if (length(u_x) != u_dim) {
      stop(paste0("u_x should be of length ", u_dim, "; length(u_x) = ",
                  length(u_x)))
    }
  } else {
    if (!is.matrix(u_x)) {
      stop("u_x shuould be a matrix.")
    }
    if (dim(u_x)[1] != u_dim || dim(u_x)[2] != x_dim) {
      stop(paste0("u_x incorrect dimensions: ", dim(u_x)[1], ", ", dim(u_x)[2]))
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

rga = function(y, X, dims, L, Kn, B_init = NULL,
               mc_cores = 1, parallel = FALSE) {
  ###
  # rga: relaxed greedy algorithm
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
  ###
  
  p = length(dims) - 1
  if (dims[1] == 1) {
    n = length(y)
  } else {
    n = dim(y)[1]
  }
  
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
  }
  
  return(list("B" = B, "J_hat" = unique(J_hat), "path" = J_hat, "G" = G,
              "loss" = loss, "lambda_seq" = lambda_seq))
}

tsrga = function(y, X, dims, L, Kn1, Kn2, 
                 mc_cores = 1, parallel = FALSE) {
  ###
  # tsrga: two-stage relaxed greedy algorithm
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
              "path" = c(res1$path, selected[res2$path]), "G" = res2$G))
}

