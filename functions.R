GGMncv_search <- function(x,
                          n,
                          penalty = "atan",
                          ic = "bic",
                          n_lambda = 50,
                          n_gamma = 50,
                          gamma = NULL,
                          select_gamma = TRUE,
                          select_lambda = TRUE,
                          initial = "sicm",
                          method = "pearson",
                          progress = TRUE,
                          store = TRUE,
                          ...) {
  
  
  
  p <- ncol(x)
  
  if(penalty == "atan" | penalty == "selo"){
    
    gammas <- seq(0.01, 0.2, length.out = n_gamma)
    
  } else if(penalty == "scad"){
    
    gammas <- seq(2.001, 6, length.out = n_gamma)
    
  } else if(penalty == "mcp"){
    
    gammas <- seq(1.001, 5, length.out = n_gamma)
    
  } else if(penalty == "lasso") {
    
  } else {
    stop("penalty not currently supported")
  }
  
  
  ls <- list()
  
  
  if(penalty == "lasso"){
    
    fit <- GGMncv::GGMncv(x = x,
                          n = n,
                          penalty = penalty,
                          
                          ic = ic,
                          n_lambda = n_lambda,
                          select = TRUE,
                          store = TRUE, 
                          progress = FALSE)
    
  } else {
    
    
    if(isTRUE(select_gamma) & isTRUE(select_lambda)){
      
      for(i in 1:n_gamma){
        
        fit <- GGMncv::GGMncv(x = x,
                              n = n,
                              penalty = penalty,
                              ic = ic,
                              n_lambda = n_lambda,
                              select = TRUE,
                              gamma = gammas[i], 
                              store = TRUE, 
                              progress = FALSE)
        
        res <-  data.frame(l = fit$lambda, 
                           g = gammas[i], 
                           sapply(fit$fitted_models, "[[", "ic"))
        ls[[i]] <- res
        
      }
      
      results <- do.call(rbind.data.frame, ls)
      results <- results[which.min(results [,3]),]
      selected_gamma <- results[,2]
      selected_lambda <- results[,1]
      
      fit <- GGMncv::GGMncv(cor(Y), n = n, 
                            lambda =  selected_lambda, 
                            gamma = selected_gamma, 
                            penalty = penalty)
      
      fit$selected_gamma <- selected_gamma
      fit$selected_lambda <- selected_lambda
      
      
    } else if (isTRUE(select_gamma) & isFALSE(select_lambda)) {
      
      for(i in 1:n_gamma){
        
        fit <- GGMncv::GGMncv(x = x,
                              n = n,
                              penalty = penalty,
                              ic = ic,
                              gamma = gammas[i],
                              lambda = sqrt(log(p) / n ),
                              select = TRUE,
                              store = TRUE, 
                              progress = FALSE)
        
        res <-  data.frame(l = fit$lambda, 
                           g = gammas[i], 
                           sapply(fit$fitted_models, "[[", "ic"))
        ls[[i]] <- res
        
      }
      
      
      results <- do.call(rbind.data.frame, ls)
      results <- results[which.min(results [,3]),]
      selected_gamma <- results[,2]
      selected_lambda <- results[,1]
      
      fit <- GGMncv::GGMncv(cor(Y), n = n, 
                            lambda =  selected_lambda, 
                            gamma = selected_gamma, 
                            penalty = penalty)
      
      fit$selected_gamma <- selected_gamma
      fit$selected_lambda <- selected_lambda
      
    } else if (isFALSE(select_gamma) & isTRUE(select_lambda)) { 
      
      fit <- GGMncv::GGMncv(x = x,
                            n = n,
                            penalty = penalty,
                            ic = ic,
                            gamma = gamma,
                            select = TRUE,
                            store = TRUE, 
                            progress = FALSE)
      
      fit$selected_lambda <- fit$fitted_models[[which.min( sapply(fit$fitted_models, "[[", "ic"))]]$lambda
    } else {
      
      stop("gamma can not be fixed. use instead the function GGMncv.")
    }
    
    
    
    returned_object <- fit
    returned_object
  }
}
