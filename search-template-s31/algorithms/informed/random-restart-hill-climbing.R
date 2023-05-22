random.restart.hill.climbing <- function(file, restarts, max_iterations = 50, count_print = 10, trace = FALSE) {
  name_method <- paste0("Random Restart Hill Climbing Search")
  start_time <- Sys.time()
  best_state <- NULL
  best_report <- NULL
    
  # Realiza la búsqueda de escalada de colina con reinicio aleatorio
  for (i in 1:restarts) {
    problem <- initialize.problem(file)
    result <- hill.climbing.search(problem, max_iterations, count_print, trace)
      
    # Actualiza el mejor estado y el mejor informe si se encuentra un estado mejor
    if (is.null(best_state) || result$state_final$evaluation < best_state$evaluation) {
      best_state <- result$state_final
      best_report <- result$report
    }
  }
    
  result <- list()
  result$state_final <- best_state
  result$report <- best_report
  
  end_time <- Sys.time()
  runtime <- end_time - start_time
  
  result$name <- name_method
  result$runtime <- runtime
  return(result)
}
