# =======================================================================
# Group Name: 
# Students: Imanol Santiago & Ignacio Gonzalez
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(filename) {
  data <- readLines(con <- file(filename, "r"))
  
  num_clientes <- as.integer(data[1])
  vector_gustos <- c()
  gustos_list <- list()
  
  for (i in 1:num_clientes) {
    gustos <- strsplit(data[2*i], " ")[[1]][-1]
    gustos <- as.character(gustos)
    vector_gustos <- c(vector_gustos, gustos)
    
    disgustos <- strsplit(data[2*i+1], " ")[[1]][-1]
    disgustos <- as.character(disgustos)
  }
  vector_gustos <- unique(vector_gustos)
  for(gusto in vector_gustos){
    gustos_list[[length(gustos_list) + 1]] <- gusto
  }
  print(gustos_list)
  problem <- list()
  problem$name <- "One Pizza Problem"
  problem$state_initial <- vector()
  problem$state_final <- NULL
  problem$gustos_list <- gustos_list
  problem$actions_possible <- data.frame(action = gustos_list, stringsAsFactors = FALSE)
  problem$lista_ingredientes <- unique(gustos_list)
  problem$num_clientes <- num_clientes
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE
  result <- !(action %in% state)
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- c(state,action) # añadir ingrediente al estado actual.
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  clientes_satisfechos <- 0
  
  for (i in seq_along(problem$gustos_list)) {
    if (all(problem$gustos_list[i] %in% state)) {
      clientes_satisfechos <- clientes_satisfechos + 1
    }
  }
  ratio_satisfechos <- clientes_satisfechos / problem$num_clientes
  return(ratio_satisfechos >= 0.3)
}

# Transforms a state into a string
to.string = function (state, problem) {
  print(state)
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  clientes_satisfechos <- 0
  
  for (cliente in problem$mi_lista) {
    if (all(cliente$gustos %in% state)) {
      clientes_satisfechos <- clientes_satisfechos + 1
    }
  }
  ratio_satisfechos <- clientes_satisfechos / problem$num_clientes
  return(ratio_satisfechos)
}