#' vandijk is right
#'
#' @param graph The data frame to be fed to
#' @param init_node The initial pointer
#'
#' @return
#' @export
#'
vandijk = function(graph, init_node) {
 if(!is.numeric(init_node) || !(init_node %in% graph[, "v1"])){
    stop()
  }
  valid_names = c("v1", "v2", "w");  unvisited = unique(graph[, "v1"])
  unvisited = sort(unvisited)  ;index = match(init_node, unvisited)
  distance = unvisited ;
  distance[] = Inf
  distance[index] = 0

  while(length(unvisited) > 0){
    idx_min_unvisited = which.min(distance[unvisited])
    current_node = unvisited[idx_min_unvisited]

    unvisited = unvisited[-idx_min_unvisited]

    neighbor_mask = graph[, "v1"] == current_node
    unvisited_mask = graph[neighbor_mask, "v2"] %in% unvisited
    reduced_graph = graph[neighbor_mask, c("v2","w")][unvisited_mask, ]

    d_i = distance[current_node]         # Distance of the current node i.
    d_k = distance[reduced_graph[, "v2"]]   # Current distance of all the unvisited neighbors (k) of i.
    d_ik = reduced_graph[, "w"]            # Distance from the node i to the node k.
    distance[reduced_graph[, "v2"]] = pmin(d_k, d_i + d_ik)
  }
  return(distance)
  }
