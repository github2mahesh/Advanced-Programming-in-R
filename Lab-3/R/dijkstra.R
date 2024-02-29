#' Dijkstra Algorithm 
#' 
#' @param graph dataframe
#' @param init_node number for starting node
#' @description calculates the shortest path between every nodes in a graph
#' @return shortest path between the selected node and all other node
#' @references
#'(https://en.wikipedia.org/wiki/Dijkstra%27s algorithm)
#' @export
#' @examples 
#' wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)


dijkstra <-
function(graph,init_node)
{
  non_used_edge<-unique(graph$v1) # 6 values for the given ex
  path<-vector(length=length(non_used_edge)) # initialising path with length of v1
  stopifnot(init_node<length(non_used_edge)) #check if length exceeds
  path[init_node]<-0 
  path[-init_node]<-999
  
  while (length(non_used_edge)!=0)
  {
    current<-non_used_edge[1] #taking first vertices value
    x<-graph[graph$v1==current,] # taking all rows from the wiki_graph data for the first edge value
    
    i<-1
    while (i <= nrow(x))
    {
      if((path[current]+x$w[i]) < path[x$v2[i]]) #check if the edge has shortest path
      {
        path[x$v2[i]]<-(path[current]+x$w[i]) #if shortest, save the current path
      }
      i<-i+1
    }
    
    non_used_edge<-non_used_edge[-1] #loop for next edge
  }
  return(path)
}
