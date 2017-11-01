checkSort <- function(sorted) {
  #input: one numeric vector
  #output: TRUE if input is in ascending order, FALSE otherwise
  if (length(sorted) == 1) return(TRUE)  #Base Case (TRUE)
  if (sorted[1] == min(sorted)) return(checkSort(sorted[-1])) #first element sorted, check next
  return(FALSE) #Base Case (False)
}