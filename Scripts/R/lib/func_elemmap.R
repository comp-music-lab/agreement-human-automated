func_elemmap <- function(noteseq) {
  charset <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
               'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
               'u', 'v', 'w', 'x', 'y', 'z',
               'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
               'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
               'U', 'V', 'W', 'X', 'Y', 'Z',
               '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  
  noteseq_list <- strsplit(noteseq, "(?<=[0-9a-zA-Z]{2})", perl = TRUE)
  
  uniqelem <- unique(unlist(noteseq_list))
  elemmap <- cbind(uniqelem, charset[1:length(uniqelem)])
  
  return(elemmap)
}