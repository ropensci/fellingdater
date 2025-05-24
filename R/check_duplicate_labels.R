check_duplicate_labels <- function(x) {
     if (sum(duplicated(x)) >= 1 | any(is.na(x))) {
          which_labs <- which(duplicated(x))
          which_labs <- unique(x[which_labs])
          no_label <- sum(is.na(x))
          
          warning("\n", paste(which_labs, " is not a unique label\n"))
          if (no_label > 0) {
               warning("\n", paste(no_label, " series have no label\n"))
          }
          stop("make sure all series have a unique id")
     }
}