#' tidy fda pca values
#'
#' Get values from fda objects tidilly
#'
#' @import fda
#' @import purrr
#' @export

pca_fd_df <- function(pca_fd, ngrid = 128,
                      harm = NULL, expand = NULL){

  harm_fd <- pca_fd[[1]]
  basis_fd <- harm_fd$basis
  range_x <- basis_fd$rangeval

  x <- seq(range_x[1], range_x[2], length = ngrid)

  fd_mat <- eval.fd(x, harm_fd)


  mean_mat <- eval.fd(x, pca_fd$meanfd)

  dimfd	 <- dim(fd_mat)
  nharm	 <- dimfd[2]

  if(is.null(harm)){
    get_harm <- 1:nharm
  }else{
    get_harm <- harm
  }

  if(is.null(expand)){
    expand_size <- sqrt(pca_fd$values)
  }else{
    expand_size <- rep(expand, length(pca_fd$values ))
  }

  var_prop <- pca_fd$varprop

  out_df <- map_df(get_harm, ~
                  tibble(harmonic = .x,
                         val = expand_size[.x],
                         var_prop = var_prop[.x],
                         x = x,
                         lower = mean_mat[,1] - (expand_size[.x] * fd_mat[,.x]),
                         mean = mean_mat[,1],
                         upper = mean_mat[,1] + (expand_size[.x] * fd_mat[,.x])))
  return(out_df)


}
