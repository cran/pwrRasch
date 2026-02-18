#' Simulate data according to the Rasch model
#' 
#' This function simulates data according to the Rasch model 
#' based on user-specified item and person parameters and an optionally specified test booklet design.
#' 
#' If \code{persons} is an integer value, the corresponding parameter vector 
#' is drawn from N(0, 1.5). If \code{items} is an integer value, the corresponding parameter vector
#' is equally spaced between [-3, 3]. Note that item parameters need to be normalized to sum-0. 
#' This precondition can be overruled using argument \code{sum0 = FALSE}. If \code{!is.null(design)}, 
#' the integer value represents the number of persons in each booklet. Note that unequal number of persons 
#' per booklet is specified in a list corresponding to the number of booklets.
#'   
#' @param persons     Either a vector of specified person parameters, an integer indicating the number of persons, 
#'                    or a list with vectors of specified person parameters or integers corresponding to the
#'                    number of bookelts specified in \code{design}.
#'                    The number of persons for a test booklet design with unequal number of persons per booklet is specified in a list. 
#' @param items       Either a vector of specified item parameters or an integer indicating the number of items. 
#' @param design      Test booklet design matrix with 0 (item not administered) and 1 (item administered) entries.
#' @param sum0        If \code{TRUE}, specified item parameters need to be normalized to sum-0.
#'  
#' @author 
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#' Jan Steinfeld \email{jan.steinfeld@@univie.ac.at}
#'  
#' @seealso 
#' \code{\link{aov.rasch}}, \code{\link{pwr.rasch}}
#'
#' @references
#' Kubinger, K. D., Rasch, D., & Yanagida, T. (2009). On designing data-sampling for Rasch model 
#' calibrating an achievement test. \emph{Psychology Science Quarterly, 51}, 370-384.
#'
#' Kubinger, K. D., Rasch, D., & Yanagida, T. (2011). A new approach for testing the Rasch model.
#' \emph{Educational Research and Evaluation, 17}, 321-333.
#' 
#' @return 
#' Returns a 0-1 matrix according to the Rasch model.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # simulate Rasch model based data
#' # 100 persons, 20 items,
#' # person parameter drawn from a normal distribution: N(0, 1.5)
#' # item parameters equally spaced between [-3, 3]
#' simul.rasch(100, items = 20)
#'
#' # simulate Rasch model based data
#' # 100 persons, 17 items
#' # person parameter drawn from a uniform distribution: U[-4, 4]
#' # item parameters: [-4.0, -3.5, -3.0, ... , 3.0, 3.5, 4.0]
#' simul.rasch(runif(100, -4, 4), items = seq(-4, 4, by = 0.5))
#' 
#' # specify test booklet design with three items
#' design <- matrix(c(1, 1, 0, 1, 0, 1, 0, 1, 1), ncol = 3)
#' # 100 persons in each booklet
#' # person parameter drawn from a normal distribution: N(0, 1.5)
#' # item parameters equally spaced between [-3, 3]
#' simul.rasch(100, items = 3, design = design)
#' 
##' # 100 persons in each booklet
#' # person parameter drawn from a uniform distribution: U[-4, 4]
#' # item parameters equally spaced between [-3, 3]
#' simul.rasch(runif(100, -4, 4), items = 3, design = design)
#' 
#' # 10 persons in booklet 1 / 20 persons in booklet 2 / 30 persons in booklet 3
#' # person parameter drawn from a normal distribution: N(0, 1.5)
#' # item parameters equally spaced between [-3, 3]
#' simul.rasch(list(10, 20, 30), items = 3, design = design)
#' 
#' # 10 persons in booklet 1 / 20 persons in booklet 2 / 30 persons in booklet 3
#' # person parameter drawn from a uniform distribution: U[-4, 4]
#' # item parameters equally spaced between [-4, 4]
#' simul.rasch(list(runif(10, -4, 4), runif(20, -4, 4), runif(30, -4, 4)), 
#'             items = seq(-4, 4, length.out = 3), design = design)
#' }
simul.rasch <- function(persons, items, design = NULL, sum0 = TRUE) {

  #------------------------------------------------------------------------------------------------------#
  # check item parameteres 
  
  # sum of item parameters = 0
  if (sum0 == TRUE & length(items) != 1 & sum(round(items, digits = 3)) != 0) {
      
      stop("Item pararameters are not normalized to sum-0")
      
  }

  #------------------------------------------------------------------------------------------------------#
  # check test design
  
  if (!is.null(design)) {
    
    booklet.n <- nrow(design) 
    
    # entries in the test design consists of 0 and 1 
    if(!all(unlist(design) %in% c(0, 1))) {
      
      stop("Entries in the test design does not consist of 0s and 1s")
      
    }
    
    # number of items specified and number of items in test design
    ifelse(length(items) != 1, items.n <- length(items), items.n <- items)
    
    if(items.n != ncol(design)) {
        
        stop("Number of specified items does not match the test design")
        
    }
      
    #  group of person parameters specified for all booklets
    if (is.list(persons) & length(persons) != booklet.n) {
        
      stop("Specified group of person parameters does not match the test design")
        
    }

    #----------------------------------------
    # equal number of persons in each booklet
    
    if (!is.list(persons) |
        (is.list(persons) & (length(unique(unlist(persons))) == 1)) |
        (is.list(persons) & all(sapply(persons, length) == 1) & length(unique(unlist(persons))) == 1) |
        (is.list(persons) & all(sapply(persons, length) != 1) & length(unique(sapply(persons, length))) == 1)) {
      
      equal <- TRUE 
      
    } else {
      
      equal <- FALSE 
      
    }
    
  } 
    
  #------------------------------------------------------------------------------------------------------#
  
  # item parameters
  if (length(items) == 1) {
    
    ipar <- seq(-3, 3, length.out = items)
    items.n <- items
    
  } else {
    
    ipar <- items
    items.n <- length(items)
    
  }
  
  # person parameters
  if (is.null(design)) {
    
    if (length(persons) == 1) {
      
      ppar <- rnorm(persons, sd = 1.5)
      persons.n <- persons
      
    } else {
      
      ppar <- persons
      persons.n <- length(ppar)
      
    }    
   
  } else {
    
    # balanced 
    if (equal == TRUE) {
      
      if (!is.list(persons)) {
        
        if (length(persons) == 1) {
          
          ppar <- rnorm(persons * booklet.n, sd = 1.5) 
          persons.n <- length(ppar)
          
        } else {
          
          ppar <- rep(persons, times = booklet.n)
          persons.n <- length(ppar)
          
        }
        
      } else {
        
        if (length(unique(unlist(persons))) == 1) {
          
          ppar <- rnorm(persons[[1]] * booklet.n, sd = 1.5) 
          persons.n <- length(ppar)
          
          
        } else {
          
          ppar <- unlist(persons)
          persons.n <- length(ppar)
          
        }
        
      }
      
    # unbalanced  
    } else {
    
      if (all(sapply(persons, length) == 1)) {
        
        ppar <- unlist(lapply(persons, function(x) rnorm(x)))
        persons.n <- length(ppar)
        
      } else {
        
        ppar <- unlist(persons)
        persons.n <- length(ppar)
        
      }  

    }
    
  }  

  #------------------------------------------------------------------------------------------------------#
  
  # no booklet design
  if (is.null(design)) {
    
    fsmat <- outer(ppar, ipar, "-")
    resmat <- (matrix(runif(items.n * persons.n), persons.n, items.n) < exp(fsmat) / (1 + exp(fsmat))) * 1
  
  } else {
  
    # balanced
    if (equal == TRUE) {
      
      fsmat <- outer(ppar, ipar, "-")
      resmat <- (matrix(runif(items.n * persons.n), persons.n, items.n) < exp(fsmat) / (1 + exp(fsmat))) * 1
      
      resmat.split <- split(data.frame(resmat), rep(1:booklet.n, each = persons.n / booklet.n))
      
      for (i in 1:length(resmat.split)) {
        
        resmat.split[[i]][, which(design[i, ] == 0)] <- NA
      
      }  
        
      resmat <- as.matrix(unsplit(resmat.split, rep(1:booklet.n, each = persons.n / booklet.n)))
      dimnames(resmat) <- NULL          
    
    # unbalanced  
    } else {

      fsmat <- outer(ppar, ipar, "-")
      resmat <- (matrix(runif(items.n * persons.n), persons.n, items.n) < exp(fsmat) / (1 + exp(fsmat))) * 1
      
      if (all(sapply(persons, length) == 1)) {
        
        resmat.split <- split(data.frame(resmat), rep(1:booklet.n, times = unlist(persons)))
        
        for (i in 1:length(resmat.split)) {
          
          resmat.split[[i]][, which(design[i, ] == 0)] <- NA
          
        }  
        
        resmat <- as.matrix(unsplit(resmat.split, rep(1:booklet.n, times = unlist(persons))))
        dimnames(resmat) <- NULL         

      } else {

        resmat.split <- split(data.frame(resmat), rep(1:booklet.n, times = unlist(sapply(persons, length))))
        
        for (i in 1:length(resmat.split)) {
          
          resmat.split[[i]][, which(design[i, ] == 0)] <- NA
          
        }  
        
        resmat <- as.matrix(unsplit(resmat.split, rep(1:booklet.n, times = unlist(sapply(persons, length)))))
        dimnames(resmat) <- NULL         
      
      }
       
    }  
    
  }  
  
return(resmat)

}