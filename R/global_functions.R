confint.gam <- function(object, parm = NULL, level = 0.95, ...) {
  # a method for extracting confidence intervals and returning a tidy data frame
  
  obj.s <- mgcv::summary.gam(object)
  
  E <- obj.s$p.coeff %>%
    tibble::tibble(Estimate = .,
                   term=names(.)) %>%
    #dplyr::mutate(., term = row.names(.)) %>%
    dplyr::select(., term, Estimate)
  
  
  
  SE <- obj.s$se %>%
    tibble::tibble(se = .,
                   term = names(.)) %>%
    #dplyr::mutate(., term = row.names(.)) %>%
    dplyr::select(., term, se)
  
  if (is.null(parm)){
    parm <- E$term
  }
  
  nu <- obj.s$residual.df
  
  my.tbl <- dplyr::inner_join(E, SE, by = "term") %>%
    dplyr::filter(., term %in% parm) %>%
    dplyr::mutate(.,
                  Statistic = Estimate/se,
                  L = Estimate +
                    se * stats::qt(df = nu,
                                   p = (1 - level) / 2),
                  U = Estimate +
                    se * stats::qt(df = nu,
                                   p = 1 - (1 - level) / 2))
  
  names(my.tbl)[3] <- "Std. Error"
  
  names(my.tbl)[5] <- sprintf("%.1f%%",
                              100*(1-level)/2)
  names(my.tbl)[6] <- sprintf("%.1f%%",
                              100*(1-(1-level)/2))
  
  return(my.tbl)
  
}




buildPoly <- function(xr, yr, slope = 1, intercept = 0, above = TRUE){
  #Assumes ggplot default of expand = c(0.05,0)
  xrTru <- xr + 0.05*diff(xr)*c(-1,1)
  yrTru <- yr + 0.05*diff(yr)*c(-1,1)
  
  #Find where the line crosses the plot edges
  yCross <- (yrTru - intercept) / slope
  xCross <- (slope * xrTru) + intercept
  
  #Build polygon by cases
  if (above & (slope >= 0)){
    rs <- data.frame(x=-Inf,y=Inf)
    if (xCross[1] < yrTru[1]){
      rs <- rbind(rs,c(-Inf,-Inf),c(yCross[1],-Inf))
    }
    else{
      rs <- rbind(rs,c(-Inf,xCross[1]))
    }
    if (xCross[2] < yrTru[2]){
      rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,Inf))
    }
    else{
      rs <- rbind(rs,c(yCross[2],Inf))
    }
  }
  if (!above & (slope >= 0)){
    rs <- data.frame(x= Inf,y= -Inf)
    if (xCross[1] > yrTru[1]){
      rs <- rbind(rs,c(-Inf,-Inf),c(-Inf,xCross[1]))
    }
    else{
      rs <- rbind(rs,c(yCross[1],-Inf))
    }
    if (xCross[2] > yrTru[2]){
      rs <- rbind(rs,c(yCross[2],Inf),c(Inf,Inf))
    }
    else{
      rs <- rbind(rs,c(Inf,xCross[2]))
    }
  }
  if (above & (slope < 0)){
    rs <- data.frame(x=Inf,y=Inf)
    if (xCross[1] < yrTru[2]){
      rs <- rbind(rs,c(-Inf,Inf),c(-Inf,xCross[1]))
    }
    else{
      rs <- rbind(rs,c(yCross[2],Inf))
    }
    if (xCross[2] < yrTru[1]){
      rs <- rbind(rs,c(yCross[1],-Inf),c(Inf,-Inf))
    }
    else{
      rs <- rbind(rs,c(Inf,xCross[2]))
    }
  }
  if (!above & (slope < 0)){
    rs <- data.frame(x= -Inf,y= -Inf)
    if (xCross[1] > yrTru[2]){
      rs <- rbind(rs,c(-Inf,Inf),c(yCross[2],Inf))
    }
    else{
      rs <- rbind(rs,c(-Inf,xCross[1]))
    }
    if (xCross[2] > yrTru[1]){
      rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,-Inf))
    }
    else{
      rs <- rbind(rs,c(yCross[1],-Inf))
    }
  }
  
  return(rs)
}
