#' Calculation of Standardized Z Scores From X Values
#' 
#' Convert numeric values of x from a non-standard normal distribution to z scores
#' from the standard normal distribution.
#' @param x A vector of 1 or more numeric values of x.
#' @param mu The value of the population mean for the x distribution 
#' (0 = default value).
#' @param sigma The value of the population standard deviation for the x 
#' distribution (1 = default value).
#' @return A printed output of one z score for each input value of x.
#' @note Use of mu = 0 and sigma = 1 (the default values) implies that the x 
#' distribution is equivalent to the standard normal distribution.  Thus,
#' the 'z' value returned under default conditions will be the input x value.
#' @examples 
#' z1 <- x2z(x = 3)
#' 
#' z2 <- x2z(x = 3, mu = 2.5, sigma = 0.4)
#' 
#' z3 <- x2z(x = c(10, 32, 112), mu = 55, sigma = 21)
#' @export
x2z <- function(x, mu = 0, sigma = 1){
  # load required packages
  suppressPackageStartupMessages({
    library(glue)
    library(tidyverse)
  })
  
  # unicode characters used to print math symbols, mu and sigma 
  a <- "\U03BC"            # Greek lowercase mu
  b <- "\U03C3"            # Greek lowercase sigma
  
  # formatting x data as a tibble
  tbl_x <- as_tibble(x)
  n <- length(tbl_x$value)
  
  print(glue("\n"))
  
  # for loop to calculate z for each element in tbl_x
  for (i in 1:n) {
    
    # data calculated: z-score (rounded to 4 decimal places, if 5 or more exist)
    #z <- (x - mu)/sigma
    z <- as.double(format(round((tbl_x$value[i] - mu)/sigma, 4), nsmall = 4))
    
    # output
    print(glue("If ", {a}, " = ", {mu}, " and ", {b}, " = ", {sigma}, ",\n"))
    print(glue("then, where x = ", {tbl_x$value[i]}, ", z = ", {z}, ".\n\n"))
    
    # increase counter on i
    i = i + 1
  }
}

#' Calculation of Raw X Values From Standard Z Scores
#' 
#' Convert numeric values of z from the standard normal distribution to x values
#' from a non-standard normal distribution.
#' @param z A vector of 1 or more numeric values of z.
#' @param mu The value of the population mean for the x distribution 
#' (0 = default value).
#' @param sigma The value of the population standard deviation for the x 
#' distribution (1 = default value).
#' @return A printed output of one x value for each input value of z.
#' @note Use of mu = 0 and sigma = 1 (the default values) implies that the x 
#' distribution is equivalent to the standard normal distribution.  Thus,
#' the 'x' value returned under default conditions will be the input z value.
#' @examples 
#' x1 <- z2x(z = -3.00)
#' 
#' x2 <- z2x(z = -3.00, mu = 5, sigma = 2)
#' 
#' x3 <- z2x(c(-1.96, 0.00, 2.56), mu = 3.34, sigma = 1.67)
#' @export
z2x <- function(z, mu = 0, sigma = 1){
  # load required packages
  suppressPackageStartupMessages({
    library(glue)
    library(tidyverse)
  })
  
  # unicode characters used to print math symbols, mu and sigma 
  a <- "\U03BC"            # Greek lowercase mu
  b <- "\U03C3"            # Greek lowercase sigma
  
  # data input with function invocation formatted as a tibble
  tbl_z <- as_tibble(z)
  n <- length(tbl_z$value)
  
  print(glue("\n"))
  
  # for loop to calculate z for each element in tbl_z
  for (i in 1:n) {
    
    # data calculated: z-score (rounded to 4 decimal places, if 5 or more exist)
    #z <- x <- (sigma * z) + mu
    x <- as.double(format(round((tbl_z$value[i] * sigma) + mu, 4), nsmall = 4))
    
    # output
    print(glue("If ", {a}, " = ", {mu}, " and ", {b}, " = ", {sigma}, ",\n"))
    print(glue("then, where z = ", {tbl_z$value[i]}, ", x = ", {x}, ".\n\n"))
    
    # increase counter on i
    i = i + 1
  }
}

#' Determining Area Under the Distribution Curve of x, z, or bar x 
#' 
#' Calculate the area under the distribution curve to the left, to the right, 
#' or between index values of x, z, or bar x.
#' @param type A character string indicating the distribution under 
#' consideration, i.e., x, z, or bar x.
#' @param value_1 The primary numeric index value for the distribution (required).
#' @param value_2 The secondary numeric index value for the distribution (only 
#' required for the 'between' direction, default = NULL)
#' @param mu The population mean of the distribution (default = 0).   
#' @param sigma The population standard deviation of the distribution (default = 1).
#' @param n The sample size (default = 1).
#' @param direction A character string indicating the directionality of the 
#' area relative to the index value(s).
#' @return A printed output of area (as a percent) under the distribution 
#' curve for x, z, or bar x.
#' @examples 
#' output1 <- area_relative_to_value(type = "z", value_1 = -2.58, 
#'                                   direction = "left")
#'                                   
#' output2 <- area_relative_to_value(type = "x", value_1 = 3.56, 
#'                                   value_2 = 4.12, mu = 4.00,
#'                                   sigma = 0.25, direction = "between")
#'                                   
#' output3 <- area_relative_to_value(type = "barx", value_1 = 12,
#'                                   mu = 10, sigma = 2, n = 20,
#'                                   direction = "right")
#' @export
area_relative_to_value <- function(type = c("x", "z", "bar_x"), 
                                   value_1, value_2 = NULL, 
                                   mu = 0, sigma = 1, n = 1, 
                                   direction = c("right", "left", "between")) {
  
  if (direction == "between") {
    
    # calculate the area between value_1 and value_2
    a <- pnorm(value_2, mean = mu, sd = sigma/sqrt(n)) - 
      pnorm(value_1, mean = mu, sd = sigma/sqrt(n));
    
    # output
    print(glue("The area between ", {type}, " = ", {value_1}, " and ", 
               {type}, " = ", {value_2}, " is ", {round(a, digits = 5)}, "."))
    
  }else if (direction == "left") {
    
    # calculate the area to the left of value_1
    a <- pnorm(value_1, mean = mu, sd = sigma/sqrt(n));
    
    # output
    print(glue("The area to the left of ", {type}, " = ", {value_1},
               " is ", {round(a, digits = 5)}, "."))
    
  }else{
    
    # calculate the area to the right of value_1
    a <- pnorm(value_1, mean = mu, sd = sigma/sqrt(n), lower.tail = FALSE);
    
    # output
    print(glue("The area to the right of ", {type}, " = ", {value_1},
               " is ", {round(a, digits = 5)}, "."))
  }
}

#' Determining Index Values of x or z Based on Area Under the Distribution 
#' Curve of x or z
#' 
#' Calculate the index value(s) of x or z to the left, to the right, or 
#' bounding a given area under the distribution curve of x or z.
#' @param type A character string indicating the distribution under 
#' consideration, i.e., x or z.
#' @param area The numeric value for the area under the x or z distribution 
#' curve (as a proportion, i.e., 0.95 for 95\%).
#' @param mu The population mean of the distribution (default = 0).   
#' @param sigma The population standard deviation of the distribution (default = 1).
#' @param direction A character string indicating the directionality of the 
#' area relative to the index value(s).
#' @return A printed output of index values of x or z associated with the area 
#' under the distribution curve of x or z.
#' @examples 
#' output1 <- value_relative_to_area(type = "z", area = 0.95,
#'                                   direction = "left")
#' 
#' output2 <- value_relative_to_value(type = "x", area = 0.15, mu = 4.00,
#'                                    sigma = 0.25, direction = "between")
#' @export

value_relative_to_area <- function(type = c("x", "z"), area, mu = 0, sigma = 1, 
                                   direction = c("right", "left", "between")) {
  
  if (direction == "between") {
    if (type == "z"){
    
    # calculate the bounding z values of area
    value_1 <- qnorm((1 - area)/2)
    value_2 <- qnorm((1 - area)/2, lower.tail = FALSE)

    # output
    print(glue("The z scores with ", {area*100}, "% of the area under the",
               " standard normal curve between them are z = (",
               {round(value_1, digits = 3)}, ",", {round(value_2, digits = 3)}, 
               ")."))
    
    } else if (type == "x"){
      
      # calculate the bounding z values of area
      value_1 <- qnorm((1 - area)/2)
      value_2 <- qnorm((1 - area)/2, lower.tail = FALSE)
      
      # calculate the bounding x values from the z values
      x_1 <- (value_1 * sigma) + mu
      x_2 <- (value_2 * sigma) + mu
      
      # output
      print(glue("The x values with ", {area*100}, "% of the area between" ,
                 " them are x = (", {round(x_1, digits = 3)}, ",",
                 {round(x_2, digits = 3)}, ")."))
      
    }
    
  } else if (direction == "left") {
    if (type == "z") {
      
      # calculate the bounding z value of area
      value_1 <- qnorm((1 - area)/2)
      
      # output
      print(glue("The z score with ", {area*100}, "% of the area under the",
                 " standard normal curve to the left of it is z = ", 
                 {round(value_1, digits = 3)}, "."))
    
    } else if (type == "x") {
      
      # calculate the bounding z value of area
      value_1 <- qnorm((1 - area)/2)
      
      # calculate the bounding x value from the z value
      x_1 <- (value_1 * sigma) + mu
      
      # output
      print(glue("The x value with ", {area*100}, "% of the area to the left", 
                 " of it is x = ", {round(x_1, digits = 3)}, "."))
    }
    
  } else {
    if (type == "z") {
      
      # calculate the bounding z value of area
      value_1 <- qnorm((1 - area)/2, lower.tail = FALSE)
      
      # output
      print(glue("The z score with ", {area*100}, "% of the area under the",
                 " standard normal curve to the right of it is z = ", 
                 {round(value_1, digits = 3)}, "."))
    
    } else if (type == "x") {
      
      # calculate the bounding z value of area
      value_1 <- qnorm((1 - area)/2, lower.tail = FALSE)
      
      # calculate the bounding x value from the z value
      x_1 <- (value_1 * sigma) + mu
      
      # output
      print(glue("The x value with ", {area*100}, "% of the area to the", 
                 "right of it is x = ", {round(value_1, digits = 3)}, "."))
    }
  }
}
