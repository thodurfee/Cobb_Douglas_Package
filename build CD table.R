#version = 9.2.15.a

#
# x1_name <- "red"
# x2_name <- "blue"
# a1 <- .6
# a2 <- .4
# b <- 1.5
# p1 <- 1
# p2 <- 2
# u <- 10
# w <- 100
# method <- "m"
#


Cobb_Douglas <- function(  x1_name
                           , x2_name
                           , a1
                           , a2
                           , b
                           , p1
                           , p2
                           , u
                           , w
                           , method
){


  ###################################################
  ##define defaults for missing conditions
  {

    #source('C:\\Users\\DurfTop\\Desktop\\RTesting\\my functions\\Cobb Douglas\\base\\defaults\\defaults.r')


    if(  missing( method )   ){
      method <- "m"
    }

    if( missing(x1_name) ){
      x1_name <- "x.1"
    }

    if( missing(x2_name)  ){
      x2_name <- "x.2"
    }


    if( missing(b)  ){
      b <- 1
    }

    if( missing(a1)  ){
      return("define a1 first")
    }

    if( missing(a2)  ){
      return("define a2 first")
    }

    if( missing(p1)  ){
      return("define prices")
    }

    if( missing(p2)  ){
      return("define prices")
    }

#
# come back later
    significant_terms <- c("curve", "add")

    if(x1_name %in% significant_terms){
      return("cannot give x1 that name")
    }


    if(x2_name %in% significant_terms){
      return("cannot give x2 that name")
    }



  }

  ###################################################
  #define functions derived from first order conditions

  # source('C:\\Users\\DurfTop\\Desktop\\RTesting\\my functions\\Cobb Douglas\\base\\foc functions\\foc functions.r')

  {#version 8.16.15


    m_lagrange <- function(x1,a1,x2,a2,lambda,w,p1,p2,b){
      (((x1)^a1)*((x2)^a2)*b) + lambda*(w - p1*x1 - p2*x2)
    }

    h_lagrange <- function(x1,a1,x2,a2,lambda,w,p1,p2,b){
      (p1*x1 + p2*x2) + lambda*(u - ((x1^a1)*(x2^a2))*b)
    }

    x1_con_d <- function(a1,a2,p1,p2,x2){
      (a1/a2)*(p2/p1)*x2
    }

    x2_con_d <- function(a1,a2,p1,p2,x1){
      (a2/a1)*(p1/p2)*x1
    }

    m_x1_unco <- function(a1,a2,w,p1,p2){
      (a1/a2)*(w/p1)*(1/(1+(a1/a2)))
    }

    m_x2_unco <- function(a1,a2,w,p1,p2){
      (a2/a1)*(w/p2)*(1/(1+(a2/a1)))
    }

    h_x1_unco <- function(a1,a2,p1,p2,u){
      ((a1/a2)*(p2/p1)*(u^(1/a2)))^(1/(1+(a1/a2)))
    }

    h_x2_unco <- function(a1,a2,p1,p2,u){
      ((a2/a1)*(p1/p2)*(u^(1/a1)))^(1/(1+(a2/a1)))
    }

    w_x1 <- function(p1,p2,x2,w){
      (-p2/p1)*x2 + (w/p1)
    }

    u_x1 <- function(u,p1,p2,a1,a2,b){
      (u/(b*(x2^a2)))^(1/a1)
    }

    w_x2 <- function(p1,p2,x1,w){
      (-p1/p2)*x1+(w/p2)
    }

    u_x2 <- function(u,x1,a1,a2,b){
      (u/(b*(x1^a1)))^(1/a2)
    }

    x_intercept <- function(w,p1){
      w/p1
    }

    y_intercept <- function(w,p2){
      w/p2
    }

    find_u <- function(x1,x2,a1,a2,b){
      (x1^a1)*(x2^a2)*b
    }

    find_w <- function(x1, x2, p1, p2){
      (p1*x1) + (p2*x2)
    }

  }

  ###################################################
  #find if I am using the hicks or the marshall method, these are acceptable input parameters

  {
    hicks_group <- c("Hicks", "hicks", "h", "H", 2, "hikcs", "hkisc", "hi", "hic")

    marshall_group <- c("marshall", "marshal", "Marshall", "Marshal", "m", "M",1, "masrlal", "mlsahr", "mar")


    verify_method <- function(in_method){
      for(i in 1:length(hicks_group)){
        if(hicks_group[i] == in_method){
          return("hicks")
        }
      }
      for(j in 1:length(marshall_group)){
        if(marshall_group[j] == in_method){
          return("marshall")
        }
      }
      return("marshall")
    }


    method <- verify_method(in_method = method)

  }

  ###################################################
  #calculate x1 and x2 star with the hicks method

  {
    find_x_star <- function(  in_method
                              , a1
                              , a2
                              , p1
                              , p2
                              , u
                              , w
    ){

      if(in_method == "hicks"){
        x1_star <- h_x1_unco(    a1 = a1
                                 , a2 = a2
                                 , p1 = p1
                                 , p2 = p2
                                 , u = u
        )

        x2_star <- h_x2_unco(    a1 = a1
                                 , a2 = a2
                                 , p1 = p1
                                 , p2 = p2
                                 , u = u
        )

        return( data.frame(x1_star, x2_star) )
      }

      else if(in_method == "marshall"){
        x1_star <- m_x1_unco(    a1 = a1
                                 , a2 = a2
                                 , p1 = p1
                                 , p2 = p2
                                 , w = w
        )

        x2_star <- m_x2_unco(    a1 = a1
                                 , a2 = a2
                                 , p1 = p1
                                 , p2 = p2
                                 , w = w
        )

        return( data.frame(x1_star, x2_star) )
      }

      else{
        return("improper definition of 'method'")
      }

    }
  }

  xstar <- find_x_star(  in_method = method
                         , a1 = a1
                         , a2 = a2
                         , p1 = p1
                         , p2 = p2
                         , u = u
                         , w = w)


  ###################################################
  #find w and u levels and generate teh domains for x1 abd x2

  {
    util <- find_u( x1 = as.numeric(xstar["x1_star"])
                    , x2 = as.numeric(xstar["x2_star"])
                    ,a1 = a1
                    ,a2 = a2
                    , b = b)

    wealth <- find_w( x1 = as.numeric(xstar["x1_star"])
                      , x2 = as.numeric(xstar["x2_star"])
                      ,p1 = p1
                      ,p2 = p2)



    yintercept <- y_intercept( w = wealth
                               , p2 = p2)

    xintercept <- x_intercept(w = wealth
                              , p1 = p1)



    x1_domain <- seq(from = 0, to = (2*xintercept), length.out =  100)
    x2_domain <- seq(from = 0, to = (2*yintercept), length.out =  100)

  }


  ###################################################
  #build wealth curve

  {
    wealth_curve <- sapply(x1_domain,
                           FUN = function(x1_domain) {w_x2(x1 = x1_domain
                                                           , p1 = p1
                                                           , p2 = p2
                                                           , w = wealth
                           )}
    )

    w_range <- cbind.data.frame(   x1_domain
                                   ,wealth_curve
                                   , replicate( expr = "wealth", n = length(wealth_curve) )
    )



    names(w_range) <- c(x1_name,x2_name, "curve")

  }

  ###################################################
  #build utility curve


  {

    utility_curve <- sapply(x1_domain,
                            FUN = function(x1_domain) {u_x2(  x1 = x1_domain
                                                              , a1 = a1
                                                              ,  a2 = a2
                                                              ,  u = util
                                                              ,  b = b)}
    )

    u_range <- cbind.data.frame( x1_domain
                                 , utility_curve
                                , replicate( expr = "utility", n = length(utility_curve) ) )


    names(u_range) <- c(x1_name,x2_name, "curve")

  }

  ###################################################
  #bring stored values together into tidy table

  curve_space <- rbind.data.frame( u_range, w_range )

  sig_values <- data.frame(  x1_star = xstar$x1_star
                           , x2_star = xstar$x2_star
                           , util = util
                           , wealth = wealth
                           , p1 = p1
                           , p2 = p2
                           , xintercept = xintercept
                           , yintercept = yintercept
                           , a1 = a1
                           , a2 = a2
                           , b = b
                           )

  output <- list(  curve_space
       , sig_values)

  names(output) <- c("data", "sig_values")

  return(output)
}

# minimalist
test_set <- Cobb_Douglas( w = 20,
              p1 = 1,
              p2 = 1,
              u = 100,
              a1 = .75,
              a2 = .25,
              b = 4,
              method = "hicks",
              x1_name = "pen",
              x2_name = "paper"
)
