#version 8.31.15:8:12

#graphing variation
variation_sets

var_scheme <- data.frame(  wealth_color = "#aaaa0044"
                           , utility_color = "#aa00aa44"
                           , wealth_thick = 2
                           , utility_thick = 2
                           , wealth_style = "solid"
                           , utility_style = "solid"
                           , x_star_color = "#00000044"
                           , x_star_size = 4
                           , x_star_style = 8)




e0_scheme <- data.frame(  wealth_color = "#aa000044"
             , utility_color = "#aa000044"
             , wealth_thick = 2
             , utility_thick = 2
             , wealth_style = "solid"
             , utility_style = "solid"
             , x_star_color = "#00000044"
             , x_star_size = 4
             , x_star_style = 8)

e1_scheme <- data.frame(  wealth_color = "#00aa0044"
             , utility_color = "#00aa0044"
             , wealth_thick = 2
             , utility_thick = 2
             , wealth_style = "solid"
             , utility_style = "solid"
             , x_star_color = "#00000044"
             , x_star_size = 4
             , x_star_style = 8)
e2_scheme <- data.frame(  wealth_color = "#0000aa44"
             , utility_color = "#0000aa44"
             , wealth_thick = 2
             , utility_thick = 2
             , wealth_style = "solid"
             , utility_style = "solid"
             , x_star_color = "#00000044"
             , x_star_size = 4
             , x_star_style = 8)
e3_scheme <- data.frame(  wealth_color = "#aaaa0044"
             , utility_color = "#aaaa0044"
             , wealth_thick = 2
             , utility_thick = 2
             , wealth_style = "solid"
             , utility_style = "solid"
             , x_star_color = "#00000044"
             , x_star_size = 4
             , x_star_style = 8)
e4_scheme <- data.frame(  wealth_color = "#aaaaaa44"
             , utility_color = "#aaaaaa44"
             , wealth_thick = 2
             , utility_thick = 2
             , wealth_style = "solid"
             , utility_style = "solid"
             , x_star_color = "#00000044"
             , x_star_size = 4
             , x_star_style = 8)

extended_scheme <- list(e0_scheme, e1_scheme, e2_scheme, e3_scheme, e4_scheme)





var_table <- var_test

var_pallete <- var_scheme

plot_variation <- function(var_table, var_pallete, base_plot){

  #initialize terms

   require(ggplot2)

  variation_names <- c("e0", "e1", "e2", "e3", "e4")

  new_set <- list(list(),list(),list(),list(),list())

  var_pallete <- extended_scheme

  names(var_pallete) <- c("e0", "e1", "e2", "e3", "e4")


  #draft new set for more easily accessible data

  for(i in 1:length(new_set)){
    new_set[i] <- list(var_table$variation_sets[variation_names[i]][[1]])
  }

  names(new_set) <- c("e0", "e1", "e2", "e3", "e4")


  #generate base layer

  original_plot #base_plot


  #add on new layers

  for(i in 1:length(new_set)){

  plot_cd(table = new_set[[i]]
          , pallete = class(var_pallete["e4"])
            , add = TRUE)

  }

  #
  #   e0_set <- var_table$variation_sets["e0"]][[1]]#[[1]]
  #
  #   e1_set <- var_table$variation_sets["e1"][[1]]#[[1]]
  #
  #   e2_set <- var_table$variation_sets["e2"][[1]]#[[1]]
  #
  #   e3_set <- var_table$variation_sets["e3"][[1]]#[[1]]
  #
  #   e4_set <- var_table$variation_sets["e4"][[1]]#[[1]]
  #
  #

  most_recent_layer <- original_plot

  inner_set <- var_table$variation_sets[[5]]#$data
  var_pallete <- extedned_var_scheme[[5]][[1]]


  most_recent_layer



  add_layer <- function(inner_set, var_pallete){

    most_recent_layer <- most_recent_layer + geom_line(data = as.data.frame(subset(inner_set$data, curve == "wealth"))
                                                       , aes(   x =  x1_star
                                                                , y =  x2_star
                                                                )
                                                       , color = var_pallete$wealth_color
                                                       , linetype = as.character(var_pallete$wealth_style)
                                                       , size = as.numeric(var_pallete$wealth_thick)
                                                       , lineend = "round"
                                                       , add = TRUE
                                         ) + geom_point(
                                                         data = inner_set$sig_values
                                                         , aes(  x = x1_star
                                                                 , y = x2_star
                                                                 )
                                                         , size = as.numeric(var_pallete$x_star_size)
                                                         , shape = as.numeric(var_pallete$x_star_style)
                                                         , color = var_pallete$x_star_color
                                                         , add = TRUE
                                         ) + geom_line(
                                           data = as.data.frame(subset(inner_set$data, curve == "utility"))
                                           , color = var_pallete$utility_color
                                           , aes(   x =  x1_star
                                                    , y =  x2_star
                                                    )
                                           , linetype = as.character( var_pallete$utility_style)
                                           , size = var_pallete$utility_thick
                                           , add = TRUE
                                           ) + xlim(0,1000)

    return(most_recent_layer)

  }

  j <- 5





  for(j in 1:5){
    add_layer(  inner_set = new_set[[j]]
              , var_pallete = extedned_var_scheme[[j]][[1]]
              )
  }

  most_recent_layer + xlim(0,1000)





  #
  #  orig_plus_e0 <-  {
  #   original_plot + geom_line(  data = as.data.frame( subset(e0_set$data, curve == "wealth") )
  #                               , aes( x =  x1_star
  #                                      , y =  x2_star)
  #                               , color = var_pallete$e0$wealth_color
  #                               , linetype = as.character(var_pallete$e0$wealth_style)
  #                               , size = var_pallete$e0$wealth_thick
  #                               , lineend = "round"
  #                               # , add = TRUE
  #
  #   ) + geom_point(data = e0_set$sig_values,
  #                  aes(x = x1_star,
  #                      y = x2_star
  #                  )
  #                  , size = var_pallete$e0$x_star_size
  #                  , shape = (var_pallete$e0$x_star_style)
  #                  , color = var_pallete$e0$x_star_color
  #   ) + geom_line(data = as.data.frame( subset(e0_set$data, curve == "utility") )
  #                 , color = var_pallete$e0$utility_color
  #                 , aes(   x =  x1_star
  #                          , y =  x2_star
  #                 )
  #                 , linetype = as.character(var_pallete$e0$utility_style)
  #                 , size = var_pallete$e0$utility_thick
  #   )
  #
  #   }
  #
  #
  #  e0_plus_e1 <-  {
  #    orig_plus_e0 + geom_line(  data = as.data.frame( subset(e1_set$data, curve == "wealth") )
  #                               , aes( x =  x1_star
  #                                      , y =  x2_star)
  #                               , color = var_pallete$e1$wealth_color
  #                               , linetype = as.character(var_pallete$e1$wealth_style)
  #                               , size = var_pallete$e1$wealth_thick
  #                               , lineend = "round"
  #                               # , add = TRUE
  #   )  + geom_point(data = e1_set$sig_values,
  #                  aes(x = x1_star,
  #                      y = x2_star
  #                  )
  #                  , size = var_pallete$e1$x_star_size
  #                  , shape = (var_pallete$e1$x_star_style)
  #                  , color = var_pallete$e1$x_star_color
  #   ) + geom_line(data = as.data.frame( subset(e1_set$data, curve == "utility") )
  #                 , color = var_pallete$e1$utility_color
  #                 , aes(   x =  x1_star
  #                          , y =  x2_star
  #                 )
  #                 , linetype = as.character(var_pallete$e1$utility_style)
  #                 , size = var_pallete$utility_thick
  #   )
  #
  #   }
  #
  #
  #  e1_plus_e2 <-  {
  #    e0_plus_e1 + geom_line(  data = as.data.frame( subset(e2_set$data, curve == "wealth") )
  #                               , aes( x =  x1_star
  #                                      , y =  x2_star)
  #                               , color = var_pallete$e2$wealth_color
  #                               , linetype = as.character(var_pallete$e2$wealth_style)
  #                               , size = var_pallete$e2$wealth_thick
  #                               , lineend = "round"
  #                               # , add = TRUE
  #   ) + geom_point(data = e2_set$sig_values,
  #                  aes(x = x1_star,
  #                      y = x2_star
  #                  )
  #                  , size = var_pallete$e2$x_star_size
  #                  , shape = (var_pallete$e2$x_star_style)
  #                  , color = var_pallete$e2$x_star_color
  #   ) + geom_line(data = as.data.frame( subset(e2_set$data, curve == "utility") )
  #                 , color = var_pallete$e2$utility_color
  #                 , aes(   x =  x1_star
  #                          , y =  x2_star
  #                 )
  #                 , linetype = as.character(var_pallete$e2$utility_style)
  #                 , size = var_pallete$utility_thick
  #   )
  #
  #   }
  #
  #
  #  e2_plus_e3 <-  {
  #    e1_plus_e2 + geom_line(  data = as.data.frame( subset(e3_set$data, curve == "wealth") )
  #                               , aes( x =  x1_star
  #                                      , y =  x2_star)
  #                               , color = var_pallete$e3$wealth_color
  #                               , linetype = as.character(var_pallete$e3$wealth_style)
  #                               , size = var_pallete$e3$wealth_thick
  #                               , lineend = "round"
  #                               # , add = TRUE
  #   )  + geom_point(data = e3_set$sig_values,
  #                  aes(x = x1_star,
  #                      y = x2_star
  #                  )
  #                  , size = var_pallete$e3$x_star_size
  #                  , shape = (var_pallete$e3$x_star_style)
  #                  , color = var_pallete$e3$x_star_color
  #   ) + geom_line(data = as.data.frame( subset(e3_set$data, curve == "utility") )
  #                 , color = var_pallete$e3$utility_color
  #                 , aes(   x =  x1_star
  #                          , y =  x2_star
  #                 )
  #                 , linetype = as.character(var_pallete$e3$utility_style)
  #                 , size = var_pallete$e3$utility_thick
  #   )
  #
  #   }
  #
  #
  #  e3_plus_e4 <-  {
  #    e2_plus_e3 + geom_line(  data = as.data.frame( subset(e4_set$data, curve == "wealth") )
  #                               , aes( x =  x1_star
  #                                      , y =  x2_star)
  #                               , color = var_pallete$e4$wealth_color
  #                               , linetype = as.character(var_pallete$e4$wealth_style)
  #                               , size = var_pallete$e4$wealth_thick
  #                               , lineend = "round"
  #                               # , add = TRUE
  #   )  + geom_point(data = e4_set$sig_values,
  #                  aes(x = x1_star,
  #                      y = x2_star
  #                  )
  #                  , size = var_pallete$e4$x_star_size
  #                  , shape = (var_pallete$e4$x_star_style)
  #                  , color = var_pallete$e4$x_star_color
  #   ) + geom_line(data = as.data.frame( subset(e4_set$data, curve == "utility") )
  #                 , color = var_pallete$e4$utility_color
  #                 , aes(   x =  x1_star
  #                          , y =  x2_star
  #                 )
  #                 , linetype = as.character(var_pallete$e4$utility_style)
  #                 , size = var_pallete$e4$utility_thick
  #   )
  #
  #  }
  #
  #
  output <- most_recent_layer  + xlim(0, max(e0_set$sig_values$xintercept*1.1
                                             , e1_set$sig_values$xintercept*1.1
                                             , e2_set$sig_values$xintercept*1.1
                                             , e3_set$sig_values$xintercept*1.1
                                             , e4_set$sig_values$xintercept*1.1
  )
  ) + ylim(0, max(e0_set$sig_values$yintercept*1.1
                  , e1_set$sig_values$yintercept*1.1
                  , e2_set$sig_values$yintercept*1.1
                  , e3_set$sig_values$yintercept*1.1
                  , e4_set$sig_values$yintercept*1.1
  )
  )
  add_layer(  inner_set = new_set[[j]]
              , var_pallete = extedned_var_scheme[[j]][[1]]
  )

  return(output)

}

plot_variation(var_table = var_test
               , var_pallete = var_scheme
               , base_plot = original_plot)
