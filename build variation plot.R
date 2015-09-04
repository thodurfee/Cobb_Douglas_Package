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

var_pallete <- extended_scheme

plot_variation <- function(var_table, var_pallete, base_plot){

  #initialize terms

  require(ggplot2)

  variation_names <- c("e0", "e1", "e2", "e3", "e4")

  new_set <- list(list(),list(),list(),list(),list())

  var_pallete <- extended_scheme

  names(var_pallete) <- c("e0", "e1", "e2", "e3", "e4")

  goods <- c( names(new_set[[1]]$data)[1]
              ,names(new_set[[1]]$data)[2])


  #draft new set for more easily accessible data

  for(i in 1:length(new_set)){
    new_set[i] <- list(var_table$variation_sets[variation_names[i]][[1]])
  }

  names(new_set) <- c("e0", "e1", "e2", "e3", "e4")






  base <- ggplot(   data = subset(new_set[["e0"]]$data, curve == "wealth" )
                    , aes_string(   x =  goods[1]
                                   ,y = goods[2]
                    )


  ) + geom_line(  data = subset(new_set[["e0"]]$data, curve == "wealth" )
                  , aes_string( x =  goods[1]
                                , y =  goods[2]
                  )
                  , color = var_pallete["e0"][[1]]$wealth_color
                  , linetype = as.character(var_pallete["e0"][[1]]$wealth_style)
                  , size = var_pallete["e0"][[1]]$wealth_thick
                  , lineend = "round"

  ) + geom_line(  data = subset(new_set[["e1"]]$data, curve == "wealth" )
                  , aes_string( x =  goods[1]
                                , y = goods[2])
                  , color = var_pallete["e1"][[1]]$wealth_color
                  , linetype = as.character(var_pallete["e1"][[1]]$wealth_style)
                  , size = var_pallete["e1"][[1]]$wealth_thick
                  , lineend = "round"

  )+ geom_line(  data = subset(new_set[["e2"]]$data, curve == "wealth" )
                 , aes_string( x =  goods[1]
                               , y =  goods[2])
                 , color = var_pallete["e2"][[1]]$wealth_color
                 , linetype = as.character(var_pallete["e2"][[1]]$wealth_style)
                 , size = var_pallete["e2"][[1]]$wealth_thick
                 , lineend = "round"

  )+ geom_line(  data = subset(new_set[["e3"]]$data, curve == "wealth" )
                 , aes_string( x =  goods[1]
                               , y =  goods[2])
                 , color = var_pallete["e3"][[1]]$wealth_color
                 , linetype = as.character(var_pallete["e3"][[1]]$wealth_style)
                 , size = var_pallete["e3"][[1]]$wealth_thick
                 , lineend = "round"

  )+ geom_line(  data = subset(new_set[["e4"]]$data, curve == "wealth" )
                 , aes_string( x =  goods[1]
                               , y = goods[2])
                 , color = var_pallete["e4"][[1]]$wealth_color
                 , linetype = as.character(var_pallete["e4"][[1]]$wealth_style)
                 , size = var_pallete["e4"][[1]]$wealth_thick
                 , lineend = "round"

  )

  add_utility <-  base + geom_line(data = subset(new_set[["e0"]]$data, curve == "utility" )
                                   , color = var_pallete["e0"][[1]]$utility_color
                                   , aes_string(   x =  goods[1]
                                                   , y =  goods[2]
                                   )
                                   , linetype = as.character(var_pallete["e0"][[1]]$utility_style)
                                   , size = var_pallete["e0"][[1]]$utility_thick
                                   , stat = "identity"
  ) + geom_line(data = subset(new_set[["e1"]]$data, curve == "utility" )
                , color = var_pallete["e1"][[1]]$utility_color
                , aes_string(   x =  goods[1]
                                , y =  goods[2]
                )
                , linetype = as.character(var_pallete["e1"][[1]]$utility_style)
                , size = var_pallete["e1"][[1]]$utility_thick
                , stat = "identity"
  ) + geom_line(data = subset(new_set[["e1"]]$data, curve == "utility" )
                , color = var_pallete["e1"][[1]]$utility_color
                , aes_string(   x =  goods[1]
                                , y =  goods[2]
                )
                , linetype = as.character(var_pallete["e1"][[1]]$utility_style)
                , size = var_pallete["e1"][[1]]$utility_thick
                , stat = "identity"
  ) + geom_line(data = subset(new_set[["e2"]]$data, curve == "utility" )
                , color = var_pallete["e2"][[1]]$utility_color
                , aes_string(   x =  goods[1]
                                , y =  goods[2]
                )
                , linetype = as.character(var_pallete["e2"][[1]]$utility_style)
                , size = var_pallete["e2"][[1]]$utility_thick
                , stat = "identity"
  ) + geom_line(data = subset(new_set[["e3"]]$data, curve == "utility" )
                , color = var_pallete["e3"][[1]]$utility_color
                , aes_string(   x =  goods[1]
                                , y =  goods[2]
                )
                , linetype = as.character(var_pallete["e3"][[1]]$utility_style)
                , size = var_pallete["e3"][[1]]$utility_thick
                , stat = "identity"
  ) + geom_line(data = subset(new_set[["e4"]]$data, curve == "utility" )
                , color = var_pallete["e4"][[1]]$utility_color
                , aes_string(   x =  goods[1]
                                , y =  goods[2]
                )
                , linetype = as.character(var_pallete["e4"][[1]]$utility_style)
                , size = var_pallete["e4"][[1]]$utility_thick
                , stat = "identity"
  )

  add_equib <- add_utility + geom_point(data = new_set[["e0"]]$sig_values,
                                        aes_string(x = goods[1],
                                                   y = goods[2]
                                        )
                                        , size = var_pallete["e0"][[1]]$x_star_size
                                        , shape = (var_pallete["e0"][[1]]$x_star_style)
                                        , color = var_pallete["e0"][[1]]$x_star_color
                                        , stat = "identity"
  )+ geom_point(data = new_set[["e1"]]$sig_values,
                aes_string(x = goods[1],
                           y = goods[2]
                )
                , size = var_pallete["e1"][[1]]$x_star_size
                , shape = (var_pallete["e1"][[1]]$x_star_style)
                , color = var_pallete["e1"][[1]]$x_star_color
                , stat = "identity"
  )+ geom_point(data = new_set[["e2"]]$sig_values,
                aes_string(x = goods[1],
                           y = goods[2]
                )
                , size = var_pallete["e2"][[1]]$x_star_size
                , shape = (var_pallete["e2"][[1]]$x_star_style)
                , color = var_pallete["e2"][[1]]$x_star_color
                , stat = "identity"
  )+ geom_point(data = new_set[["e3"]]$sig_values,
                aes_string(x = goods[1],
                           y = goods[2]
                )
                , size = var_pallete["e3"][[1]]$x_star_size
                , shape = (var_pallete["e3"][[1]]$x_star_style)
                , color = var_pallete["e3"][[1]]$x_star_color
                , stat = "identity"
  )+ geom_point(data = new_set[["e4"]]$sig_values,
                aes_string(x = goods[1],
                           y = goods[2]
                )
                , size = var_pallete["e4"][[1]]$x_star_size
                , shape = (var_pallete["e4"][[1]]$x_star_style)
                , color = var_pallete["e4"][[1]]$x_star_color
                , stat = "identity"
  )


  add_shell <- add_equib + ggtitle( paste(names(new_set[[1]]$data)[1], " vs ", names(new_set[[1]]$data)[2]) )+ xlab(names(new_set[[1]]$data)[1] ) + ylab(names(new_set[[1]]$data)[2]) + geom_hline() + geom_vline()

  x_lim <-  max(new_set[[1]]$sig_values$xintercept*1.1
                , new_set[[2]]$sig_values$xintercept*1.1
                , new_set[[3]]$sig_values$xintercept*1.1
                , new_set[[4]]$sig_values$xintercept*1.1
                , new_set[[5]]$sig_values$xintercept*1.1
  )

  y_lim <-  max(new_set[[1]]$sig_values$yintercept*1.1
                , new_set[[2]]$sig_values$yintercept*1.1
                , new_set[[3]]$sig_values$yintercept*1.1
                , new_set[[4]]$sig_values$yintercept*1.1
                , new_set[[5]]$sig_values$yintercept*1.1
  )


  output <- add_shell + xlim(0, x_lim) + ylim(0, y_lim)

  output

  return(output)


}



plot_variation(var_table = var_test
               , var_pallete = var_pallete
               , base_plot = original_plot)
