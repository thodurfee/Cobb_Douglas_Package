## version 8.31.15:12:22

#graphing the Cobb Douglas Table

scheme <- data.frame(  wealth_color = "#00aaaaff"
                       , utility_color = "#aa0000ff"
                       , wealth_thick = 2
                       , utility_thick = 2
                       , wealth_style = "solid"
                       , utility_style = "solid"
                       , x_star_color = "#000000ff"
                       , x_star_size = 4
                       , x_star_style = 8)


plot_cd <- function(table, pallete, add){
  require(ggplot2)

  if(missing(add)){
    add <- FALSE
  }

  wealth <- as.data.frame( subset(table$data, curve == "wealth") )

  utility <- as.data.frame( subset(table$data, curve == "utility") )

  names(wealth) <- c("x1", "x2", "curve")
  names(utility) <- c("x1", "x2", "curve")

  sig_values <- as.data.frame(table$sig_values)

  base <- ggplot(   data = wealth
                    , aes( x =  x1
                           ,y =  x2
                    )
                    , add = add
  )



  wlayer <- base + geom_line(  data = wealth
                               , aes( x =  x1
                                      , y =  x2)
                               , color = pallete$wealth_color
                               , linetype = as.character(pallete$wealth_style)
                               , size = pallete$wealth_thick
                               , lineend = "round"
  )


  labslayer <- wlayer + ggtitle( paste(names(table$data)[1], " vs ", names(table$data)[2]) )+ xlab(names(table$data)[1] ) + ylab(names(table$data)[2])


  axislayer <- labslayer + geom_hline() + geom_vline()

  ulayer <-  axislayer + geom_line(data = utility
                                   , color = pallete$utility_color
                                   , aes(   x =  x1
                                            , y =  x2
                                   )
                                   , linetype = as.character(pallete$utility_style)
                                   , size = pallete$utility_thick
  )



  starlayer <- ulayer + geom_point(data = sig_values,
                                   aes(x = x1_star,
                                       y = x2_star
                                   )
                                   , size = pallete$x_star_size
                                   , shape = (pallete$x_star_style)
                                   , color = pallete$x_star_color
  )

  shell <- starlayer + xlim(0, max(sig_values$xintercept*1.1, sig_values$xintercept2*1.1)) + ylim(0, max(sig_values$yintercept*1.1, sig_values$yintercept2*1.1))

  return(shell)

}

original_plot <- plot_cd(pallete = scheme
        , table = test_set)

original_plot