#version 9.2.15.a
#add compensating and equivalent variation to an existing plot space




calculate_variation <- function(set, dp1, dp2){

  add_variation <- function(dp1, dp2, set){

    goods <- names(set$data)

    variant_table <- Cobb_Douglas(w = set$sig_values["wealth"],
                                  p1 = as.numeric(set$sig_values["p1"])*dp1,
                                  p2 = as.numeric(set$sig_values["p2"])*dp2,
                                  u = as.numeric(set$sig_values["util"]),
                                  a1 = as.numeric(set$sig_values["a1"]),
                                  a2 = as.numeric(set$sig_values["a2"]),
                                  b = as.numeric(set$sig_values["b"]),
                                  method = "hicks",
                                  x1_name = goods[1],
                                  x2_name = goods[2])


    return(variant_table)

  }

  var_set <- add_variation( dp1 = dp1, dp2 = dp2, set = set)

  set$data <- cbind(set$data, replicate(expr = "base", n = length(set$data[,1])))

  names(set$data)[4] <- c("version")

  var_set$data <- cbind(var_set$data, replicate(expr = "variation", n = length(var_set$data[,1])))

  names(var_set$data)[4] <- c("version")

  set$sig_values <- cbind(set$sig_values, replicate(expr = "base", n = length(set$sig_values[,1])))

  names(set$sig_values)[12] <- c("version")

  var_set$sig_values <- cbind(var_set$sig_values, replicate(expr = "variation", n = length(var_set$sig_values[,1])))

  names(var_set$sig_values)[12] <- c("version")

  goods <- names(set$sig_values)


  e0 <- Cobb_Douglas(w = set$sig_values["wealth"],
                     p1 = as.numeric(set$sig_values["p1"])*dp1,
                     p2 = as.numeric(set$sig_values["p2"])*dp2,
                     u = as.numeric(set$sig_values["util"]),
                     a1 = as.numeric(set$sig_values["a1"]),
                     a2 = as.numeric(set$sig_values["a2"]),
                     b = as.numeric(set$sig_values["b"]),
                     method = "h",
                     x1_name = as.character(goods[1]),
                     x2_name = as.character(goods[2]))


  e0$data <- cbind(e0$data, replicate(expr = "e0", n = length(e0$data[,1])))

  names(e0$data)[4] <- c("version")




  e1 <- Cobb_Douglas(w = e0$sig_values["wealth"],
                     p1 = as.numeric(e0$sig_values["p1"]),
                     p2 = as.numeric(e0$sig_values["p2"]),
                     u = as.numeric(e0$sig_values["util"]),
                     a1 = as.numeric(e0$sig_values["a1"]),
                     a2 = as.numeric(e0$sig_values["a2"]),
                     b = as.numeric(set$sig_values["b"]),
                     method = "h",
                     x1_name = as.character(goods[1]),
                     x2_name = as.character(goods[2]))


  e1$data <- cbind(e1$data, replicate(expr = "e1", n = length(e1$data[,1])))

  names(e1$data)[4] <- c("version")



  e2 <- Cobb_Douglas(w = set$sig_values["wealth"],
                     p1 = as.numeric(e0$sig_values["p1"]),
                     p2 = as.numeric(e0$sig_values["p2"]),
                     u = as.numeric(set$sig_values["util"]),
                     a1 = as.numeric(set$sig_values["a1"]),
                     a2 = as.numeric(set$sig_values["a2"]),
                     b = as.numeric(set$sig_values["b"]),
                     method = "h",
                     x1_name = as.character(goods[1]),
                     x2_name = as.character(goods[2]))



  e2$data <- cbind(e2$data, replicate(expr = "e2", n = length(e2$data[,1])))

  names(e2$data)[4] <- c("version")


  e3 <- Cobb_Douglas(w = e0$sig_values["wealth"],
                     p1 = as.numeric(set$sig_values["p1"]),
                     p2 = as.numeric(set$sig_values["p2"]),
                     u = as.numeric(e0$sig_values["util"]),
                     a1 = as.numeric(set$sig_values["a1"]),
                     a2 = as.numeric(set$sig_values["a2"]),
                     b = as.numeric(set$sig_values["b"]),
                     method = "h",
                     x1_name = as.character(goods[1]),
                     x2_name = as.character(goods[2]))


  e3$data <- cbind(e3$data, replicate(expr = "e3", n = length(e3$data[,1])))

  names(e3$data)[4] <- c("version")



  e4 <- Cobb_Douglas(w = set$sig_values["wealth"],
                     p1 = as.numeric(set$sig_values["p1"]),
                     p2 = as.numeric(set$sig_values["p2"]),
                     u = as.numeric(set$sig_values["util"]),
                     a1 = as.numeric(set$sig_values["a1"]),
                     a2 = as.numeric(set$sig_values["a2"]),
                     b = as.numeric(set$sig_values["b"]),
                     method = "h",
                     x1_name = as.character(goods[1]),
                     x2_name = as.character(goods[2]))


  e4$data <- cbind(e4$data, replicate(expr = "e4", n = length(e4$data[,1])))

  names(e4$data)[4] <- c("version")




  variation_sets <- list(e0, e1, e2, e3, e4)

  names(variation_sets) <- c("e0" , "e1" , "e2" , "e3" , "e4")

  compensating_variation <- (variation_sets$e1$sig_values["wealth"] - variation_sets$e2$sig_values["wealth"])

  equivalent_variation <- (variation_sets$e3$sig_values["wealth"] - variation_sets$e4$sig_values["wealth"])

  massive_output <- list(variation_sets, compensating_variation, equivalent_variation)

  names(massive_output) <- c("variation_sets", "compensating_variation", "equivalent_variation")

  return(massive_output)

}


var_test <- calculate_variation(set = test_set
                                , dp1 = 2
                                , dp2 = 4)



