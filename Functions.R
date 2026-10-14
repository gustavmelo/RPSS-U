
############ Modeling ############################################################################

### Function 1 - clustering ###

cluster_month <- function(dorig, m){

  set.seed(100)

  month_data <- dorig[dorig$month == m, ]

  mydata = month_data[3]
  max_aux1 = max(month_data[3])
  min_aux1 = min(month_data[3])

  if (max_aux1>0) {mydata[,1] = (mydata[,1] - min_aux1) / (max_aux1 - min_aux1)}

  wss      = NULL
  min_size = NULL
  clusterings = list()

  for(i in 1:6){
    c = kmeans(mydata,centers=i,iter.max = 15)
    wss[i] = sum(c$withinss)
    min_size[i] = min(c$size)
    clusterings[[i]] = c
  }

  # R² = variance explained by the k clusters (BSS/TSS = 1 - WSS(k)/TSS)
  wss_aux <- 1 - wss / wss[1]
  idx = which(min_size != 1)
  wss_aux2 = wss_aux[idx]
  wss_aux3 = rbind(wss_aux2, idx)
  if (last(wss_aux3[1,]) < 0.95) {
    column = which.max(abs(wss_aux3[1,]))
    aux_num_cluster = wss_aux3[2,column]
  } else {
    column = which(abs(wss_aux2) >= 0.95)[1]
    aux_num_cluster = wss_aux3[2,column]
  }

  clustering = clusterings[[aux_num_cluster]]

  centroids <- clustering$centers
  cluster_assignment <- clustering$cluster

  #Ascending order of the centroids

  order_idx <- order(centroids)
  new_index <- seq_along(order_idx)
  new_names <- setNames(new_index, order_idx)
  reordered_clusters <- new_names[as.character(cluster_assignment)]
  reordered_centroids <- centroids[order_idx]
  reordered_size <- as.vector(table(reordered_clusters))

  reordered_centroids = (reordered_centroids * (max_aux1 - min_aux1)) + min_aux1

  return(list(reordered_centroids, reordered_clusters, reordered_size))

}


### Function 2 - transition matrix ###

transition_matrix<-function(mun,m){

  ## Matrix non accumulated

  if(m<12){
    p_matrix = matrix(nrow = length(centroids[[(12*(mun-1))+m]]), ncol = length(centroids[[(12*(mun-1))+m+1]]), data = 0)
  }else{ #December to January case
    p_matrix = matrix(nrow = length(centroids[[(12*(mun-1))+m]]), ncol = length(centroids[[(12*(mun-1))+1]]), data = 0)
  }

  case <- as.data.frame(municip[[mun]])

  month_or <- case[case$month == m, ]

  if (m<12){

    month_dt <- case[case$month == m+1, ]

    for (i in 1:nrow(p_matrix)) {
      total_or <- sum(month_or$cluster == i)

      if (total_or > 0) {

        for (j in 1:ncol(p_matrix)) {
          transitions <- sum(month_or$cluster == i &
                             month_dt$cluster == j)

          p_matrix[i, j] <- transitions / total_or
        }
      }
    }
  } else{ #December to January case

    month_dt <- case[case$month == 1, ]

    month_or <- month_or %>% mutate(next_year = year + 1)
    month_dt <- month_dt %>% rename(next_year = year)

    trans_dec_jan <- month_or %>%
      inner_join(month_dt, by = "next_year", suffix = c("_dec", "_jan"))

    for (i in 1:nrow(p_matrix)) {
      total_dec <- sum(trans_dec_jan$cluster_dec == i)

      if (total_dec > 0) {

        for (j in 1:ncol(p_matrix)) {
          transitions <- sum(trans_dec_jan$cluster_dec == i &
                             trans_dec_jan$cluster_jan == j)

          p_matrix[i, j] <- transitions / total_dec
        }

      }
    }
  }

  ## Matrix accumulated

  p_matrix_accum <- p_matrix
  for(z in 2:ncol(p_matrix)){
    p_matrix_accum[,z]<-p_matrix_accum[,z-1]+p_matrix_accum[,z]
  }

  return(list(p_matrix, p_matrix_accum))

}



############ Future Scenarios Simulation #########################################################

# Function 3 - cluster simulation

simulat_state <- function(state1, matrix_tr, rand){
  column=1
  while (column <= ncol(matrix_tr)) {
    if(column==1 && rand<=matrix_tr[state1,column]) {
      state2=column
      column=ncol(matrix_tr)+1
    }else if(matrix_tr[state1,column]<rand && rand<=matrix_tr[state1,column+1]) {
      state2=column+1
      column=ncol(matrix_tr)+1
    }else {
      column=column+1
    }
  }
  return(state2)
}

# Function 4 - cluster element sampling

simulat_value <- function(possible_values,rand){
  all_values <- c(possible_values[[3]], possible_values[[5]][1])
  indice <- ceiling(rand * length(all_values))
  drawn_value <- all_values[indice]
  return(drawn_value)
}

