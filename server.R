max_name_length <- 10

# Returns 2 if Jaro-Winkler score is above threshold t
# with maximum prefix length of p
is_jw <- function(m,l,t,p) {
  if (m > p) {
    wt_p <- 0.1 * p
  } else {
    wt_p <- 0.1 * m
  }
  m_l <- m / l
  sc <- ((3/2) * (t - wt_p) / (1 - wt_p)) - 0.5
  if (m_l >= sc) {
    return(2)
  } else {
    return(0)
  }
}

is_jw_2 <- function(m,l,t,p) {
  if (m > p) {
    wt_p <- 0.1 * p
  } else {
    wt_p <- 0.1 * m
  }
  m_l = m / l
  sc <- (3 * (t - wt_p) / (1 - wt_p)) - 2
  if (m_l >= sc) {
    return(2)
  } else {
    return(0)
  }
}

# Returns 1 if difference between same characters
# and twice number of different characters is
# above CMD threshold of t
is_cmd <- function(m,l,t,r) {
  if (m %% 2) {
    maxrep <- m / 2
  } else {
    maxrep <- (m - 1) / 2
  }
  if (r > maxrep) {
    pref_rep = maxrep
  } else {
    pref_rep = r
  }
  if (m - pref_rep - (2*(l-m)) >= t) {
    return(1)
  } else {
    return(0)
  }
}

is_cmd_2 <- function(m,l,t,r) {
  if (m %% 2) {
    maxrep <- m / 2
  } else {
    maxrep <- (m - 1) / 2
  }
  if (r > maxrep) {
    pref_rep = maxrep
  } else {
    pref_rep = r
  }
  if (m - pref_rep - ((l-m)) >= t) {
    return(1)
  } else {
    return(0)
  }
}

#Create matrix of totals of above two functions
# 0 = both below threshold
# 1 = CMD above threshold, JW below
# 2 = CMD below threshold, JW above
# 3 = both above threshold
upd_mat <- function(mat,t_j, t_c,p, r) {
  for (row in 1:nrow(mat)) {
    for (col in 1:ncol(mat)) {
      if (col <= row) {
        mat[row,col] <- is_jw(col,row,t_j,p) + is_cmd(col,row,t_c, r)
      }
    }
  }
  return(mat)
}

upd_mat_2 <- function(mat,t_j, t_c,p, r) {
  for (row in 1:nrow(mat)) {
    for (col in 1:ncol(mat)) {
      if (col <= row) {
        mat[row,col] <- is_jw_2(col,row,t_j,p) + is_cmd_2(col,row,t_c, r)
      }
    }
  }
  return(mat)
}

library(ggplot2)
library(reshape)
library(scales)


colourful <- c("beige","orange", "red", "blue")
verygrey <- c("gray96","gray42","gray22","gray68")

# Function to generate matrix mappings for strings upto 8 length
# Plot the results as a heatmap
# Red highlights values of "matched" characters and string length
# where data will be filtered out possibly incorrectly
# Orange highlights unnecessary records passing through filter
newplot <- function(t,c,p,r) {
  scores <- matrix(,nrow=max_name_length, ncol=max_name_length)
  scores <- upd_mat(scores,t,c,p,r)
  scores_m <- melt(scores)
  ggplot(scores_m, aes(x = X1, y = X2)) +
    geom_tile(data=scores_m, aes(fill=value), color="white") +
    scale_fill_gradientn(limits=c(0,3),colours=verygrey,
                         values=rescale(c(0,1, 2, 3)),
                         guide="colorbar", na.value="transparent") +
    scale_x_discrete(breaks=seq(1,max_name_length,1), limits=seq(1,max_name_length,1)) +
    scale_y_discrete(breaks=seq(1,max_name_length,1), limits=seq(1,max_name_length,1))
}

newplot(0.9,0,4,0)

newplot_2 <- function(t,c,p,r) {
  scores <- matrix(,nrow=max_name_length, ncol=max_name_length)
  scores <- upd_mat_2(scores,t,c,p,r)
  scores_m <- melt(scores)
  colourful <- c("beige","orange", "red", "blue")
  verygrey <- c("gray32","gray42","gray52","gray62")
  ggplot(scores_m, aes(x = X1, y = X2)) +
    geom_tile(data=scores_m, aes(fill=value), color="white") +
    scale_fill_gradientn(limits=c(0,3),colours=colourful,
                         values=rescale(c(0,1, 2, 3)),
                         guide="colorbar", na.value="transparent") +
    scale_x_discrete(breaks=seq(1,max_name_length,1), limits=seq(1,max_name_length,1)) +
    scale_y_discrete(breaks=seq(1,max_name_length,1), limits=seq(1,max_name_length,1))
}



shinyServer(function(input, output, session) {
  output$JaroCMDPlot <- renderPlot({ newplot(input$jaro, input$cmd, input$prefix, input$prefrep)})
  output$JaroCMDPlot_2 <- renderPlot({ newplot_2(input$jaro, input$cmd, input$prefix, input$prefrep)})
  output$maintext <- renderUI({
    x <- input$cmd
    HTML("Blue blocks indicate correctly allowed Jaro-Winkler tests<br/>
         Yellow blocks indicate unnecessary JW tests<br/>
         Red indicate incorrectly filtered strings<br/>
         Beige are correctly filtered out names")
  })
})


