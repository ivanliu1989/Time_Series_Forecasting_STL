# install.packages("devtools")
# library("devtools")
# install_github("ropensci/plotly")
# devtools::install_github("ropensci/plotly")

library(plotly)
set_credentials_file("ivan.liu", "7lne8mnymc")

py <- plotly()
plot_list <- list()
for(i in 1:27){
    plot_list[[i]] <- list(
        x = c(0:24),
        y = c(as.data.frame(irf_fit$irf[22])[,i]),
        name = names(finFeatList[i]),
        line = list(width = 2), 
        type = "bar"
    )
}
layout <- list(
    title = paste0(finFeatList[22], ' - Impulse Response Function'), 
    font = list(size = 12), 
    showlegend = TRUE, 
    autosize = TRUE, 
    width = 1368, 
    height = 745, 
    barmode = "stack",
    xaxis = list(
        title = "Month (s)", 
        range = c(-1, 24), 
        type = "linear", 
        autorange = TRUE
    ), 
    yaxis = list(
        title = "Unit Change", 
        range = c(-1, 1), 
        type = "linear", 
        autorange = TRUE
    )
)
response <- py$plotly(plot_list, kwargs=list(layout=layout))
response$url
### Plot ### 
irf_fit