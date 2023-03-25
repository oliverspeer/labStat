library(reshape2)
> 
  > # Reshape data to long format
  > dxi.sum.data.long <- melt(dxi.sum.data, id.vars = "Year_Month")
  > 
    > # Create separate plots for each variable
    > ggplot(dxi.sum.data.long, aes(x = Year_Month, y = value, group = variable)) +
    +     geom_line() +
    +     facet_grid(cols = vars(variable), scales = "free_y") +
    +     labs(x = "Year Month") +
    +     theme(legend.position = "none")



# +
#   facet_wrap(~Year_Month) 
  