``` R 
# handling
* filter(!is.na() & cat_var %in% c(v1,v2,v3) | )
* select( , ) # select(-var1, -var2)
* arrange( desc() ) 
* mutate( , )
* summarise( qt = quantile(val, p, na.rm = T), iqr = IQR(  ), n = n())
* group_by( , )
* left_join(df1, df2, id = )

# shaping
* 

# visual - (ggplot)
* ggplot(data, aes(x = reorder(var_main, var_order), y, fill = ))
* geom_bar( ) # count(default)
* geom_col( position = "dodge" ) 
* geom_histogram( alpha = 0.5 )
* geom_point( )
* geom_line( )
* geom_boxplot( )

* xlim(0, 100); ylim(0, 100)
* coord_flip( )
* scale_x_discrete(limits = order)

* library(gridExtra)
* grid.arrange(p1, p2, ncol = 2)
# --------------------------------------

# crawling


# map
* ggChoropleth(data = value_set, aes(fill = feature, 
                                     map_id = region_category, ...), 
               map = geom_set, interactive = T)
```
