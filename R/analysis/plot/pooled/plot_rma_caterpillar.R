plot_rma_caterpillar <- function(
    res = tar_read(pooled_retention),
    yi_var_str = "retent.asin",
    yi_var_lab = "Retention (arcsine scale)",
    xscale_trans_fun = metafor::transf.arcsin,
    xlab_trans_fun = metafor::transf.iarcsin
) {
  function(show_y = TRUE){
    forest.rma(res,
               xlab = yi_var_lab,
               xlim = c(-0,1) %>% xscale_trans_fun() %>% 
                 {c(.[1]-0.1,.[2]+0.1)},        ### adjust horizontal plot region limits
               ylim = c(-10,res$k+res$k*0.1),
               alim = c(0,1) %>% xscale_trans_fun(),
               atransf = xlab_trans_fun,
               order="obs",             ### order by size of yi
               slab=NA, annotate=FALSE, ### remove study labels and annotations
               efac=0,                  ### remove vertical bars at end of CIs
               pch=19,                  ### changing point symbol to filled circle
               col="gray40",            ### change color of points/CIs
               psize=2,                 ### increase point size
               cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
               refline = res$b[1],
               lty=c("solid","blank"))  ### remove horizontal line at top of plot
    
    ### draw points one more time to make them easier to see
    points(sort(res$data[[yi_var_str]]), length(res$data[[yi_var_str]]):1, pch=19, cex=0.5,col="grey")
    
    ### add summary polygon at bottom and text
    addpoly(res, mlab="", cex=3)
    
    if(show_y){
      text(
        res$b[1], res$k, 
        label = paste0("γ =",round(xlab_trans_fun(res$b[1]),2)), 
        pos=4, offset=0.5, cex=1
      )
    }
  }
}
