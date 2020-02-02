
library(circlize)

input = as.data.frame(read.delim("/Users/Nguyenphuong/Documents/GitHub/Visualization/Correlation_input.txt", 
                                 header=TRUE, sep="\t" ,na.strings = "", row.names = 1))

fac1 = factor(input$From)
fac2 = factor(input$To)
df = data.frame(from = fac1,
                to = fac2,
                value = as.vector(input$r_value),
                stringsAsFactors = FALSE)
df

color_range = c(-0.9, 0, 0.9)
col_fun = colorRamp2(color_range, c("blue","white","gold1"), transparency = 0.2)

lty_df    = data.frame(c("Bacteria1", "Bacteria4", "Bacteria3", "Bacteria1.", "Bacteria2."), c("disease1", "disease1","disease2", "disease2", "disease2"), c(2,2,2,2,2))
lwd_df    = data.frame(c("Bacteria1", "Bacteria4", "Bacteria3", "Bacteria1.", "Bacteria2."), c("disease1", "disease1","disease2", "disease2", "disease2"), c(2,2,2,2,2))
border_df = data.frame(c("Bacteria1", "Bacteria4", "Bacteria3", "Bacteria1.", "Bacteria2."), c("disease1", "disease1","disease2", "disease2", "disease2"), rep(1,5))




grid.col = c(Bacteria1 = "#FF6A6A", 
             Bacteria2 = "#FF6A6A", 
             Bacteria3 = "#FF6A6A", 
             Bacteria4 = "#FF6A6A",
             
             Bacteria1.= "#ADD8E6", 
             Bacteria2.= "#ADD8E6", 
             Bacteria3.= "#ADD8E6", 
             Bacteria4.= "#ADD8E6",
             
             disease1  = "#BEBEBE",
             disease2  = "#2F4F4F"
)

circos.par(start.degree = 90, clock.wise = FALSE)

chordDiagram(df, grid.col = grid.col, col = col_fun,
             link.sort = TRUE, link.decreasing = TRUE, 
             symmetric = TRUE,
             directional = 1, direction.type = c("diffHeight","arrows"),
             link.arr.type = "big.arrow",
             link.lty = lty_df, link.lwd = lwd_df,
             link.border = border_df,
             annotationTrack = c("grid"),
             annotationTrackHeight = c(0.05, 0.02),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df)))))
             
)

  circos.track(track.index = 1, panel.fun = function(x, y) {
  xlim  = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim  = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5), col = "black")
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                niceFacing = TRUE, adj = c(0.5, 0), col= "black")
  }
  }, bg.border = NA)

  circos.clear()


library(ComplexHeatmap)

lgd_points = Legend(at = c("Treatment", "Control", "disease1", "disease2" ), type = "grid",
                    legend_gp = gpar(fill = c("#FF6A6A", "#ADD8E6", "#BEBEBE", "#2F4F4F"), title_position = "topleft", 
                                     title = "Main Groups"))
# discrete
lgd_lines = Legend(at = c("FDR < 5%"), type = "lines",
                   legend_gp = gpar(lty =c(3), lwd = 2), title_position = "topleft"
)
# continuous
lgd_links = Legend(at = c(-0.9, 0, 0.9), col_fun = col_fun,
                   title_position = "topleft", title = "Spearman's Correlation")


lgd_list_vertical = packLegend(lgd_points, lgd_links, lgd_lines)
lgd_list_vertical
draw(lgd_list_vertical, x = unit(3.5, "mm"), 
     y = unit(3.5, "mm"), just = c( "left", "bottom"))
