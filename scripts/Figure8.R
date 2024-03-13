

## Figure 8

SSP3_GRO <- readRDS("outputs/SSP3_ent/GRO/GRO_bilateral_accum_net_mig.rdata")
SSP5_GRO <- readRDS("outputs/SSP5_ent/GRO/GRO_bilateral_accum_net_mig.rdata")
SSP3_BCS <- readRDS("outputs/SSP3_ent/BCS/BCS_bilateral_accum_net_mig.rdata")
SSP5_BCS <- readRDS("outputs/SSP5_ent/BCS/BCS_bilateral_accum_net_mig.rdata")

plot_grid(SSP3_BCS + theme(plot.margin = unit(c(0,0,0,0),"cm")), 
          SSP5_BCS + theme(plot.margin = unit(c(0,0,0,0),"cm")), 
          SSP3_GRO + theme(plot.margin = unit(c(0,0,0,0),"cm")), 
          SSP5_GRO + theme(plot.margin = unit(c(0,0,0,0),"cm")),
          ncol = 2, nrow = 2, rel_widths = c(1,1), rel_heights = c(1,1))
