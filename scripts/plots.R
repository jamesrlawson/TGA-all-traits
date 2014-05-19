########### LETS OUTPUT SOME GRAPHS - stuff, along hydro gradients #############

library(ggplot2)

# plot.linear requires: df, var, trait, labels. 
# var is alphaT/betaT/ts/Rs, etc.



plot.linear(D_Sna_hydro_maxheight, D_Sna_hydro_maxheight$ts, maxheight)
plot.linear(D_Sna_hydro_seedmass, D_Sna_hydro_seedmass$ts, seedmass)
plot.linear(D_Sna_hydro_SLA, D_Sna_hydro_SLA$ts, SLA)
plot.linear(D_Sna_hydro_WD, D_Sna_hydro_WD$ts, WD)

plot.quad(D_Sna_hydro_maxheight, D_Sna_hydro_maxheight$ts, maxheight)
plot.quad(D_Sna_hydro_seedmass, D_Sna_hydro_seedmass$ts, seedmass)
plot.quad(D_Sna_hydro_SLA, D_Sna_hydro_SLA$ts, SLA)
plot.quad(D_Sna_hydro_WD, D_Sna_hydro_WD$ts, WD)

plot.linear(hydroplots, hydroplots$betaTrange_WD, WD)
plot.linear(hydroplots, hydroplots$betaTrange_SLA, SLA)
plot.linear(hydroplots, hydroplots$betaTrange_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$betaTrange_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$betaTrange_WD, WD)
plot.quad(hydroplots, hydroplots$betaTrange_SLA, SLA)
plot.quad(hydroplots, hydroplots$betaTrange_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$betaTrange_maxheight, maxheight)

plot.linear(hydroplots, hydroplots$alphaTrange_WD, WD)
plot.linear(hydroplots, hydroplots$alphaTrange_SLA, SLA)
plot.linear(hydroplots, hydroplots$alphaTrange_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$alphaTrange_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$alphaTrange_WD, WD)
plot.quad(hydroplots, hydroplots$alphaTrange_SLA, SLA)
plot.quad(hydroplots, hydroplots$alphaTrange_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$alphaTrange_maxheight, maxheight)

plot.linear(hydroplots, hydroplots$Tp_WD, WD)
plot.linear(hydroplots, hydroplots$Tp_SLA, SLA)
plot.linear(hydroplots, hydroplots$Tp_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$Tp_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$Tp_WD, WD)
plot.quad(hydroplots, hydroplots$Tp_SLA, SLA)
plot.quad(hydroplots, hydroplots$Tp_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$Tp_maxheight, maxheight)





## stuff to do:
# fix plot.linear and plot.quadratic to output to the right directories using sprintf
# have an option somewhere to only use data for which entries exist for all traits (or woody spp. only)
#   that way I can compare alphaT/betaT for traits directly, 
#   and can then compare alphaT/betaT with alphaTrange/betaT range relationships between traits
# have a think about whether range is the appropriate metric for dispersion 
#   (because we're comparing different traits with different units)
#
# something to do with null models...
#
# something seems fishy with the WD Tp values. Why are the results not the same as the WDmeans values?
# because there are more species, and the values used are species means... Still, it's a pretty big difference.s