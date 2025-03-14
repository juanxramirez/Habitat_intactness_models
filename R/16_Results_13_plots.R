# Install required packages
if(!requireNamespace("ggplot2", quietly=TRUE))
  install.packages("ggplot2", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggpubr", quietly=TRUE))
  install.packages("ggpubr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("cowplot", quietly=TRUE))
  install.packages("cowplot", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggcorrplot", quietly=TRUE))
  install.packages("ggcorrplot", quiet=TRUE, dependencies=TRUE)

# Load required packages
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggcorrplot)

# Set working directory
setwd("My_working_directory")

# Import data
pmcmhm.effectsize.generalists<-read.csv("effectsize_pmcmhm_generalists.csv", sep="\t")
pmcmhm.effectsize.specialists<-read.csv("effectsize_pmcmhm_specialists.csv", sep="\t")
rpmrcmrhm.effectsize.generalists<-read.csv("effectsize_rpmrcmrhm_generalists.csv", sep="\t")
rpmrcmrhm.effectsize.specialists<-read.csv("effectsize_rpmrcmrhm_specialists.csv", sep="\t")
pp.pmcmhm.generalists<-read.csv("predicted_probability_pmcmhm_generalists.csv", sep="\t")
pp.pmcmhm.specialists<-read.csv("predicted_probability_pmcmhm_specialists.csv", sep="\t")
cmhm.effectsize.generalists<-read.csv("effectsize_cmhm_generalists.csv", sep="\t")
cmhm.effectsize.specialists<-read.csv("effectsize_cmhm_specialists.csv", sep="\t")
pmcmhm.corr.generalists<-read.csv("corr_generalists.csv", sep="\t")
pmcmhm.corr.specialists<-read.csv("corr_specialists.csv", sep="\t")
pmcmhm.effectsize.generalists.threatened.criterionB.excluded<-read.csv("effectsize_pmcmhm_generalists_threatened_criterionB_excluded.csv", sep="\t")
pmcmhm.effectsize.specialists.threatened.criterionB.excluded<-read.csv("effectsize_pmcmhm_specialists_threatened_criterionB_excluded.csv", sep="\t")
rpmrcmrhm.effectsize.generalists.threatened.criterionB.excluded<-read.csv("effectsize_rpmrcmrhm_generalists_threatened_criterionB_excluded.csv", sep="\t")
rpmrcmrhm.effectsize.specialists.threatened.criterionB.excluded<-read.csv("effectsize_rpmrcmrhm_specialists_threatened_criterionB_excluded.csv", sep="\t")
pmcmhm.auc.6.fold.cv.by.region.generalists<-read.csv("auc_spatially_blocked_cv_pmcmhm_generalists.csv", sep="\t")
pmcmhm.auc.6.fold.cv.by.region.specialists<-read.csv("auc_spatially_blocked_cv_pmcmhm_specialists.csv", sep="\t")
pmcmhm.auc.10.fold.random.cv.generalists<-read.csv("auc_10-fold_cv_pmcmhm_generalists.csv", sep="\t")
pmcmhm.auc.10.fold.random.cv.specialists<-read.csv("auc_10-fold_cv_pmcmhm_specialists.csv", sep="\t")
pmcmhm.auc.6.fold.cv.by.region.generalists.threatened.criterionB.excluded<-read.csv("auc_spatially_blocked_cv_pmcmhm_generalists_threatened_criterionB_excluded.csv", sep="\t")
pmcmhm.auc.6.fold.cv.by.region.specialists.threatened.criterionB.excluded<-read.csv("auc_spatially_blocked_cv_pmcmhm_specialists_threatened_criterionB_excluded.csv", sep="\t")
pmcmhm.auc.10.fold.random.cv.generalists.threatened.criterionB.excluded<-read.csv("auc_10-fold_cv_pmcmhm_generalists_threatened_criterionB_excluded.csv", sep="\t")
pmcmhm.auc.10.fold.random.cv.specialists.threatened.criterionB.excluded<-read.csv("auc_10-fold_cv_pmcmhm_specialists_threatened_criterionB_excluded.csv", sep="\t")

#----
# Effect sizes
pmcmhm.effectsize.generalists$habitat_model<-factor(pmcmhm.effectsize.generalists$habitat_model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
pmcmhm.effectsize.generalists.plot<-ggplot(pmcmhm.effectsize.generalists, aes(x=habitat_model, y=std_coefficient, colour=habitat_model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=habitat_model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="none", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) + 
  scale_shape_manual(values=c(16, 16, 16), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) + 
  scale_x_discrete(name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("PMM", "CM", "HM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", linewidth=0.5) +
  ylim(-1.11, 0) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pmcmhm.effectsize.generalists.plot

pmcmhm.effectsize.specialists$habitat_model<-factor(pmcmhm.effectsize.specialists$habitat_model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
pmcmhm.effectsize.specialists.plot<-ggplot(pmcmhm.effectsize.specialists, aes(x=habitat_model, y=std_coefficient, colour=habitat_model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=habitat_model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="none", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16, 16, 16), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_x_discrete(name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("PMM", "CM", "HM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) +
  ylim(-1.11, 0) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pmcmhm.effectsize.specialists.plot

rpmrcmrhm.effectsize.generalists$habitat_model<-factor(rpmrcmrhm.effectsize.generalists$habitat_model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
rpmrcmrhm.effectsize.generalists.plot<-ggplot(rpmrcmrhm.effectsize.generalists, aes(x=habitat_model, y=std_coefficient, colour=habitat_model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=habitat_model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="none", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) + 
  scale_shape_manual(values=c(16, 16, 16), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) + 
  scale_x_discrete(name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("RPMM", "RCM", "RHM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", linewidth=0.5) +
  ylim(-0.25, 0.33) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rpmrcmrhm.effectsize.generalists.plot

rpmrcmrhm.effectsize.specialists$habitat_model<-factor(rpmrcmrhm.effectsize.specialists$habitat_model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
rpmrcmrhm.effectsize.specialists.plot<-ggplot(rpmrcmrhm.effectsize.specialists, aes(x=habitat_model, y=std_coefficient, colour=habitat_model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=habitat_model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="none", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16, 16, 16), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_x_discrete(name="Habitat model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("RPMM", "RCM", "RHM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) +
  ylim(-0.25, 0.33) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rpmrcmrhm.effectsize.specialists.plot

# Combine plots
pmcmhm.effectsize.plot<-plot_grid(pmcmhm.effectsize.generalists.plot + theme(legend.position="none"),
                                  pmcmhm.effectsize.specialists.plot + theme(legend.position="none"),
                                  rpmrcmrhm.effectsize.generalists.plot + theme(legend.position="none"),
                                  rpmrcmrhm.effectsize.specialists.plot + theme(legend.position="none"),
                                  labels=c('(a)', '(b)', '(c)', '(d)'), 
                                  label_size = 7, 
                                  ncol=2, 
                                  nrow=2,
                                  align ="hv",
                                  axis="tblr", 
                                  rel_heights=c(2,2))
pmcmhm.effectsize.plot

# Extract legend
# legend.fig.2 <- get_legend(
#   pmcmhm.effectsize.generalists.plot + 
#     theme(legend.position = "bottom")
# )

# Export Fig.2
#pdf("Fig.2.pdf", width=3, height=5)
png("Fig.2.png", units="in", width=3, height=5, res=1200, type="cairo")
plot_grid(pmcmhm.effectsize.plot, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Predicted probabilities
pp.pmcmhm.generalists$predictor <- factor(pp.pmcmhm.generalists$predictor , levels=c("pm", "cm", "hm"))
pp.pmcmhm.generalists.plot<-ggplot(pp.pmcmhm.generalists, aes(x=x_bs, y=predicted_bt, fill=predictor, color=predictor)) +
  geom_line(size=1) +
  xlab("") + 
  ylab("Prob. of being threatened") +
  theme(legend.position="none", legend.title=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        legend.margin=margin(-11, 1, 1, 1),
        legend.spacing.x=unit(1, "mm"),
        legend.background = element_rect(color = NA),
        panel.border=element_rect(colour="black", fill=NA, linewidth=0.5),
        legend.text=element_text(size=7),
        panel.background=element_rect(fill="transparent", colour="grey85", size=0.5, linetype="solid"),
        panel.grid.major=element_line(linewidth=0.25, linetype='dashed', colour="grey85"),
        panel.grid.minor=element_line(linewidth=0.25, linetype='dashed', colour="grey85"), 
        plot.title=element_text(color="black", size=7, hjust=0), axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=7), axis.text.x=element_text(size=7), 
        axis.text.y=element_text(size=7), legend.key=element_rect(fill="transparent", colour="transparent"),
        strip.background = element_rect(fill="transparent", size=0.5, color="transparent"), strip.text.x = element_text(size=7), strip.text.y = element_text(size=7),
        strip.placement = "outside") +
  geom_ribbon(aes(ymin = conf.low_bt, ymax = conf.high_bt), alpha=0.3, colour=NA) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_fill_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  xlim(-0.01, 1.01) +
  ylim(0.5, 0.69) +
  guides(fill=guide_legend(title="Habitat model", title.position = "top"), color=guide_legend(title="Habitat model", title.position = "top")) +
  facet_grid(habitat_breadth~predictor, switch="x", labeller = labeller(predictor = c("pm" = "Patch-matrix",
                                                                                      "cm" = "Continuum",
                                                                                      "hm" = "Hybrid")), scales="free_x")
pp.pmcmhm.generalists.plot

pp.pmcmhm.specialists$predictor <- factor(pp.pmcmhm.specialists$predictor , levels=c("pm", "cm", "hm"))
pp.pmcmhm.specialists.plot<-ggplot(pp.pmcmhm.specialists, aes(x=x_bs, y=predicted_bt, fill=predictor, color=predictor)) +
  geom_line(size=1) +
  xlab("") + 
  ylab("Prob. of being threatened") +
  theme(legend.position="none", legend.title=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        legend.margin=margin(-11, 1, 1, 1),
        legend.spacing.x=unit(1, "mm"),
        legend.background = element_rect(color = NA),
        panel.border=element_rect(colour="black", fill=NA, linewidth=0.5),
        legend.text=element_text(size=7),
        panel.background=element_rect(fill="transparent", colour="grey85", size=0.5, linetype="solid"),
        panel.grid.major=element_line(linewidth=0.25, linetype='dashed', colour="grey85"),
        panel.grid.minor=element_line(linewidth=0.25, linetype='dashed', colour="grey85"), 
        plot.title=element_text(color="black", size=7, hjust=0), axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=7), axis.text.x=element_text(size=7), 
        axis.text.y=element_text(size=7), legend.key=element_rect(fill="transparent", colour="transparent"),
        strip.background = element_rect(fill="transparent", size=0.5, color="transparent"), strip.text.x = element_text(size=7), strip.text.y = element_text(size=7),
        strip.placement = "outside") +
  geom_ribbon(aes(ymin = conf.low_bt, ymax = conf.high_bt), alpha=0.3, colour=NA) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_fill_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  guides(fill=guide_legend(title="Habitat model", title.position = "top"), color=guide_legend(title="Habitat model", title.position = "top")) +
  xlim(-0.01, 1.01) +
  ylim(0.5, 0.69) +
  facet_grid(habitat_breadth~predictor, switch="x", labeller = labeller(predictor = c("pm" = "Patch-matrix",
                                                                                      "cm" = "Continuum",
                                                                                      "hm" = "Hybrid")), scales="free_x")
pp.pmcmhm.specialists.plot

# Combine plots
pp.pmcmhm.plot<-plot_grid(pp.pmcmhm.generalists.plot + theme(legend.position="none"),
                          pp.pmcmhm.specialists.plot + theme(legend.position="none"),
                          labels=c('(a)', '(b)'), 
                          label_size = 7, 
                          ncol=1, 
                          nrow=2,
                          align ="hv",
                          axis="tblr", 
                          rel_heights=c(2,2))
pp.pmcmhm.plot

# Extract legend 
# legend.fig.3 <- get_legend(
#   pp.pmcmhm.generalists.plot + 
#     theme(legend.position = "bottom")
# )

# Export Fig.3 
#pdf("Fig.3.pdf", width=4.3, height=4.5)
png("Fig.3.png", units="in", width=4.3, height=4.5, res=1200, type="cairo")
plot_grid(pp.pmcmhm.plot, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Effect sizes for continuum and hybrid models
cmhm.effectsize.table<-rbind(cmhm.effectsize.generalists, cmhm.effectsize.specialists)
cmhm.effectsize.table$parameter <- factor(cmhm.effectsize.table$parameter , levels=c("5th", "10th", "50th", "90th", "95th"))
cmhm.effectsize.table$group <- factor(cmhm.effectsize.table$group , levels=c("c", "h"))
cmhm.effectsize.plot<-ggplot(cmhm.effectsize.table, aes(x=parameter, y=std_coefficient, colour=habitat_model)) +
  coord_flip() +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5) +
  labs(y="Standardized coefficient", x="Percentile") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="none", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7),
        strip.text.y = element_text(size=7)) +
  scale_shape_manual(values=c(15, 16, 0)) + 
  scale_color_manual(values=c("#80B1D3", "#CC79A7")) + 
  geom_hline(yintercept = 0, linetype="dashed", color = "#8DD3C7", size=0.5) +
  guides(fill=guide_legend(title="Habitat model", title.position = "top"), color=guide_legend(title="Habitat model", title.position = "top")) +
  facet_grid(habitat_breadth~group, labeller = labeller(group = c("c" = "Continuum", "h"= "Hybrid")))
cmhm.effectsize.plot

# Reduce plot margins
cmhm.effectsize.plot.fv<-cmhm.effectsize.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S1
png("Fig.S1.png", units="in", width=6.5, height=4, res=1200, type="cairo")
cmhm.effectsize.plot.fv
dev.off()

#----
# Correlation between variables
rownames(pmcmhm.corr.generalists)<-c("PMM", "RPMM", "CM", "RCM", "HM", "RHM", "RS", "GL", "WA")
rownames(pmcmhm.corr.specialists)<-c("PMM", "RPMM", "CM", "RCM", "HM", "RHM", "RS", "GL", "WA")

# Visualize correlation  matrix
corr.generalists.plot<-ggcorrplot(pmcmhm.corr.generalists, 
                                  type = "lower",
                                  outline.color = "white",
                                  ggtheme = ggplot2::theme_gray,
                                  colors = c("#6D9EC1", "white", "#E46726"),
                                  lab=TRUE, 
                                  lab_size=2,
                                  legend.title = "r") +
  labs(title="Generalists") +
  theme(legend.key.width=unit(0.2, "cm"),
        legend.key.height=unit(1.25, "cm"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7),
        legend.justification = "top",
        axis.text.x=element_text(size=7, angle=0, hjust=0.5),
        axis.text.y=element_text(size=7),
        plot.margin = unit(c(0,1,0,1), "lines"),
        plot.title=element_text(size=7, hjust=0.5))
corr.generalists.plot

corr.specialists.plot<-ggcorrplot(pmcmhm.corr.specialists, 
                                  type = "lower",
                                  outline.color = "white",
                                  ggtheme = ggplot2::theme_gray,
                                  colors = c("#6D9EC1", "white", "#E46726"),
                                  lab=TRUE, 
                                  lab_size=2,
                                  legend.title = "r") +
  labs(title="Specialists") +
  theme(legend.key.width=unit(0.2, "cm"),
        legend.key.height=unit(1.25, "cm"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7),
        legend.justification = "top",
        axis.text.x=element_text(size=7, angle=0, hjust=0.5),
        axis.text.y=element_text(size=7),
        plot.margin = unit(c(0,1,0,1), "lines"),
        plot.title=element_text(size=7, hjust=0.5))
corr.specialists.plot

# Correlation plot
corr.plot<- ggarrange(corr.generalists.plot, 
                      corr.specialists.plot,
                      ncol=2, 
                      nrow=1, 
                      legend="right", 
                      align="hv", 
                      common.legend=TRUE,
                      font.label=list(size=7))

# Export Fig.S2
png("Fig.S2.png", units="in", width=6.5, height=3, res=1200, type="cairo")
corr.plot
dev.off()

#----
# Effect sizes - criterion B excluded
pmcmhm.effectsize.table<-rbind(pmcmhm.effectsize.generalists, pmcmhm.effectsize.specialists)
pmcmhm.effectsize.criterionB.excluded.table<-rbind(pmcmhm.effectsize.generalists.threatened.criterionB.excluded, pmcmhm.effectsize.specialists.threatened.criterionB.excluded)
pmcmhm.effectsize.criterionB.excluded.table$criterionB<-"Excluded"
pmcmhm.effectsize.table$criterionB<-"Included"
pmcmhm.effectsize.criterionB.includedvsexcluded.table<-rbind(pmcmhm.effectsize.table, pmcmhm.effectsize.criterionB.excluded.table)
pmcmhm.effectsize.criterionB.includedvsexcluded.table$id<-letters[1:12]
rpmrcmrhm.effectsize.table<-rbind(rpmrcmrhm.effectsize.generalists, rpmrcmrhm.effectsize.specialists)
rpmrcmrhm.effectsize.criterionB.excluded.table<-rbind(rpmrcmrhm.effectsize.generalists.threatened.criterionB.excluded, rpmrcmrhm.effectsize.specialists.threatened.criterionB.excluded)
rpmrcmrhm.effectsize.criterionB.excluded.table$criterionB<-"Excluded"
rpmrcmrhm.effectsize.table$criterionB<-"Included"
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table<-rbind(rpmrcmrhm.effectsize.table, rpmrcmrhm.effectsize.criterionB.excluded.table)
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$id<-letters[1:12]

# Plot effect sizes
pmcmhm.effectsize.criterionB.includedvsexcluded.table$criterionB<-factor(pmcmhm.effectsize.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pmcmhm.effectsize.criterionB.includedvsexcluded.table$parameter<-factor(pmcmhm.effectsize.criterionB.includedvsexcluded.table$parameter, levels=c("pm", "cm", "hm"))
pmcmhm.effectsize.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pmcmhm.effectsize.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pmcmhm.effectsize.criterionB.includedvsexcluded.plot<-ggplot(pmcmhm.effectsize.criterionB.includedvsexcluded.table, aes(x=parameter, y=std_coefficient, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, position=position_dodge(width=0.7)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(-8, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16)) +
  scale_color_manual(values=c("#FF61CC", "#8494FF", "#CC79A7")) + 
  scale_x_discrete(labels=c("PMM", "CM", "HM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pmcmhm.effectsize.criterionB.includedvsexcluded.plot

rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$criterionB<-factor(rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$parameter<-factor(rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$parameter, levels=c("rpm", "rcm", "rhm"))
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$habitat_breadth<-factor(rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.plot<-ggplot(rpmrcmrhm.effectsize.criterionB.includedvsexcluded.table, aes(x=parameter, y=std_coefficient, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, position=position_dodge(width=0.7)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(-8, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16)) +
  scale_color_manual(values=c("#FF61CC", "#8494FF", "#CC79A7")) + 
  scale_x_discrete(labels=c("RPMM", "RCM", "RHM")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rpmrcmrhm.effectsize.criterionB.includedvsexcluded.plot

# Combine plots
pmcmhm.effectsize.criterionB.includedvsexcluded.plot.fv<-plot_grid(pmcmhm.effectsize.criterionB.includedvsexcluded.plot + theme(legend.position="none"),
                                                                   rpmrcmrhm.effectsize.criterionB.includedvsexcluded.plot + theme(legend.position="none"),
                                                                   #labels=c('(a)', '(c)', '(b)', '(d)'), 
                                                                   label_size = 7, 
                                                                   ncol=1, 
                                                                   nrow=2,
                                                                   align ="hv",
                                                                   axis="tblr", 
                                                                   rel_heights=c(2,2))
pmcmhm.effectsize.criterionB.includedvsexcluded.plot.fv

# Extract legend
legend.fig.S3 <- get_legend(
  pmcmhm.effectsize.criterionB.includedvsexcluded.plot + 
    theme(legend.position = "bottom")
)

# Export Fig.S3
png("Fig.S3.png", units="in", width=3, height=6.3, res=1200, type="cairo")
plot_grid(pmcmhm.effectsize.criterionB.includedvsexcluded.plot.fv, legend.fig.S3, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Spatially blocked cv | 10-fold cv
pmcmhm.auc.6.fold.cv.by.region.table<-rbind(pmcmhm.auc.6.fold.cv.by.region.generalists, pmcmhm.auc.6.fold.cv.by.region.specialists)
pmcmhm.auc.6.fold.cv.by.region.table$cv<-"6-fold CV (by biogeographic realm)"
pmcmhm.auc.10.fold.random.cv.table<-rbind(pmcmhm.auc.10.fold.random.cv.generalists, pmcmhm.auc.10.fold.random.cv.specialists)
pmcmhm.auc.10.fold.random.cv.table$cv<-"10-fold CV"
pmcmhm.auc.cv.table<-rbind(pmcmhm.auc.6.fold.cv.by.region.table, pmcmhm.auc.10.fold.random.cv.table)

# Plot AUCs ~ Habitat models
pmcmhm.auc.cv.table$cv<-factor(pmcmhm.auc.cv.table$cv, levels=c("6-fold CV (by biogeographic realm)", "10-fold CV"))
pmcmhm.auc.cv.table$parameter<-factor(pmcmhm.auc.cv.table$parameter, levels=c("pm", "cm", "hm"))
pmcmhm.auc.cv.table$habitat_breadth<-factor(pmcmhm.auc.cv.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pmcmhm.auc.cv.plot<-ggplot(pmcmhm.auc.cv.table, aes(x=parameter, y=auc, color=cv)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=cv), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "vertical",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#A6D854", "#FFD92F")) + 
  scale_x_discrete(labels=c("ER ~ PMM + RPMM", "ER ~ CM + RCM", "ER ~ HM + RHM")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Cross-validation (CV) scheme", title.position = "top"), shape=guide_legend(title="Cross-validation (CV) scheme", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y")
pmcmhm.auc.cv.plot

# Reduce plot margins
pmcmhm.auc.cv.plot.fv<-pmcmhm.auc.cv.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S4
png("Fig.S4.png", units="in", width=3, height=4, res=1200, type="cairo")
pmcmhm.auc.cv.plot.fv
dev.off()

#----
# Spatially blocked cv - criterion B excluded
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table<-rbind(pmcmhm.auc.6.fold.cv.by.region.generalists.threatened.criterionB.excluded, pmcmhm.auc.6.fold.cv.by.region.specialists.threatened.criterionB.excluded)
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table$criterionB<-"Excluded"
pmcmhm.auc.6.fold.cv.by.region.table$criterionB<-"Included"
vars<-c("auc", "ci_lower", "ci_upper", "parameter", "habitat_model", "habitat_breadth", "criterionB")
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table<-rbind(pmcmhm.auc.6.fold.cv.by.region.table[,vars], pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table[,vars])

# Plot AUCs ~ Habitat models
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$cv<-factor(pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$parameter<-factor(pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$parameter, levels=c("pm", "cm", "hm"))
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot<-ggplot(pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table, aes(x=parameter, y=auc, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=criterionB), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#FF61CC", "#8494FF")) + 
  scale_x_discrete(labels=c("ER ~ PMM + RPMM", "ER ~ CM + RCM", "ER ~ HM + RHM")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top"), shape=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot

# Reduce plot margins
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot.fv<-pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S5
png("Fig.S5.png", units="in", width=3, height=4, res=1200, type="cairo")
pmcmhm.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot.fv
dev.off()

#----
# 10-fold cv - criterion B excluded
pmcmhm.auc.10.fold.random.cv.threatened.criterionB.excluded.table<-rbind(pmcmhm.auc.10.fold.random.cv.generalists.threatened.criterionB.excluded, pmcmhm.auc.10.fold.random.cv.specialists.threatened.criterionB.excluded)
pmcmhm.auc.10.fold.random.cv.threatened.criterionB.excluded.table$criterionB<-"Excluded"
pmcmhm.auc.10.fold.random.cv.table$criterionB<-"Included"
vars<-c("auc", "ci_lower", "ci_upper", "parameter", "habitat_model", "habitat_breadth", "criterionB")
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table<-rbind(pmcmhm.auc.10.fold.random.cv.table[,vars], pmcmhm.auc.10.fold.random.cv.threatened.criterionB.excluded.table[,vars])

# Plot AUCs ~ habitat models
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$cv<-factor(pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$parameter<-factor(pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$parameter, levels=c("pm", "cm", "hm"))
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot<-ggplot(pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table, aes(x=parameter, y=auc, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=criterionB), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#FF61CC", "#8494FF")) + 
  scale_x_discrete(labels=c("ER ~ PMM + RPMM", "ER ~ CM + RCM", "ER ~ HM + RHM")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top"), shape=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y") 
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot

# Reduce plot margins
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot.fv<-pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S6
png("Fig.S6.png", units="in", width=3, height=4, res=1200, type="cairo")
pmcmhm.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot.fv
dev.off()

