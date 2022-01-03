library(survival)
library(survminer)
library(dplyr)
library("survminer")
library(ggthemes)


surv <- read.table("./patients.csv",
                   sep = ",",
                   dec = ",",
                   header = T)

attach(surv)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = surv$survtime, event = surv$survstat)
surv_object

fit1 <- survfit(surv_object~1, data = surv) #survstat ist hier falsch
summary(fit1)


ggsurvplot(fit1, data = surv, pval = TRUE, color = "red",
           xscale = "d_y", break.x.by = 182.6,
           conf.int = FALSE, surv.scale = "percent", 
           xlab = "Years", ylab = "Survival",
           axes.offset = FALSE, risk.table = TRUE)


fit2 <- survfit(surv_object~Präop.Status, data = surv) #survstat ist hier falsch
summary(fit2)

ggsurvplot(fit2, data = surv, risk.table = TRUE, legend.title="",
           ylab="Survival")


elektiv <- subset(surv, Präop.Status=='Elektiv')
elektiv

surv_elektiv <- Surv(time = elektiv$survtime, event = elektiv$survstat)
surv_elektiv


fit3 <- survfit(surv_elektiv~1, data = elektiv) #survstat ist hier falsch
summary(fit3)
fitt <- list(All_Patients = fit1, Elective_Patients = fit3)
ggsurvplot_combine(fitt, data=surv,
                          ylab="Survival",
                          xscale = "d_y", break.x.by = 182.6,
                          xlab="Years",
                          #pval = TRUE, #conf.int = TRUE, 
                          combine = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          legend.title="", # to remove the word strata
                          axes.offset = FALSE,
                          ggtheme = theme_bw() + 
                            theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())
                          # Change ggplot2 theme
                          
                          #palette = c("#E7B800", "#2E9FDF",)
)



fit <- list(Patients = fit1, Status = fit2)
# Change color, linetype by strata, risk.table color by strata
res <- ggsurvplot_combine(fit, data=surv,
                          ylab="Survival",
                          xscale = "d_y", break.x.by = 182.6,
                          xlab="Years",
                          #pval = TRUE, #conf.int = TRUE, 
                          combine = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          legend.title="", # to remove the word strata
                          axes.offset = FALSE,
                          ggtheme = theme_bw() + 
                            theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())
                          # Change ggplot2 theme
                          
                          #palette = c("#E7B800", "#2E9FDF",)
)
res


print(res)
