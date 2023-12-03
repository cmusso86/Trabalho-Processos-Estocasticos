# Análise de sobrevivência
# Curvas de Kaplan Meyer


# Bibliotecas
library(summarytools)
library(dplyr)
library(ggplot2)
library(survival)
library(flexsurv)
library(survminer)

# Objeto Survival
surv_object <- Surv(time = data$time, event = data$survival, type="right")


# Curva geral
fit_1 <- survfit(surv_object ~ 1,
                 data = data)

plot(fit_1)
fit_1

ggsurvplot(fit_1, data = data)

# Curva de acordo com o tratamento
fit_geral <- survfit(surv_object ~ treatcanmat, data = data)
summary(fit_geral)

ggsurvplot(fit_geral, data = data,
           pval = TRUE,
           pval.size = 4,
           pval.coord = c(800, 1),
           ggtheme = theme_bw(),
           legend = "bottom",
           legend.title = "Tratamentos",
           xlab = "Time (Years)",
           ylab = "Readmission probability",
           palette = c("#E7B800", "#2E9FDF", "#A52A2A"),
           legend.labs = c("No treatment", "Other treatment", "1st and 2nd line")
)
