# Cleaning of the R environment  
  rm(list=ls())

# Load packages:
  library(openxlsx)
  library(rstatix)
  library(ggplot2)
  library(ggpubr)
  library(lme4)
  library(emmeans)
  library(multcomp)
  library(car)
  library(MASS)
  library(nlme)

# Set your working directory to where your excel file is saved:
  rm(list=ls())
  setwd("C:/Users/annes/Documents/R_Statistique/Statistique_méthode_inoculation/Excel")

# Import the data
  df <- within(read.xlsx(xlsxFile="data.xlsx"),{
    ID = as.factor(ID)
    Ferti = as.factor(Ferti)
    Inoc = as.factor(Inoc)
    Rep = as.factor(Rep)
    DPI = as.factor(DPI)
    Prop_infection = as.numeric(Prop_infection)
  })
  str(df)
  
  length(levels(df$ID))
  
#########################
### % necrotic leaves ###
#########################  
  # Keep the inoculation methods Scalpel and Syringe inoculated with Cm and times at 14 dpi and 21 dpi
  
  mydata1 = df[df$Inoc %in% c("Scalpel", "Seringue") & df$DPI %in% c("14","21"),]
  mydata1$Inoc = droplevels(mydata1$Inoc)
  mydata1$DPI = droplevels(mydata1$DPI)
  mydata1$ID = droplevels(mydata1$ID)
  length(levels(mydata1$ID))
  
  # GLME Model
  glmer1 <- glmer(cbind(Nb_feuilles_necrose+0.5, Nb_feuilles-Nb_feuilles_necrose+0.5) ~ 
                Inoc*Ferti*DPI + (1|Rep/ID), family=binomial, data=mydata1,
                control = glmerControl(calc.derivs=F))
  
  joint_tests(glmer1)
  summary(glmer1)
 
  # Tukey's multiple comparisons
  a1 = emmeans(glmer1, ~ Inoc*DPI, type="response"); a1
  pairs(a1)
  cld(a1, Letters=letters)
      
  a2=emmeans(glmer1, ~ DPI*Ferti, type="response"); a2
  pairs(a2)
  cld(a2, Letters=letters)
    
  # Analyse the overdispersion of the glmer model
  overdisp_fun <- function(model) {
      rdf <- df.residual(model)
      rp <- residuals(model,type="pearson")
      Pearson.chisq <- sum(rp^2)
      prat <- Pearson.chisq/rdf
      pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
      c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
    }
      
      overdisp_fun(glmer1)
      
##############
### Height ###
##############
  # Addition of a column for plants inoculated with Cm or with water (Negative control)
  levels(df$Inoc)
  df$CN = 0
  df$CN[df$Inoc %in% c("CNScalpel","CNSeringue")]=1
  
  df$Inoc.new=NA
  df$Inoc.new[df$Inoc %in% c("CNScalpel","Scalpel")]="Scapel"
  df$Inoc.new[df$Inoc %in% c("CNSeringue","Seringue")]="Seringue"
  
  with(df, table(Inoc.new, CN))
  df$CN = as.factor(df$CN)
  df$Hauteur = as.numeric(df$Hauteur)
  df$Inoc.new = as.factor(df$Inoc.new)
  df$time = as.numeric(as.character(df$DPI))
  df$time2 = df$time/7
  df$no_obs = 1:nrow(df)
  
  str(df)
  unique(df$time2)
  
  # Remove an outlier  
  df2 = df[df$no_obs != 276, ]

  # Investigation of a transformation
    librar
    boxcox = boxcox(Hauteur ~ CN*Inoc.new*Ferti*DPI , data=df2,
                      lambda = seq(-2, 2, length = 1000))
  # Transforming the height measures by the square root, in order to satisfy the normality assumption
    
  # Repeated mesured ANOVA
    fit.AR1 = lme(sqrt(Hauteur) ~ CN*Inoc.new*Ferti*DPI, random = ~ 1 | Rep / ID, 
            correlation = corCAR1(form=~ time | Rep/ID), data=df2)
    fit.CS = lme(sqrt(Hauteur) ~ CN*Inoc.new*Ferti*DPI, random = ~ 1 | Rep / ID, 
                  correlation = corCompSymm(form=~ time | Rep/ID), data=df2)
    fit.UN = lme(sqrt(Hauteur) ~ CN*Inoc.new*Ferti*DPI, random = ~ 1 | Rep / ID, 
                 correlation = corSymm(form=~ time2 | Rep/ID), data=df2)

    AIC(fit.AR1, fit.CS, fit.UN)
    #The AR(1) correlation structure fit the data best
    
    joint_tests(fit.AR1)
    

  # Tukey's multiple comparisons
    a1 = emmeans(fit.AR1, ~ CN*Inoc.new | DPI , type="response"); a1
    cld(a1, Letters=letters)
    
    a2 = emmeans(fit.AR1, ~ CN*Inoc.new*DPI , type="response"); a2
    cld(a2, Letters=letters)
    
    a3 = emmeans(fit.AR1, ~ CN*Ferti | DPI , type="response"); a3
    cld(a3, Letters=letters)
    
    a4 = emmeans(fit.AR1, ~ Ferti | CN*DPI , type="response"); a4
    cld(a4, Letters=letters)
    
    a5 = emmeans(fit.AR1, ~ Ferti*CN*DPI , type="response"); a5
    cld(a5, Letters=letters)
    


  # Normality
    r = residuals(fit.AR1,type="normalized", level=0)
    hist(r,freq=F)
    xfit<-seq(min(r),max(r),length=40)
    yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
    lines(xfit, yfit,col="red",lwd=2) 
    shapiro.test(r) 
    e1071::kurtosis(r)  


  # Equality of variances
    plot(fitted(fit.AR1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")
    abline(h=0, lty=2)

