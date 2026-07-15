
rm(list=ls())
setwd("E:\\AAM\\6. health\\new\\提交\\二轮返修")
#install.packages("readxl")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("agricolae")
library(agricolae)


df <- read.csv('Supplementary raw data.csv',header = T,stringsAsFactors = FALSE, encoding='UTF-8')
head(df)
df$type = as.factor(df$type)
df$type <- factor(df$type, levels = c('wasteland', 'woodland','upland','paddy'))
df$group <- factor(df$group, levels = c('low', 'high'))

library(ggplot2)
library(dplyr)
library(agricolae)
# Turkey test
plot_bar <- function(df, yvar, ylab){
 ##-----------------------------
  df$type <- factor(df$type,
                    levels=c("wasteland","woodland","upland","paddy"))
  df$group <- factor(df$group,
                     levels=c("low","high"))
  ## 计算均值和SE
  stat <- df %>%
    group_by(type,group) %>%
    summarise(
      mean = mean(.data[[yvar]],na.rm=TRUE),
      se = sd(.data[[yvar]],na.rm=TRUE)/sqrt(n()),
      .groups="drop"
    )
  ## low组 Tukey
  low <- subset(df,group=="low")
  fit.low <- aov(as.formula(paste(yvar,"~type")),data=low)
  tukey.low <- HSD.test(fit.low,"type",group=TRUE)
  letter.low <- data.frame(
    type = rownames(tukey.low$groups),
    letter = tukey.low$groups$groups,
    stringsAsFactors = FALSE
  )
  letter.low$type <- factor(
    letter.low$type,
    levels = c("wasteland","woodland","upland","paddy")
  )
  letter.low <- letter.low[order(letter.low$type), ]
  letter.low$group <- "low"
  ##-----------------------------
  ## high组 Tukey
  ##-----------------------------
  high <- subset(df,group=="high")
  fit.high <- aov(as.formula(paste(yvar,"~type")),data=high)
  tukey.high <- HSD.test(fit.high,"type",group=TRUE)
  letter.high <- data.frame(
    type = rownames(tukey.high$groups),
    letter = tukey.high$groups$groups,
    stringsAsFactors = FALSE
  )
  letter.high$type <- factor(
    letter.high$type,
    levels = c("wasteland","woodland","upland","paddy")
  )
  letter.high <- letter.high[order(letter.high$type), ]
  letter.high$group <- "high"
  letters <- rbind(letter.low,letter.high)
  stat <- left_join(stat,letters,
                    by=c("type","group"))
  stat$type <- factor(stat$type,
                      levels=c("wasteland","woodland","upland","paddy"))
  stat$group <- factor(stat$group,
                       levels=c("low","high"))
  ##-----------------------------
  ## low vs high
  ##-----------------------------
  sig <- data.frame()
  for(i in levels(df$type)){
    tmp <- subset(df,type==i)
    fit <- aov(as.formula(paste(yvar,"~group")),data=tmp)
    p <- summary(fit)[[1]][["Pr(>F)"]][1]
    star <- ifelse(p<0.001,"***",
                   ifelse(p<0.01,"**",
                          ifelse(p<0.05,"*","ns")))
    ymax <- max(stat$mean[stat$type==i]+
                  stat$se[stat$type==i])
    sig <- rbind(sig,
                 data.frame(type=i,
                            y=ymax*1.15,
                            label=star))
  }
  ##-----------------------------
  ## 作图
  ##-----------------------------
  p <- ggplot(df,
              aes(type,
                  .data[[yvar]],
                  fill=group))+
    geom_bar(stat="summary",
             fun="mean",
             position=position_dodge(.9),
             width=.8)+
    stat_summary(fun.data=mean_se,
                 geom="errorbar",
                 width=.2,
                 position=position_dodge(.9))+
    geom_text(
      data = stat,
      aes(
        x = type,
        y = mean + se + 0.04 * max(stat$mean),
        label = letter,
        group = group
      ),
      position = position_dodge(width = 0.9),
      inherit.aes = FALSE,
      size = 5
    )+
    geom_text(
      data = sig,
      aes(
        x = type,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      size = 6
    )+
    ## 红色参考线
    geom_hline(
      yintercept = 1.0,
      colour = "red",
      linewidth = 0.8,
      linetype = "dashed"
    )+
    scale_fill_manual(values=c("#a6cee3","#fb9a99"))+
    labs(x=NULL,
         y=ylab)+
    theme_classic()+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=15,face="bold"),
          legend.position="top")
  cat("\n=============================\n")
  cat(yvar,"\n")
  print(letters)
  print(stat)
  return(p)
}

# Figures
p1=plot_bar(df,
         yvar="lnClnN",
         ylab="Ln(BG+CBH)/Ln(NAG+LAP)")
p1
p2=plot_bar(df,
            yvar="lnClnP",
            ylab="Ln(BG+CBH)/Ln(Phos)")
p2
p3=plot_bar(df,
            yvar="length",
            ylab="microbial C limitation")
p3
p4=plot_bar(df,
            yvar="angle",
            ylab="microbial P limitation")
p4





# the figures of SQI and SMF
df <- read.csv('Supplementary SQI_SMF.csv',header = T,stringsAsFactors = FALSE, encoding='UTF-8')
head(df)
df$type = as.factor(df$type)
df$type <- factor(df$type, levels = c('wasteland', 'woodland','upland','paddy'))
df$group <- factor(df$group, levels = c('low', 'high'))
# Turkey test
plot_bar <- function(df, yvar, ylab){
  ##-----------------------------
  df$type <- factor(df$type,
                    levels=c("wasteland","woodland","upland","paddy"))
  df$group <- factor(df$group,
                     levels=c("low","high"))
  ## 计算均值和SE
  stat <- df %>%
    group_by(type,group) %>%
    summarise(
      mean = mean(.data[[yvar]],na.rm=TRUE),
      se = sd(.data[[yvar]],na.rm=TRUE)/sqrt(n()),
      .groups="drop"
    )
  ## low组 Tukey
  low <- subset(df,group=="low")
  fit.low <- aov(as.formula(paste(yvar,"~type")),data=low)
  tukey.low <- HSD.test(fit.low,"type",group=TRUE)
  letter.low <- data.frame(
    type = rownames(tukey.low$groups),
    letter = tukey.low$groups$groups,
    stringsAsFactors = FALSE
  )
  letter.low$type <- factor(
    letter.low$type,
    levels = c("wasteland","woodland","upland","paddy")
  )
  letter.low <- letter.low[order(letter.low$type), ]
  letter.low$group <- "low"
  ##-----------------------------
  ## high组 Tukey
  ##-----------------------------
  high <- subset(df,group=="high")
  fit.high <- aov(as.formula(paste(yvar,"~type")),data=high)
  tukey.high <- HSD.test(fit.high,"type",group=TRUE)
  letter.high <- data.frame(
    type = rownames(tukey.high$groups),
    letter = tukey.high$groups$groups,
    stringsAsFactors = FALSE
  )
  letter.high$type <- factor(
    letter.high$type,
    levels = c("wasteland","woodland","upland","paddy")
  )
  letter.high <- letter.high[order(letter.high$type), ]
  letter.high$group <- "high"
  letters <- rbind(letter.low,letter.high)
  stat <- left_join(stat,letters,
                    by=c("type","group"))
  stat$type <- factor(stat$type,
                      levels=c("wasteland","woodland","upland","paddy"))
  stat$group <- factor(stat$group,
                       levels=c("low","high"))
  ##-----------------------------
  ## low vs high
  ##-----------------------------
  sig <- data.frame()
  for(i in levels(df$type)){
    tmp <- subset(df,type==i)
    fit <- aov(as.formula(paste(yvar,"~group")),data=tmp)
    p <- summary(fit)[[1]][["Pr(>F)"]][1]
    star <- ifelse(p<0.001,"***",
                   ifelse(p<0.01,"**",
                          ifelse(p<0.05,"*","ns")))
    ymax <- max(stat$mean[stat$type==i]+
                  stat$se[stat$type==i])
    sig <- rbind(sig,
                 data.frame(type=i,
                            y=ymax*1.15,
                            label=star))
  }
  ##-----------------------------
  ## 作图
  ##-----------------------------
  p <- ggplot(df,
              aes(type,
                  .data[[yvar]],
                  fill=group))+
    geom_bar(stat="summary",
             fun="mean",
             position=position_dodge(.9),
             width=.8)+
    stat_summary(fun.data=mean_se,
                 geom="errorbar",
                 width=.2,
                 position=position_dodge(.9))+
    geom_text(
      data = stat,
      aes(
        x = type,
        y = mean + se + 0.04 * max(stat$mean),
        label = letter,
        group = group
      ),
      position = position_dodge(width = 0.9),
      inherit.aes = FALSE,
      size = 5
    )+
    geom_text(
      data = sig,
      aes(
        x = type,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      size = 6
    )+
    scale_fill_manual(values=c("#a6cee3","#fb9a99"))+
    labs(x=NULL,
         y=ylab)+
    theme_classic()+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=15,face="bold"),
          legend.position="top")
  cat("\n=============================\n")
  cat(yvar,"\n")
  print(letters)
  print(stat)
  return(p)
}
p5=plot_bar(df,
            yvar="SQI",
            ylab="SQI")
p5
p6=plot_bar(df,
            yvar="emf",
            ylab="SMF")
p6


library(patchwork)
p7=(p1+p2)/(p3+p4)/(p5+p6)+
  plot_layout(
    widths=c(1,1),
    heights=c(1,1,1)
  )
p7
ggsave('fig 1.pdf', p7, width = 8, height = 10)


# the result of Anova
df <- read.csv("Supplementary raw data.csv")
vars <- c("lnClnN","lnClnP","length","angle")
library(agricolae)

for(v in vars){
  cat("\n=====================================\n")
  cat("Variable:", v, "\n")
  ## ANOVA
  fit <- aov(as.formula(paste(v, "~ type * group")), data = df)
  print(summary(fit))
  ## low
  low <- subset(df, group == "low")
  fit.low <- aov(as.formula(paste(v, "~ type")), data = low)
  cat("\nLow group Tukey:\n")
  print(HSD.test(fit.low, "type", group = TRUE)$groups)
  ## high
  high <- subset(df, group == "high")
  fit.high <- aov(as.formula(paste(v, "~ type")), data = high)
  cat("\nHigh group Tukey:\n")
  print(HSD.test(fit.high, "type", group = TRUE)$groups)
  ##low and high
  cat("\nLow vs High:\n")
  for(tp in levels(df$type)){
    tmp <- subset(df, type == tp)
    fit2 <- aov(as.formula(paste(v, "~ group")), data = tmp)
    cat(tp, "\n")
    print(summary(fit2))
  }
}


df <- read.csv("SQI_SMF.csv")
vars <- c("SQI","emf")
library(agricolae)

for(v in vars){
  cat("\n=====================================\n")
  cat("Variable:", v, "\n")
  ## ANOVA
  fit <- aov(as.formula(paste(v, "~ type * group")), data = df)
  print(summary(fit))
  ## low
  low <- subset(df, group == "low")
  fit.low <- aov(as.formula(paste(v, "~ type")), data = low)
  cat("\nLow group Tukey:\n")
  print(HSD.test(fit.low, "type", group = TRUE)$groups)
  ## high
  high <- subset(df, group == "high")
  fit.high <- aov(as.formula(paste(v, "~ type")), data = high)
  cat("\nHigh group Tukey:\n")
  print(HSD.test(fit.high, "type", group = TRUE)$groups)
  ## low and high
  cat("\nLow vs High:\n")
  for(tp in levels(df$type)){
    tmp <- subset(df, type == tp)
    fit2 <- aov(as.formula(paste(v, "~ group")), data = tmp)
    cat(tp, "\n")
    print(summary(fit2))
  }
}



# Output all statistical results. xlsx
library(openxlsx)
library(agricolae)
df <- read.csv("raw data.csv")
df$type  <- factor(df$type,
                   levels=c("wasteland","woodland","upland","paddy"))
df$group <- factor(df$group,
                   levels=c("low","high"))
vars <- c("lnClnN","lnClnP","length","angle")
wb <- createWorkbook()
##############################
## Table S1  Two-way ANOVA
##############################
addWorksheet(wb,"Table S1")
anova.out <- data.frame()
for(v in vars){
  fit <- aov(as.formula(paste(v,"~type*group")),data=df)
  aa <- summary(fit)[[1]]
  tmp <- data.frame(
    Variable=v,
    Source=rownames(aa)[1:3],
    Df=aa$Df[1:3],
    F=round(aa$`F value`[1:3],3),
    P=signif(aa$`Pr(>F)`[1:3],3)
  )
  anova.out <- rbind(anova.out,tmp)
}
writeData(wb,"Table S1",anova.out)


##############################
## Table S2
##############################
addWorksheet(wb,"Table S2")
tukey.out <- data.frame()
for(v in vars){
  ## low
  fit.low <- aov(as.formula(paste(v,"~type")),
                 data=subset(df,group=="low"))
  low <- HSD.test(fit.low,"type",group=TRUE)$groups
  low <- data.frame(
    Variable=v,
    Salinity="Low",
    Type=rownames(low),
    Mean=round(low[,1],4),
    Group=low$groups
  )
  ## high
  fit.high <- aov(as.formula(paste(v,"~type")),
                  data=subset(df,group=="high"))
  high <- HSD.test(fit.high,"type",group=TRUE)$groups
  high <- data.frame(
    Variable=v,
    Salinity="High",
    Type=rownames(high),
    Mean=round(high[,1],4),
    Group=high$groups
  )
  tukey.out <- rbind(tukey.out,
                     low,
                     high)
}
writeData(wb,"Table S2",tukey.out)


##############################
## Table S3
##############################
addWorksheet(wb,"Table S3")
compare.out <- data.frame()
for(v in vars){
  for(tp in levels(df$type)){
    tmp <- subset(df,type==tp)
    fit <- aov(as.formula(paste(v,"~group")),
               data=tmp)
    aa <- summary(fit)[[1]]
    p <- aa$`Pr(>F)`[1]
    star <- ifelse(p<0.001,"***",
                   ifelse(p<0.01,"**",
                          ifelse(p<0.05,"*","ns")))
    compare.out <- rbind(compare.out,
                         data.frame(
                           Variable=v,
                           Stage=tp,
                           F=round(aa$`F value`[1],3),
                           P=signif(p,3),
                           Significance=star
                         ))
  }
}
writeData(wb,"Table S3",compare.out)
saveWorkbook(wb,
             "Supplementary statistical results 1.xlsx",
             overwrite=TRUE)



library(openxlsx)
library(agricolae)
df <- read.csv("SQI_SMF.csv")
df$type  <- factor(df$type,
                   levels=c("wasteland","woodland","upland","paddy"))
df$group <- factor(df$group,
                   levels=c("low","high"))
vars <- c("SQI","emf")
wb <- createWorkbook()
##############################
## Table S1  Two-way ANOVA
##############################
addWorksheet(wb,"Table S1")
anova.out <- data.frame()
for(v in vars){
  fit <- aov(as.formula(paste(v,"~type*group")),data=df)
  aa <- summary(fit)[[1]]
  tmp <- data.frame(
    Variable=v,
    Source=rownames(aa)[1:3],
    Df=aa$Df[1:3],
    F=round(aa$`F value`[1:3],3),
    P=signif(aa$`Pr(>F)`[1:3],3)
  )
  anova.out <- rbind(anova.out,tmp)
}
writeData(wb,"Table S1",anova.out)


##############################
## Table S2
##############################
addWorksheet(wb,"Table S2")
tukey.out <- data.frame()
for(v in vars){
  ## low
  fit.low <- aov(as.formula(paste(v,"~type")),
                 data=subset(df,group=="low"))
  low <- HSD.test(fit.low,"type",group=TRUE)$groups
  low <- data.frame(
    Variable=v,
    Salinity="Low",
    Type=rownames(low),
    Mean=round(low[,1],4),
    Group=low$groups
  )
  ## high
  fit.high <- aov(as.formula(paste(v,"~type")),
                  data=subset(df,group=="high"))
  high <- HSD.test(fit.high,"type",group=TRUE)$groups
  high <- data.frame(
    Variable=v,
    Salinity="High",
    Type=rownames(high),
    Mean=round(high[,1],4),
    Group=high$groups
  )
  tukey.out <- rbind(tukey.out,
                     low,
                     high)
}
writeData(wb,"Table S2",tukey.out)


##############################
## Table S3
##############################
addWorksheet(wb,"Table S3")
compare.out <- data.frame()
for(v in vars){
  for(tp in levels(df$type)){
    tmp <- subset(df,type==tp)
    fit <- aov(as.formula(paste(v,"~group")),
               data=tmp)
    aa <- summary(fit)[[1]]
    p <- aa$`Pr(>F)`[1]
    star <- ifelse(p<0.001,"***",
                   ifelse(p<0.01,"**",
                          ifelse(p<0.05,"*","ns")))
    compare.out <- rbind(compare.out,
                         data.frame(
                           Variable=v,
                           Stage=tp,
                           F=round(aa$`F value`[1],3),
                           P=signif(p,3),
                           Significance=star
                         ))
  }
}
writeData(wb,"Table S3",compare.out)
saveWorkbook(wb,
             "Supplementary statistical results 2.xlsx",
             overwrite=TRUE)
