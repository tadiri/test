
### FILE OPENING & PROCESSING
library(here)

# FACTORS' MEANS INTO MAIN DF FILE:

add_factors_upd <- function(df) {
  
  df$rad_enh_true28 = rowMeans(df %>% select(rad_esc_3_1:rad_esc_3_4), dims = 1)
  df$dys_alien_frbio26 = rowMeans(df %>% select(dys_sent_1_1, dys_sent_1_3, dys_sent_3_2), dims = 1)
  df$lib_par_resp13 = rowMeans(df %>% select(lib_enh_3_2,lib_enh_3_3,lib_enh_3_1), dims = 1) 
  df$rad_dig_twin10 = rowMeans(df %>% select(rad_sub_2_2,rad_sub_2_3,rad_sub_2_1), dims = 1)
  
  
  return(df)
}

# CORRELATION MATRICES

#Spearman
draw_corr_s <- function(df) {
  ggcorr(df, method = c("pairwise.complete.obs", "spearman"), 
    # geom = "blank", label = TRUE, hjust = 0.75) +
    # geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
    # scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
    # guides(color = FALSE, alpha = FALSE)
  nbreaks = 20,
  label = TRUE,
  label_round = 1,
  label_size = 3,
  digits = 2,
  legend.size = 8,
  hjust = 0.75,
  size = 2,
  layout.exp = 1,
  low = "#3B9AB2",
  mid = "#FFFFFF",
  high = "#F21A00")
}

#Pearson
draw_corr_p <- function(df) {
  ggcorr(df, method = c("pairwise.complete.obs", "pearson"),
         nbreaks = ,
         label = TRUE,
         label_round = 1,
         digits = 2,
         legend.size = 8,
         hjust = 0.75, 
         size = 2, 
         layout.exp = 1,
         label_size = 2,
         low = "#3B9AB2",
         mid = "#FFFFFF",
         high = "#F21A00")
}



## VIOLIN PLOTS

#' plot_raincloud_v
#' A convenience function to create a vertical rain cloud plot faceted by one other variable.
#' @param data A data.frame or tibble that is passed to \link[ggplot2]{ggplot()}.
#' @param x_var A character (1) string, specifying the variable that is plotted on the x-axis.
#' @param y_var A character (1) string, specifying the variable that is plotted on the y-axis.
#' @param facet A character (1) string, specifying the variable that is used in \link[ggplot2]{facet_wrap()} for faceting.
#' @return A ggplot object.
#' @examples plot_raincloud_v(df_qole_items, "Group", "qole_1", "p_stimulus")

plot_raincloud_v <- function(data, x_var, y_var, facet) {
  data[, x_var] <- as.factor(data[, x_var])
  plot <- ggplot(data, aes_string(
    x = x_var,
    y = y_var,
    fill = x_var,
    colour = x_var
  )) +
    geom_flat_violin(
      position = position_nudge(x = .2, y = 0),
      adjust = 2,
      trim = TRUE,
      alpha = .8,
      scale = "width"
    ) +
    geom_point(position = position_jitter(width = .15), size = .25) +
    stat_summary(
      fun = "mean",
      colour = "black",
      size = 2,
      geom = "point"
    ) +
    geom_boxplot(
      position = position_nudge(x = .25, y = 0),
      aes_string(x = x_var, y = y_var),
      outlier.shape = NA,
      alpha = 0.3,
      width = 0.1,
      colour = "black"
    ) +
    theme_cowplot() + guides(fill = FALSE, colour = FALSE) +
    facet_wrap(reformulate(facet)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")
  print(plot)
  return(plot)
}



means_sig_pain <- function(i) { 
  
  if (i == 'all') { 
    mean_list1 <- list(dfw$qole, dfw$vas_str, dfw$vas_rsm)
    mean_list2 <- list(dfout$qole, dfout$vas_str, dfout$vas_rsm)
    
    ttest_fac <- data.frame('Factor' = c("qole", "vas_str", "vas_rsm"), 
                            'With' = 'NULL', 'Without' = 'NULL', 'p_t.test' = 'NULL', 'Significance1' = 'NULL', 
                            'p_wilcox' = 'NULL', 'Significance2' = 'NULL')
  }
  
  
  
  foreach(mean1 = mean_list1, mean2 = mean_list2, i = 1:nrow(ttest_fac)) %do% {
    ttest <- t.test(mean1, mean2)
    ttest_fac[i, "p_t.test"] <- ttest$p.value
    if (ttest$p.value > 0.05) {
      ttest_fac[i, "Significance1"] <- " "
    } else if (ttest$p.value <= 0.05 & ttest$p.value > 0.01) {
      ttest_fac[i, "Significance1"] <- "*"
    } else if (ttest$p.value <= 0.01) {
      ttest_fac[i, "Significance1"] <- "***"
    }
    ttest_fac[i, c("With", "Without")] <- c(mean(mean1), mean(mean2))
  }
  
  foreach(mean1 = mean_list1, mean2 = mean_list2, i = 1:nrow(ttest_fac)) %do% {
    wil <- wilcox.test(mean1, mean2)
    ttest_fac[i, "p_wilcox"] <- wil$p.value
    if (wil$p.value > 0.05) {
      ttest_fac[i, "Significance2"] <- " "
    } else if (wil$p.value <= 0.05 & wil$p.value > 0.01) {
      ttest_fac[i, "Significance2"] <- "*"
    } else if (wil$p.value <= 0.01) {
      ttest_fac[i, "Significance2"] <- "***"
    }
    
  }
  
  
  ttest_fac <- transform(ttest_fac, With = as.numeric(With),
                         Without = as.numeric(Without),
                         p_t.test = as.numeric(p_t.test),
                         p_wilcox = as.numeric(p_wilcox))
  ttest_fac[,c('With', 'Without', 'p_t.test',
               'p_wilcox')] <- round(ttest_fac[,c('With', 'Without', 'p_t.test', 'p_wilcox')], 3)
  
  return (ttest_fac)
}


## ARTool

artfun <- function(i) {
  df_out=NULL
  scount = 0
  for (target in TARGETS) {
    form = as.formula(paste(target," ~ ", FACTOR1))
    m=art(form, data=i) ### <----- change the data source
    a=anova(m)
    F_val=a$`F value`
    p_val=a$`Pr(>F)`
    if (p_val<0.05) scount <- scount+1
    df_out = rbind(df_out, fixp(data.frame(target,F_val,p_val),add_stars = TRUE))
  }
  return(df_out)
}

## FUNCTION TO SAVE RESULTS OF EFA WITH LOADINGS 
getS3method("print","loadings")
printLoadings <- function (x, digits = 2, cutoff = 0.1, sort = FALSE, ...) 
{
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p), ]
  }
  cat("\nLoadings:\n")
  fx <- format(round(Lambda, digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  newx <- print(fx, quote = FALSE, ...) # I assigned this to a variable
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1) 
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx) #previously returned x
}

## CRONBACH ALPHA TABLE               
alpha_table <- function(df, name_list) {
  alfa_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("factor", "cr_alpha"))
  for (x in name_list) {
    alphaa <- psych::alpha(df[ , grep(x, names(df))])
    out = round(alphaa$total$raw_alpha, 2)
    alfa_df = rbind(alfa_df, list(factor = x, cr_alpha = out))
  }
  return(alfa_df)
}

## CRONBACH ALPHA TABLE 2.0               
my_cron_fun <- function(list_of_pas){
  alfa_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("factor", "cr_alpha"))
  list_of_df_names = names(list_of_pas)
  pa_number = 1
  for (i in list_of_pas){
    alphaa <- psych::alpha(i)
    out = round(alphaa$total$raw_alpha, 2)
    alfa_df = rbind(alfa_df, list(factor = list_of_df_names[pa_number], cr_alpha = out))
    pa_number<-pa_number+1
  }
  return(alfa_df)
}

## DOWNLOAD  and INSTALL PACKAGES
allpackages <- function() {
  if(!require(pacman)){install.packages("pacman")}
  pacman::p_load(boot, d3r, DAAG, DataExplorer, foreach, foreign, FSA, GGally, ggplot2, ggpubr, heplots, knitr,
                 lattice, lavaan, phia, plyr, psych, rcompanion, relaimpo, rstatix, skimr, matrixStats, xlsx, 
                 semTools, cowplot, zeallot, lmerTest, MuMIn, nFactors, RColorBrewer, 
                 performance, see, patchwork, pheatmap, ggplotify, heatmaply, factoextra)
  library(buildmer)
  library(semTools)
  library(matrixStats)
  library(ARTool)
  library(broom)
  library(car)
  library(d3r)
  library(DAAG)
  library(DataExplorer)
  library(dplyr)
  library(foreach)
  library(foreign) 
  library(GGally)
  library(ggplot2)
  library(ggpubr)
  library(grDevices)
  library(grid)
  library(gridExtra)
  library(heplots)
  library(Hmisc)
  #library(hrbrthemes)
  library(knitr)
  library(labelVector)
  library(lavaan)
  library(likert)
  library(nlme)
  library(phia)
  library(plotly)
  library(psych)
  library(relaimpo)
  library(rstatix)
  library(skimr)
  library(tidyverse)
  library(viridis)
  library(viridisLite)
  library(shiny)
  library(lme4)
  library(nFactors)
  library(tidyr)
  library(RColorBrewer)
  library(effectsize)
  library(MASS)
  #library(sjPlot)
  #library(Cairo)
  library(performance)
  library(pheatmap) ## for heatmap generation
  library(tidyverse) ## for data wrangling
  library(ggplotify) ## to convert pheatmap to ggplot2
  library(heatmaply) ## for constructing interactive heatmap
  library("FactoMineR")
  
  
  
  
  #update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
  
  return()
}

## MY BARPLOT WITH SIGNFICANT LEVEL
mybarplot <- function (df, score_max, lab_size, mycol, num) {
  pd = position_dodge2(
    width = 0.1,
    preserve = c("total"),
    padding = 0.6,
    reverse = FALSE
  )
  ggplot(df, aes(x = Factors, y = Mean, fill = {{mycol}})) +
    scale_fill_manual(values = brewer.pal(n = num, name = "Dark2")) +
    coord_cartesian(ylim = c(1, score_max)) +
    geom_bar(stat = "identity", position = "dodge2") +
    geom_errorbar(aes(ymin  = Trad.lower,
                      ymax  = Trad.upper,
    ),
    #width = 0.5,
    size  = 0.3,
    position = pd,
    color = "black",
    ) + theme_classic() +
    theme(axis.text = element_text(size=lab_size))
}


### PIVOTING TABLE 

to_piv <- function(df,mycol) {
  group_piv <-  pivot_longer(df_short %>%
                             select(c({{mycol}}, lib_sent, lib_enh, lib_hmn, lib_trans,
                                      dys_esc, dys_sent, dys_enh, dys_hmn,
                                      rad_esc, rad_sent, rad_ext, rad_sub)),
                                      c(lib_sent, lib_enh, lib_hmn, lib_trans,
                                      dys_esc, dys_sent, dys_enh, dys_hmn,
                                      rad_esc, rad_sent, rad_ext, rad_sub),
                                      names_to = "Factors", values_to = "score") %>% na.omit()
  return(group_piv)
}


# ## BOXPLOT FOR FACTOR COMPARISON BETWEEN TT
# mybarplot <- function (df, param, score_max, lab_size) {
#   pd = position_dodge2(
#     width = 0.1,
#     preserve = c("total"),
#     padding = 0.6,
#     reverse = FALSE
#   )
#   ggplot(df, aes(x = Factors, y = Mean, fill = param)) +
#     scale_fill_manual(values = c("#7C79F4" , "#EE6C55")) +
#     coord_cartesian(ylim = c(1, score_max)) +
#     geom_bar(stat = "identity", position = "dodge2") +
#     geom_errorbar(aes(ymin  = Trad.lower,
#                       ymax  = Trad.upper,
#                       ),
#                   #width = 0.5,
#                   size  = 0.3,
#                   position = pd,
#                   color = "black",
#     ) + theme_classic() + 
#     theme(axis.text = element_text(size=lab_size))  
# }


# fix display if p values in last column to '< .001' format
fixp <- function(x, dig=3, idx=NULL, add_stars=FALSE) {
  x <- as.data.frame(x)
  if(is.null(idx))
    idx=ncol(x)
  if(tolower(substr(names(x)[idx],1,1)) != "p")
    warning("The name of the last/specified column didn't start with P/p. This may indicate that p-values weren't in the last row, and thus, that this function is inappropriate.")
  x[,idx] <- round(x[,idx], dig)
  if(add_stars) { # add separate stars column (optional)
    x <- add_column(x, sign = rep("",nrow(x)), .after = idx)   # add *** column (optional)
  }
  for(i in 1:nrow(x)){
    val = x[i,idx]
    if(val == 0) {
      x[i,idx] <- paste0("<0.", paste0(rep(0,dig-1), collapse=""), "1")
    } else if (val == 1) {
      x[i,idx] <- paste0("1.", paste0(rep(0,dig-1), collapse=""), "0")
    }
    if (add_stars) {
      if (0.01 > val)  {
        x[i,idx+1] <- "***"
      } else if (val>= 0.05) {
        x[i,idx+1] <- " "
      } else if (val>= 0.01) {
        x[i,idx+1] <- "*"
      } else x[i,idx+1] <- " "
    }
  }
  if(add_stars) names(x) <- gsub("sign", "", names(x), fixed=TRUE)
  x
}

