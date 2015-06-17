


lib <- c("foreign", "lme4", "car", "descr", "effects", "xtable",  "sas7bdat", "texreg", "lmtest", "Hmisc", "ggplot2")
require(lib)
lapply(lib, require, character.only=T)

## re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}

wmata <- read.dta('Data/Collapsed_Example_0616_Hiro1.dta')
wmata <- subset(wmata, log(ridersum) >0)


null_model <- gls( log1p(ridersum) ~ peak_fare_05, data=wmata)
null_model_multi_o <- lme( log1p(ridersum) ~ peak_fare_05, random=~1 | mstn_id_o, data=wmata)

anova(null_model, null_model_multi_o)

null_model_multi_d <- lme( log1p(ridersum) ~ peak_fare_05, random=~1 | mstn_id_d, data=wmata)

anova(null_model_multi_o, null_model_multi_d)

null_model_od <- lmer( log(ridersum) ~ peak_fare_05 + (1 | mstn_id_d)+ (1 | mstn_id_o) , data=wmata)


plot(null_model_od)
dev.print(png, filename="output/nullod_residplot.png", height=300)

qqnorm(resid(null_model_od))
dev.print(png, filename="output/nullod_qqplot.png", height=300)

gCaterpillar(ranef(null_model_od, condVar=TRUE))  ## using ggplot2
dev.print(png, filename="output/model1_qq_rand_d.png", height=300)

htmlreg(null_model_od, type="html", file="output/nullod_results.docx")

tbl1 = VarCorr(null_model_od)
class(tbl1) = "matrix"
print.xtable(xtable(tbl1), type="html", file="output/nullod_rand.docx")




model_1<- lmer( log(ridersum) ~ peak_fare_05 + off_peak_fare_05 + track_mile + comp_mile + travel_time +
                  (1 | mstn_id_o) + (1 | mstn_id_d),
                 data = wmata)

model_1a<- lmer( log(ridersum) ~ peak_fare_05 + off_peak_fare_05 + track_mile + comp_mile + travel_time +
                  (1 | mstn_id_d)+ (1 | mstn_id_o),
                data = wmata)

summary(model_1)


model_2<- lmer( log(ridersum) ~ peak_fare_05 + off_peak_fare_05 + track_mile + comp_mile + travel_time + MedianHHIncome_O +
                  (1 | mstn_id_o) + (1 | mstn_id_d),
                data = wmata) 

model_2a<- lmer( log(ridersum) ~ peak_fare_05 + off_peak_fare_05 + track_mile + comp_mile + travel_time + MedianHHIncome_O +
                  (1 + MedianHHIncome_O| mstn_id_o) + (1 | mstn_id_d),
                data = wmata) 

model_3 <- lmer( log(ridersum) ~ log(peak_fare_05) + inv_comp_mile + log_tt_ratio2_fill + tt_ratio201 + log_parking_users + redline_OD + m25_1 + m25_2 + logjobshalfUp_D + log_tphpeakv2_D + log_buslinescount_D + log_ParkingCapacityNew_O + log_hh_o + log_buslinescount_O +
                   (1 | mstn_id_o) + (1 | mstn_id_d),
                 data = wmata)


model_4 <- lmer( log(ridersum) ~ log(peak_fare_05) + log(off_peak_fare_05) + track_mile + inv_comp_mile + log_tt_ratio2_fill + tt_ratio201 + log_parking_users + redline_OD + m25_1 + m25_2 + logjobshalfUp_D + log_tphpeakv2_D + log_buslinescount_D + log_ParkingCapacityNew_O + log_hh_o + log_buslinescount_O +
                   + log1p(travel_time) + log(MedianHHIncome_O) +
                   (1 | mstn_id_o) + (1 | mstn_id_d),
                 data = wmata)


model_5 <- lmer( log(ridersum) ~ log(peak_fare_05) + log(off_peak_fare_05) + track_mile + inv_comp_mile + log_tt_ratio2_fill + tt_ratio201 + log_parking_users + redline_OD + m25_1 + m25_2 + logjobshalfUp_D + log_tphpeakv2_D + 
                   log_buslinescount_D + log_ParkingCapacityNew_O + log_hh_o + log_buslinescount_O + log1p(travel_time) + 
                   (1 + log_ParkingCapacityNew_O + log_hh_o | mstn_id_o) + (1 +logjobshalfUp_D + log_tphpeakv2_D | mstn_id_d),
                 data = wmata)
