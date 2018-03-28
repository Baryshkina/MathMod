library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
setwd("C:/125")
getwd()
br=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
br
br = br[-1,] 
br
glimpse(br) 
br = select(br, -(roll)) 
br
br<-br[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)]
names(br)
br = br %>% mutate_if(is.character, factor) 
names(br) = str_replace_all(names(br), "[!]","_emph_") 
names(br) = names(br) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(br) 
sapply(br,is.numeric) 
br_numeric = br[,sapply(br,is.numeric) ] 
br_non_numeric = br[,!sapply(br,is.numeric) ] 
br$daytime = as.logical(br$daytime) 
br = subset(br, as.Date(date) >= as.Date("2013-06-05") & as.Date(date) <= as.Date("2013-09-05") & daytime == T) 
br
cor_td = cor(br_numeric) 
cor_td
cor_td = cor(drop_na(br_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
vars
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula
mod = lm(formula, data=br)
row_numbers = 1:length(br$date) 
teach = sample(row_numbers, floor(length(br$date)*.7)) 
test = row_numbers[-teach] 
teaching_br_unq = br[teach,]
testing_br_unq = br[test,] 
mod1 = lm(co2_flux ~ (DOY + Tau +rand_err_Tau + H + LE + 
                        rand_err_LE + h2o_flux + rand_err_h2o_flux + 
                        co2_molar_density + co2_mixing_ratio + RH + VPD + 
                        max_speed + TKE  + un_H + un_LE + un_h2o_flux + u_var + 
                        v_var + co2_signal_strength_7200 + flowrate)^2,data=br)
coef(mod1) 
resid(mod1) 
confint(mod1)
summary(mod1)
anova(mod1)
mod2 = lm(co2_flux ~ (DOY + Tau +rand_err_Tau + H + LE + 
                        rand_err_LE + h2o_flux + rand_err_h2o_flux + 
                        co2_molar_density + co2_mixing_ratio + RH + VPD + 
                        max_speed + un_LE + un_h2o_flux + 
                        co2_signal_strength_7200 + flowrate)^2,data=br)
coef(mod2) 
resid(mod2) 
confint(mod2)
summary(mod2)
anova(mod2)
mod3 = lm(co2_flux ~ (DOY + Tau +rand_err_Tau + H + LE + 
                        rand_err_LE + h2o_flux + rand_err_h2o_flux + 
                        co2_molar_density + co2_mixing_ratio + RH + VPD + 
                        max_speed + un_LE + un_h2o_flux + 
                        co2_signal_strength_7200 + flowrate)^2-DOY:VPD - 
            DOY:un_LE - DOY:flowrate - Tau:LE - Tau:rand_err_LE - Tau:h2o_flux - 
            Tau:rand_err_h2o_flux - Tau:co2_mixing_ratio - Tau:VPD - Tau:max_speed - 
            Tau:un_LE - Tau:un_h2o_flux - Tau:co2_signal_strength_7200 - rand_err_Tau:rand_err_h2o_flux - 
            rand_err_Tau:co2_molar_density - rand_err_Tau:co2_mixing_ratio - 
            rand_err_Tau:VPD - rand_err_Tau:un_LE - rand_err_Tau:un_h2o_flux - 
            rand_err_Tau:co2_signal_strength_7200 - H:rand_err_LE - 
            H:h2o_flux - H:rand_err_h2o_flux - H:VPD - H:max_speed - 
            H:un_h2o_flux - H:flowrate - LE:h2o_flux - LE:co2_molar_density - 
            LE:co2_mixing_ratio - LE:max_speed - LE:un_LE - LE:un_h2o_flux - 
            LE:co2_signal_strength_7200 - LE:flowrate - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:co2_molar_density - rand_err_LE:RH - rand_err_LE:VPD - 
            rand_err_LE:max_speed - rand_err_LE:un_LE - rand_err_LE:un_h2o_flux - 
            h2o_flux:co2_molar_density - h2o_flux:RH - h2o_flux:VPD - h2o_flux:max_speed -
            h2o_flux:un_h2o_flux - h2o_flux:co2_signal_strength_7200 - h2o_flux:flowrate -
            rand_err_h2o_flux:RH - rand_err_h2o_flux:VPD - rand_err_h2o_flux:flowrate - 
            co2_molar_density:max_speed - co2_molar_density:co2_signal_strength_7200 - 
            co2_mixing_ratio:co2_signal_strength_7200 - RH:VPD - RH:un_h2o_flux -
            RH:flowrate - VPD:un_h2o_flux - VPD:co2_signal_strength_7200 - 
            un_h2o_flux:co2_signal_strength_7200 - co2_signal_strength_7200:flowrate -
            un_LE:co2_signal_strength_7200 - max_speed:un_h2o_flux -
            max_speed:un_LE - VPD:flowrate - RH:co2_signal_strength_7200 - 
            un_LE:un_h2o_flux - VPD:un_LE - co2_mixing_ratio:flowrate -
            co2_molar_density:un_LE - rand_err_LE:co2_mixing_ratio -
            h2o_flux:un_LE - rand_err_Tau:flowrate - DOY:un_h2o_flux -
            Tau:flowrate - co2_mixing_ratio:max_speed - co2_mixing_ratio:VPD - 
            co2_mixing_ratio:RH ,data=br)
coef(mod3) 
resid(mod3) 
confint(mod3)
summary(mod3)
anova(mod3)
plot(mod3)