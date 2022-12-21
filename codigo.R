library(tidyverse)
library(haven)
library(estimatr)



#####Codigo 1######

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

abortion <- read_data("abortion.dta") %>% 
  mutate(
    repeal = as_factor(repeal),
    year   = as_factor(year),
    fip    = as_factor(fip),
    fa     = as_factor(fa),
  )

reg <- abortion %>% 
  filter(bf15 == 1) %>% 
  lm_robust(lnr ~ repeal*year + fip + acc + ir + pi + alcohol+ crack + poverty+ income+ ur,
            data = ., weights = totpop, clusters = fip)

abortion_plot <- tibble(
  sd = reg[[2]][76:90],
  mean = reg[[1]][76:90],
  year = c(1986:2000))

fig1<-abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1985.7, xmax=1992.3, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05))+
  labs(x = "Year",y = " Estimated coefficient (Repeal x year)")+ 
  theme(axis.text = element_text(size=15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5)))



ggsave(fig1, filename = "fig1.png",
       dpi = 300, width = 12, height = 7)




#####Codigo 2######


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

abortion <- read_data("abortion.dta") %>% 
  mutate(
    repeal  = as_factor(repeal),
    year    = as_factor(year),
    fip     = as_factor(fip),
    fa      = as_factor(fa),
    younger = as_factor(younger),
    yr      = as_factor(case_when(repeal == 1 & younger == 1 ~ 1, TRUE ~ 0)),
    wm      = as_factor(case_when(wht == 1 & male == 1 ~ 1, TRUE ~ 0)),
    wf      = as_factor(case_when(wht == 1 & male == 0 ~ 1, TRUE ~ 0)),
    bm      = as_factor(case_when(wht == 0 & male == 1 ~ 1, TRUE ~ 0)),
    bf      = as_factor(case_when(wht == 0 & male == 0 ~ 1, TRUE ~ 0))
  ) %>% 
  filter(bf == 1 & (age == 15 | age == 25))

regddd <- lm_robust(lnr ~ repeal*year + younger*repeal + younger*year + yr*year + fip*t + acc + ir + pi + alcohol + crack + poverty + income + ur,
                    data = abortion, weights = totpop, clusters = fip)

abortion_plot <- tibble(
  sd = regddd$std.error[110:124],
  mean = regddd$coefficients[110:124],
  year = c(1986:2000))

fig2<-abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1985.7, xmax=1992.3, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean-sd*1.96, ymax = mean+sd*1.96), width = 0.2,
                position = position_dodge(0.05))+
labs(x = "Year",y = "Estimated coefficient (Repeal x age x year)")+ 
  theme(axis.text = element_text(size=15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5,  size=rel(1.5)))



ggsave(fig2, filename = "fig2.png",
       dpi = 300, width = 12, height = 7)



####Going beyond Cunningham and Cornwell####


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

abortion1 <- read_data("abortion.dta") %>% 
  mutate(
    repeal = as_factor(repeal),
    year   = as_factor(year),
    fip    = as_factor(fip),
    fa     = as_factor(fa),
  )

reg <- abortion1 %>% 
  filter(race == 2 & sex == 2 & age == 20) %>% 
  lm_robust(lnr ~ repeal*year + fip + acc + ir + pi + alcohol+ crack + poverty+ income+ ur,
            data = ., weights = totpop, clusters = fip)



abortion_plot <- tibble(
  sd = reg$std.error[76:90],
  mean = reg$coefficients[76:90],
  year = c(1986:2000))


fig3<-abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1990.7, xmax=1997.3, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean-sd*1.96, ymax = mean+sd*1.96), width = 0.2,
                position = position_dodge(0.05))+
  labs(x = "Year",y = "Estimated coefficient (Repeal x year)")+ 
  theme(axis.text = element_text(size=15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5)))



ggsave(fig3, filename = "fig3.png",
       dpi = 300, width = 12, height = 7)





####codigo 4#####

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

abortion <- read_data("abortion.dta") %>% 
  mutate(
    repeal   = as_factor(repeal),
    year     = as_factor(year),
    fip      = as_factor(fip),
    fa       = as_factor(fa),
    younger2 = case_when(age == 20 ~ 1, TRUE ~ 0),
    yr2      = as_factor(case_when(repeal == 1 & younger2 == 1 ~ 1, TRUE ~ 0)),
    wm       = as_factor(case_when(wht == 1 & male == 1 ~ 1, TRUE ~ 0)),
    wf       = as_factor(case_when(wht == 1 & male == 0 ~ 1, TRUE ~ 0)),
    bm       = as_factor(case_when(wht == 0 & male == 1 ~ 1, TRUE ~ 0)),
    bf       = as_factor(case_when(wht == 0 & male == 0 ~ 1, TRUE ~ 0))
  )


regd<- abortion %>% 
  filter(bf == 1 & (age == 20 | age ==25)) %>% 
  lm_robust(lnr ~ repeal*year*younger2 + acc + ir + pi + alcohol + crack + poverty + income + ur,
            data = ., weights = totpop, clusters = fip)




abortion_plot <- tibble(
  sd = regd$std.error[58:72],
  mean = regd$coefficients[58:72],
  year = c(1986:2000))

fig4<-abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1990.7, xmax=1997.3, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean-sd*1.96, ymax = mean+sd*1.96), width = 0.2,
                position = position_dodge(0.05))+
  labs(x = "Year",y = "Estimated coefficient (Repeal x age x year)")+ 
  theme(axis.text = element_text(size=15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5,  size=rel(1.5)))



ggsave(fig4, filename = "fig4.png",
       dpi = 300, width = 12, height = 7)







