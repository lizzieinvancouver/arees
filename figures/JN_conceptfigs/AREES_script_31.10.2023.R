library(tidyverse)
library(ggplot2)
library(ggthemes)

# making rnorms into dataframes that can be bound together for ggplot
sp1 <- data.frame(value = rnorm(1000000,mean = 53, sd = 25),
                  species="a")
sp2 <- data.frame(value = rnorm(1000000, mean = 82, sd = 25),
                  species="b")
sp3 <- data.frame(value = rnorm(1000000, mean = 275, sd = 25),
                  species="c")
sp4 <- data.frame(value = rnorm(1000000, mean = 240, sd = 25),
                  species="d")
sp5 <- data.frame(value = rnorm(1000000, mean = 310, sd = 25),
                  species="e")

# making the concept plot with ggplot()
sp_combined <- rbind(sp1,sp2,sp3,sp4,sp5)

jornada_abund_plot <- sp_combined %>%
  ggplot(aes(x=value,colour=species))+
  geom_density(linewidth=1)+
  theme_few()+
  labs(x = "Time (days)",
       y = "Abundance") +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  xlim(0,365) +
  ylim(0,0.02) +
  scale_colour_manual(values = c("a" = "#E69F00",
                                 "b" = "#56B4E9",
                                 "c" = "#CC79A7",
                                 "d" = "#009E73",
                                 "e" = "#F0E442"),
                      breaks = c("a","b","c","d","e"))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),)
jornada_abund_plot
# ggsave("plots/jornada_concept_abundance.png",plot=jornada_abund_plot,scale=1,dpi=600)
# ggsave("plots/jornada_concept_abundance.pdf",plot=jornada_abund_plot,scale=1,dpi=600)

############################################################################################################################
# One pulse plots
# making a big rnorm distribution centered around ~20 with a large sd
# pulse_initial <- data.frame(value = rnorm(1000000, mean = 20, sd = 75))

# pulse_i_plot <- pulse_initial %>%
#   ggplot(aes(x = value)) +
#   geom_density(linewidth = 1)+
#   theme_few()+
#   labs(y = "Resource quantity",
#        x = "Time (days)") +
#   theme(axis.title = element_text(size = 12,face = "bold",),
#         legend.title = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 10)) +
#   geom_vline(xintercept = 20, linewidth = 1,colour = "red", linetype = "twodash")+
#   xlim(0,365)+
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
# pulse_i_plot
# # ggsave("plots/one_pulse_concept.png",plot=pulse_i_plot,scale=1,dpi=600)

# making the responses per species
pulse_sp1 <- data.frame(value = rnorm(1000000, mean = 106, sd = 24),
                        species = "a")
pulse_sp2 <- data.frame(value = rnorm(1000000, mean = 135, sd = 20),
                        species = "b")
pulse_sp3 <- data.frame(value = rnorm(1000000, mean = 277, sd = 25),
                        species = "c")
pulse_sp4 <- data.frame(value = rnorm(1000000, mean = 199, sd = 38),
                        species = "d")
pulse_sp5 <- data.frame(value = rnorm(1000000, mean = 238, sd = 75),
                        species = "e")

response_combined <- rbind(pulse_sp1,pulse_sp2,pulse_sp3,pulse_sp4,pulse_sp5)

# one_pulse_response_plot <- response_combined %>%
#   ggplot(aes(x=value,colour=species))+
#   geom_density(linewidth=1)+
#   theme_few()+
#   labs(x = "Time (days)",
#        y = "Abundance") +
#   theme(axis.title = element_text(size = 12,face = "bold",),
#         legend.title = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 10)) +
#   xlim(0,365) +
#   scale_colour_manual(values = c("a" = "#E69F00",
#                                  "b" = "#56B4E9",
#                                  "c" = "#CC79A7",
#                                  "d" = "#009E73",
#                                  "e" = "#F0E442"),
#                       breaks = c("a","b","c","d","e"))+
#   geom_vline(xintercept = 20,linewidth = 1,colour = "red", linetype = "twodash")+
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
# one_pulse_response_plot
# # ggsave("plots/one_pulse_response_plot.png",plot=one_pulse_response_plot,scale=1,dpi=600)
# ggsave("plots/one_pulse_response_plot.pdf",plot=one_pulse_response_plot,scale=1,dpi=600)

############################################################################################################################
# Chemostat and no pulse response

chemostat_plot <- ggplot()+
  geom_abline(slope = 0,
              intercept = 20,
              linewidth = 1) +
  geom_abline(slope = 0,
              intercept = 35,
              linewidth = 1,
              linetype = "dotdash") +
  labs(x = "Time (days)",
       y = "Resource quantity") +
  xlim(0,365) +
  ylim(0, 50) +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
 theme(axis.text.y = element_blank(),
       axis.ticks.y = element_blank(),
       axis.text.x = element_blank(),
       axis.ticks.x = element_blank())
chemostat_plot
# ggsave("plots/chemostat_plot.png",plot=chemostat_plot,scale=1,dpi=600)
# ggsave("plots/chemostat_plot.pdf",plot=chemostat_plot,scale=1,dpi=600)

# logistic curve? Will try doing rnorm into cumulative frequency
# chemo_sp1 <- data.frame(values = rnorm(10000, mean = 150, sd = 30),
#                            species = "a")
# chemo_sp2 <- data.frame(values = rnorm(10000, mean = 80, sd = 30),
#                            species = "b")
# 
# chemostat_combined <- rbind(chemo_sp1,chemo_sp2)
# 
# chemostat_response_concept <- chemostat_combined %>%
#   ggplot(aes(x = values,
#              colour = species)) +
#   stat_ecdf(linewidth = 1)+
#   xlim(0,365) +
#   ylim(0, 1.25) +
#   theme_few() +
#   theme(axis.title = element_text(size = 12,face = "bold",),
#         legend.title = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 10)) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# chemostat_response_concept

# This doesn't work because both species terminate at the same end value
# find a new way to make logistic curves

# From https://library.virginia.edu/data/articles/simulating-a-logistic-regression-model ??
# using the logistic formula y = L/(1+e^(-k(x-x0))))

data1 <- data.frame(x = c(1:100)) %>%
  mutate(category = "sp_a")%>%
  mutate(y = 50/(1+exp(-0.3*(x-10))))
data2 <- data.frame(x = c(1:100)) %>%
  mutate(category = "sp_b")%>%
  mutate(y = 40/(1+exp(-0.3*(x-10))))
ggplot(data1,aes(x=x,y=y))+geom_line() #proof of concept
ggplot(data2,aes(x=x,y=y))+geom_line()
datacomb <- rbind(data1,data2)       
logistic_plot <- datacomb %>%
  ggplot(aes(x = x,
             y = y,
             colour = category)) +
  geom_line(linewidth = 1) +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "none") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(x = "Time (days)",
       y = "Abundance") +
  scale_colour_manual(values = c("sp_a" = "black",
                                 "sp_b" = "red"))
logistic_plot
# ggsave("plots/logisticAbundance.png",plot=logistic_plot,scale=1,dpi=600)
# ggsave("plots/logisticAbundance.pdf",plot=logistic_plot,scale=1,dpi=600)

#############################################################################
# 4/10/2023 plots 

# Changing the multiple pulse response plot so that all the plants have the same distribution just with different standard deviations? same mean
pulse_new1 <- data.frame(value = rnorm(1000000, mean = 25, sd = 15),
                        species = "a")
pulse_new2 <- data.frame(value = rnorm(1000000, mean = 85, sd = 20),
                        species = "b")
pulse_new3 <- data.frame(value = rnorm(1000000, mean = 50, sd = 30),
                        species = "c")
pulse_new4 <- data.frame(value = rnorm(1000000, mean = 125, sd = 40),
                        species = "d")
pulse_new5 <- data.frame(value = rnorm(1000000, mean = 165, sd = 50),
                        species = "e")

response_new <- rbind(pulse_new1,pulse_new2,pulse_new3,pulse_new4,pulse_new5)

one_pulse_response_new <- response_new %>%
  ggplot(aes(x=value,colour=species))+
  geom_density(linewidth=1)+
  theme_few()+
  labs(x = "Time (days)",
       y = "Abundance") +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("a" = "#E69F00",
                                 "b" = "#56B4E9",
                                 "c" = "#CC79A7",
                                 "d" = "#009E73",
                                 "e" = "#F0E442"),
                      breaks = c("a","b","c","d","e"))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  xlim(-50,365)
one_pulse_response_new
# ggsave("plots/one_pulse_response_new3.png",plot=one_pulse_response_new,scale=1,dpi=600)
# ggsave("plots/one_pulse_response_new3.pdf",plot=one_pulse_response_new,scale=1,dpi=600)

# fixing the one pulse concept resource
x <- seq(0, 20, length.out=1000)
resource <- data.frame(x=x, px=dexp(x, rate=0.65))
resource[1,2] <- 0

# Lizzie's example plot
# plot(dat$px ~ x, type="l", ylab="imaginary resource", xlab="time")

onepulse_plot <- resource %>%
  ggplot(aes(x = x,
             y = px)) +
  geom_line(linewidth = 1) +
  theme_few()+
  labs(y = "Resource quantity",
       x = "Time (days)") +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
onepulse_plot
# ggsave("plots/one_pulse_resource.png",plot=onepulse_plot,scale=1,dpi=600)
# ggsave("plots/one_pulse_resource.pdf",plot=onepulse_plot,scale=1,dpi=600)

# plotting a basic rnorm curve that can be edited and morphed into different heights and widths for the other concept plots
normcurve_plot <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1),size = 1) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  theme_few()
normcurve_plot
ggsave("plots/normcurve_plot.pdf",plot=normcurve_plot,scale=1,dpi=600)
                        