# Figure_supp_1
# SAA data plot to ID outlier to remove

# Import data as 'data'

# create labels for any horse with SAA >1000 at time = 0
labels_p_supp_1 <- data %>% filter(saa > 1000 & hrs == 0)

p_supp_1 <- ggplot(data, aes(hrs, saa)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'Time(hrs)', y = 'Serum Amyloid A (mcg/mL)',
       title = 'Plot of Serum Amyloid A over time') +
  theme_bw() +
  xlim(0, 72) + ylim(0, 3100)
print(p_supp_1)

# Figure_supp_2
# Cytokine data plot to ID outlier

# create labels for any horse with SAA >1000 at time = 0
labels_p_supp_2 <- data %>% filter(Eotaxin > 30 & hrs == 0)

p <- ggplot(data, aes(hrs, Eotaxin)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p2 <- ggplot(data, aes(hrs, G.CSF)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p3 <- ggplot(data, aes(hrs, IL1a)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p4 <- ggplot(data, aes(hrs, IL.5)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p5 <- ggplot(data, aes(hrs, IL.18)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p6 <- ggplot(data, aes(hrs, IL.6)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p7 <- ggplot(data, aes(hrs, IL.17A)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p8 <- ggplot(data, aes(hrs, IL.2)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p9 <- ggplot(data, aes(hrs, IL.4)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p10 <- ggplot(data, aes(hrs, IFNg)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p11 <- ggplot(data, aes(hrs, IL.8)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p12 <- ggplot(data, aes(hrs, IP.10)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p13 <- ggplot(data, aes(hrs, IL.10)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()
p14 <- ggplot(data, aes(hrs, TNFa)) +
  geom_line(aes(group = ID)) +
  geom_text_repel(data = labels_p_supp_2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  labs(x = 'time(hrs)') +
  theme_bw()

plot_list <- list(p, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)

p_supp_2 <- patchwork::wrap_plots(plot_list, nrow=4, guides='collect')
print(p_supp_2)

#Figure 2
# plot saa without horse AB and Myla as outliers
# remove 2 horses
data2 <- data %>% filter(ID != 'AB' & ID != 'Myla')

# summarize data
saa_plot_data_overall <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'saa', groupvars = c('hrs'))

# set up jitter width for consistency
pd <- position_dodge(width = 3)

#Create plot
figure_2 <- ggplot(data = saa_plot_data_overall, aes(hrs,saa)) +
  geom_line() +
  geom_errorbar(aes(ymin=saa-ci, ymax=saa+ci), width=2) +
  geom_point(data = data2, aes(shape = group), size = 2.5, position = pd) +
  labs(x = 'Time(hrs)', y = 'Serum Amyloid A (mcg/mL)',
       title = 'Plot of Serum Amyloid A over time') +
  annotate(geom = 'text', x = c(48, 72), y = c(3000, 2900),
           label = '*', size = 10) +
  theme_bw()
print(figure_2)

# Figure 3
# SAA by group
# create summary data for plot
saa_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'saa', groupvars = c('group', 'hrs'))

# create plot
figure_3 <- ggplot(data = saa_plot_data, aes(x = hrs, y = saa, group = group)) +
  geom_errorbar(aes(ymin=saa-ci, ymax=saa+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape=group), size = 2.5, position = pd) +
  annotate(geom = 'text', x = c(48, 72), y = c(2450, 2350), label = '*', size = 10) +
  theme_bw() +
  labs(x = 'Time(hrs)', y = 'Serum Amyloid A (mcg/mL)',
       title = 'Plot of Serum Amyloid A over time')
print(figure_3)

# figure 4
# plot of 3 cytokines over time

# Eotaxin plot
eotaxin_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'Eotaxin', groupvars = c('group', 'hrs'))

# plot
figure_4a <- ggplot(data = eotaxin_plot_data, aes(x = hrs, y = Eotaxin, group = group)) +
  geom_errorbar(aes(ymin=Eotaxin-ci, ymax=Eotaxin+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  theme_bw()
print(figure_4a)

#gcsf plot
# summarize data
gcsf_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'G.CSF', groupvars = c('group', 'hrs'))

# plot
figure_4b <- ggplot(data = gcsf_plot_data, aes(x = hrs, y = G.CSF, group = group)) +
  geom_errorbar(aes(ymin=G.CSF-ci, ymax=G.CSF+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  theme_bw()
print(figure_4b)

# IL-8 plot
IL8_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'IL.8', groupvars = c('group', 'hrs'))

# plot
figure_4c <- ggplot(data = IL8_plot_data, aes(x = hrs, y = IL.8, group = group)) +
  geom_errorbar(aes(ymin=IL.8-se, ymax=IL.8+se), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
#  annotate(geom = 'text', x = c(0, 0.5, 1, 2, 4, 8, 24),
#           y = c(275, 300, 325, 350, 350, 350, 350), label = '*', size = 10) +
  theme_bw()
print(figure_4c)

# now combine the three plots (4a, 4b, 4c)
figure_4 <- patchwork::wrap_plots(figure_4a, figure_4b, figure_4c,
                                  nrow = 1, guides = "collect")
print(figure_4)
