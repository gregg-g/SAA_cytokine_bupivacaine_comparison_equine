library(ggrepel)
  # add separator for new controls and older data
data$time <- ifelse(data$ID == 'Vesunna'|
                      data$ID == 'Rita'|
                      data$ID == 'Myla',
                                      'new', 'old') %>% factor()
str(data)  
  #test plot
labels1 <- data %>% filter(time == 'new' & hrs == 72)
labels2 <- data %>% filter(ID == 'Rita' & hrs == 48)
  # create separately d/t Rita data ending at 48 hrs

p_saa <- ggplot(data, aes(hrs, saa)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
print(p_saa)

p <- ggplot(data, aes(hrs, Eotaxin)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
# works as expected
p2 <- ggplot(data, aes(hrs, G.CSF)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p3 <- ggplot(data, aes(hrs, IL1a)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p4 <- ggplot(data, aes(hrs, IL.5)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p5 <- ggplot(data, aes(hrs, IL.18)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p6 <- ggplot(data, aes(hrs, IL.6)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p7 <- ggplot(data, aes(hrs, IL.17A)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p8 <- ggplot(data, aes(hrs, IL.2)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p9 <- ggplot(data, aes(hrs, IL.4)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p10 <- ggplot(data, aes(hrs, IFNg)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p11 <- ggplot(data, aes(hrs, IL.8)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p12 <- ggplot(data, aes(hrs, IP.10)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p13 <- ggplot(data, aes(hrs, IL.10)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)
p14 <- ggplot(data, aes(hrs, TNFa)) +
  geom_line(aes(group = ID, color = time)) +
  geom_text_repel(data = labels1, aes(label = ID),
                  fontface ="plain", color = "black", size = 3) +
  geom_text_repel(data = labels2, aes(label = ID),
                  fontface ="plain", color = "black", size = 3)

plot_list <- list(p, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
#cols_to_plot <- c("Eotaxin", "G.CSF", "IL1a", "IL.5", "IL.18",
#                  "IL.6", "IL.17A", "IL.2", "IL.4", "IFNg",
#                  "IL.8", "IP.10", "IL.10", "TNFa")
#plot_list <- list()
#for (col_name in cols_to_plot) {
#                             p <- ggplot(data, aes_string(x=data$hrs, y=col_name)) +
#                               geom_line(aes(color=time, group = ID)) +
#                             geom_text_repel(data = labels1, aes(label = ID),
#                                                 fontface ="plain", color = "black", size = 3) +
#                               geom_text_repel(data = labels2, aes(label = ID),
#                                               fontface ="plain", color = "black", size = 3)
#                             plot_list[[col_name]] <- p
#                             print(p)
#                           }

combined_plot <- patchwork::wrap_plots(plot_list, nrow=4, guides='collect')
print(combined_plot)

#check by group
plot_list2 <- list()
for (col_name in cols_to_plot) {
  p <- ggplot(data, aes_string(x=data$hrs, y=col_name)) +
    geom_line(aes(color=group, group = ID))
  plot_list2[[col_name]] <- p
}

combined_plot2 <- patchwork::wrap_plots(plot_list2, nrow=4, guides='collect')
print(combined_plot2)

#plot additional controls without 2 outliers (AB and Myla)
plot_list3 <- list()
for (col_name in cols_to_plot) {
  p <- ggplot(data2, aes_string(x=data2$hrs, y=col_name)) +
    geom_point(aes(color=time)) +
    geom_line(aes(color=time, group = ID))
  plot_list3[[col_name]] <- p
}

combined_plot3 <- patchwork::wrap_plots(plot_list3, nrow=4, guides='collect')
print(combined_plot3)

# plot all saa data
plot1 <- ggplot(data, aes(hrs, saa))
plot1 + geom_jitter(aes(color = group)) +
  geom_smooth(aes(color = group), se = FALSE) +
  geom_line(aes(group = ID))

# plot saa without horse AB and Myla as outliers
plot2 <- data %>% filter(ID != 'AB' & ID != 'Myla') %>% ggplot(aes(hrs, saa))
plot2 + geom_jitter(aes(shape = group), width = 2) +
  geom_line(data = saa_plot_data, aes(hrs))

# Better plot to evaluate groups
# summarize data
saa_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'saa', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot saa
saa_plot <- ggplot(data = saa_plot_data, aes(x = hrs, y = saa, group = group)) +
  geom_errorbar(aes(ymin=saa-ci, ymax=saa+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  annotate(geom = 'text', x = c(48, 72), y = c(2450, 2350), label = '*', size = 10)
print(saa_plot)

# Eotaxin plot
eotaxin_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'Eotaxin', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
eotaxin_plot <- ggplot(data = eotaxin_plot_data, aes(x = hrs, y = Eotaxin, group = group)) +
  geom_errorbar(aes(ymin=Eotaxin-ci, ymax=Eotaxin+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) 
  #annotate(geom = 'text', x = c(48, 72), y = c(2450, 2350), label = '*', size = 10)
print(eotaxin_plot)


gcsf_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'G.CSF', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
gcsf_plot <- ggplot(data = gcsf_plot_data, aes(x = hrs, y = G.CSF, group = group)) +
  geom_errorbar(aes(ymin=G.CSF-ci, ymax=G.CSF+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd)
  #annotate(geom = 'text', x = c(48, 72), y = c(2450, 2350), label = '*', size = 10)
print(gcsf_plot)


IL18_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'IL.18', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
IL18_plot <- ggplot(data = IL18_plot_data, aes(x = hrs, y = IL.18, group = group)) +
  geom_errorbar(aes(ymin=IL.18-ci, ymax=IL.18+ci), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  annotate(geom = 'text', x = 72, y = 280, label = '*', size = 10)
#print(IL18_plot)
IL18_raw <- ggplot(data2, aes(hrs, IL.18))
IL18_raw_plot <- IL18_raw + geom_jitter(aes(color=group)) +
  geom_smooth(aes(color=group), se=FALSE) +
  geom_line(aes(group=ID))

IL6_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'IL.6', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
IL6_plot <- ggplot(data = IL6_plot_data, aes(x = hrs, y = IL.6, group = group)) +
  geom_errorbar(aes(ymin=IL.6-se, ymax=IL.6+se), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  annotate(geom = 'text', x = 72, y = 125, label = '*', size = 10)
#print(IL6_plot)
IL6_raw <- ggplot(data2, aes(hrs, IL.6))
IL6_raw_plot <- IL6_raw + geom_jitter(aes(color=group)) +
  geom_smooth(aes(color=group), se=FALSE) +
  geom_line(aes(group=ID))

IL2_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'IL.2', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
IL2_plot <- ggplot(data = IL2_plot_data, aes(x = hrs, y = IL.2, group = group)) +
  geom_errorbar(aes(ymin=IL.2-se, ymax=IL.2+se), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  annotate(geom = 'text', x = 72, y = 110, label = '*', size = 10)
#print(IL2_plot)
IL2_raw <- ggplot(data2, aes(hrs, IL.2))
IL2_raw_plot <- IL2_raw + geom_jitter(aes(color=group)) +
  geom_smooth(aes(color=group), se=FALSE) +
  geom_line(aes(group=ID))

IL.2.6.18_plot <- patchwork::wrap_plots(IL2_plot, IL6_plot, IL18_plot,
                                        nrow=2, guides='collect')
print(IL.2.6.18_plot)

IL.2.6.18_raw_plot <- patchwork::wrap_plots(IL2_raw_plot, IL6_raw_plot, IL18_raw_plot,
                                            nrow=2, guides='collect')
print(IL.2.6.18_raw_plot)

IL8_plot_data <- data %>%
  filter(ID != 'AB' & ID != 'Myla') %>%
  summarySE(measurevar = 'IL.8', groupvars = c('group', 'hrs'))
# set jitter
pd <- position_dodge(3)
# plot
IL8_plot <- ggplot(data = IL8_plot_data, aes(x = hrs, y = IL.8, group = group)) +
  geom_errorbar(aes(ymin=IL.8-se, ymax=IL.8+se), width=2, position=pd) +
  geom_line(aes(linetype = group), position=pd) +
  geom_point(aes(shape = group), size = 3, position=pd) +
  annotate(geom = 'text', x = c(0, 0.5, 1, 2, 4, 8, 24), y = c(275, 300, 325, 350, 350, 350, 350), label = '*', size = 10)
print(IL8_plot)

# playing around with IL8 data
p_il8 <- ggplot(data2, aes(hrs, IL.8)) +
  geom_line(aes(group = ID, color = group))
print(p_il8)  
