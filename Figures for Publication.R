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