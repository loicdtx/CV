library(ggplot2)

# Programming skills
df <- data.frame(language = c('python', 'R', 'bash', 'JS'),
                 competence = c(40, 40, 10, 10))

ggplot(df, aes(x = "", y = competence, fill = factor(language))) +
  geom_bar(width = 1, stat="identity", color = 'white') +
  coord_polar(theta = "y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.position="none")

ggsave('step/pieProgramming.svg', width = 5, height = 5)


# Personnal
df2 <- data.frame(skills = c('empathy', 'curiosity', 'facilitator', 'manage', 'selfLearner'),
                  values = rep(20, 5))

ggplot(df2, aes(x = "", y = values, fill = factor(skills))) +
  geom_bar(width = 1, stat="identity", color = 'white') +
  coord_polar(theta = "y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.title=element_blank(),
        legend.position="none")

ggsave('step/piePersonnal.svg', width = 5, height = 5)