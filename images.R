
# scaled-plots ------------------------------------------------------------


min_max_function <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}


banco %>%
  filter(!is.na(intensidade_dor)) %>%
  select(pitt_escore, isi_escore, ole_cat, intensidade_dor) %>%
  mutate(
    ISI = min_max_function(isi_escore),
    PSQI = min_max_function(pitt_escore),
    intensidade_dor = fct_relevel(intensidade_dor,
                                  c("No pain", "Mild pain", "Moderate pain", "Severe pain")
    )
  ) %>%
  pivot_longer(cols = ISI:PSQI,
               names_to = "Questionnaire",
               values_to = "Score") %>%        # scaled score
  ggplot(aes(intensidade_dor, Score,
             fill= ole_cat
  ))+
  geom_boxplot()+
  facet_wrap(~ Questionnaire)+
  ylab("Scaled score")+
  xlab("Pain intensity")+
  labs(fill = "PBS")


# boxplot com Kruskal-Wallis ----------------------------------------------


library(ggpubr)


pain_classes <- list(c("No pain", "Mild pain"), c("Mild pain", "Moderate pain"),
                     c("Moderate pain", "Severe pain"))

# compare_means(isi_escore ~ int_dor_pelve, data = banco)


# Sem legenda -------------------------------------------------------------


banco %>%
  group_by(int_dor_pelve) %>%
  mutate(mediana = median(isi_escore)) %>%
  ggboxplot(x = "int_dor_pelve", y = "isi_escore",
            title = 'Figure 1',
            ylab = "ISI SCORE", xlab = "Pelvic pain",
            fill = "int_dor_pelve", palette = "Paired",
            # add = "jitter",
  )+
  labs(fill = "Pelvic pain")+
  annotate("text", x = 1, y = 34, label= "Kruskal-Wallis, P-value < 0.001", size = 3)+
  stat_compare_means( comparisons = pain_classes , label.y = c(27,29, 27),                       digits = c(3,3,3),
                      label = "p.signif")+
  geom_hline(yintercept = 14, linetype = 5)+
  geom_text(aes(label = paste0("Median = ", mediana),
                y = mediana, vjust = -.6)) +
  theme(text = element_text(size = 14), legend.position = "none")

ggsave("Figure_1.jpg", width = 20, height = 14, units = c("cm"), dpi =300)



# Com legenda -------------------------------------------------------------

banco %>%
  group_by(int_dor_pelve) %>%
  mutate(mediana = median(isi_escore)) %>%
  ggboxplot(x = "int_dor_pelve", y = "isi_escore",
            title = 'Figure 1',
            ylab = "ISI SCORE", xlab = "Pelvic pain",
            fill = "int_dor_pelve", palette = "Paired",
            # add = "jitter",
  )+
  labs(fill = "Pelvic pain")+
  annotate("text", x = 1, y = 34, label= "Kruskal-Wallis, P-value < 0.001", size = 3)+
  stat_compare_means( comparisons = pain_classes , label.y = c(27,29, 27),                       digits = c(3,3,3),
                      label = "p.signif")+
  geom_hline(yintercept = 14, linetype = 5)+
  geom_text(aes(label = paste0("Median = ", mediana),
                y = mediana, vjust = -.6)) +
  theme(text = element_text(size = 14))

ggsave("Figure_1_leg.jpg", width = 20, height = 14, units = c("cm"), dpi =300)

