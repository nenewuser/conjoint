rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(estimatr)
library(marginaleffects)
library(stringr)
library(patchwork)
library(ragg)

d <- read_excel("long_df_SA.xlsx")
d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)

ATTRS <- c("region", "motivation", "education", "employer", "gender",
           "age", "occupation", "language", "politics", "appearance")
d <- d %>% mutate(across(all_of(ATTRS), as.factor))

REFS <- list(
  region = "Восточная Европа (Беларусь, Молдова, Украина)",
  motivation = "Поиск работы",
  education = "Среднее (школа)",
  employer = "Небольшая частная компания",
  gender = "Мужчина",
  occupation = "Сфера услуг (общепит)",
  language = "Говорит плохо",
  politics = "Страна помогает России обходить санкции (параллельный экспорт)",
  appearance = "В повседневной жизни носит обычную светскую одежду"
)
for (v in names(REFS)) {
  if (REFS[[v]] %in% levels(d[[v]])) d[[v]] <- relevel(d[[v]], ref = REFS[[v]])
}
d$age <- factor(d$age, levels = sort(unique(as.numeric(as.character(d$age)))))

INDEXES <- c("identity_civic_index", "identity_ethnic_index", "trust_index",
             "authoritar_index", "contact_index", "valence_index")
INDEXES <- INDEXES[INDEXES %in% names(d)]
for (v in INDEXES) {
  d[[v]] <- as.numeric(d[[v]])
  d[[paste0(v, "_c")]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
}

if ("Supporta" %in% names(d)) d$Supporta <- as.numeric(as.character(d$Supporta))

dir.create("plots_png", showWarnings = FALSE)


clean_contrast <- function(z) {
  out <- sub("\\s*-\\s*[^-]+$", "", z)
  str_wrap(out, width = 35)
}

base_theme <- theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 10,
                                     margin = margin(b = 6)),
        legend.position = "none",
        strip.background = element_rect(fill = "grey92"),
        strip.text = element_text(face = "bold", size = 9,
                                  lineheight = 0.9),
        panel.grid.minor = element_blank())


mod_plot <- function(model, variable, condition, title, x_label,
                     base_label, ncol = 2) {
  plot_comparisons(model, variables = variable, condition = condition) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ contrast, ncol = ncol,
               labeller = as_labeller(clean_contrast)) +
    labs(title = title,
         subtitle = paste0("Сравнения относительно базовой категории — «",
                           base_label, "», 95% ДИ"),
         x = x_label,
         y = "Эффект на вероятность выбора") +
    base_theme
}

# 1. Этническая идентичность * регион
model_identity <- lm_robust(
  outcome ~ region * identity_ethnic_index_c +
    region * identity_civic_index_c +
    motivation + education + employer + gender + age +
    occupation + language + politics + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_identity_ethnic <- mod_plot(
  model_identity, "region", "identity_ethnic_index_c",
  title = "Предельные эффекты региона при разных значениях этнической идентичности",
  x_label = "Этническая идентичность, центрированный индекс",
  base_label = REFS$region)

# 2. Гражданская идентичность * регион
p_identity_civic <- mod_plot(
  model_identity, "region", "identity_civic_index_c",
  title = "Предельные эффекты региона при разных значениях гражданской идентичности",
  x_label = "Гражданская идентичность, центрированный индекс",
  base_label = REFS$region)

# 3. Доверие × политика
model_trust <- lm_robust(
  outcome ~ region + motivation + education + employer + gender + age +
    occupation + language + politics * trust_index_c + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_trust_c <- mod_plot(
  model_trust, "politics", "trust_index_c",
  title = "Предельные эффекты политики страны при разных значениях доверия",
  x_label = "Институциональное доверие, центрированный индекс",
  base_label = REFS$politics)

# 4. Авторитаризм * регион
model_authoritar <- lm_robust(
  outcome ~ region * authoritar_index_c + motivation + education + employer +
    gender + age + occupation + language + politics + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_author_c <- mod_plot(
  model_authoritar, "region", "authoritar_index_c",
  title = "Предельные эффекты региона при разных значениях авторитарных установок",
  x_label = "Авторитарные установки, центрированный индекс",
  base_label = REFS$region)

# 5. Контакт * внешний вид
model_contact <- lm_robust(
  outcome ~ region + motivation + education + employer + gender + age +
    occupation + language + politics + appearance * contact_index_c,
  data = d, clusters = respondent_id, se_type = "stata")

p_contact_c <- mod_plot(
  model_contact, "appearance", "contact_index_c",
  title = "Предельный эффект внешнего вида при разных значениях частоты контакта",
  x_label = "Частота контакта, центрированный индекс",
  base_label = REFS$appearance)

# 6. Валентность * внешний вид
model_valence <- lm_robust(
  outcome ~ region + motivation + education + employer + gender + age +
    occupation + language + politics + appearance * valence_index_c,
  data = d, clusters = respondent_id, se_type = "stata")

p_valence_c <- mod_plot(
  model_valence, "appearance", "valence_index_c",
  title = "Предельный эффект внешнего вида при разных значениях валентности контакта",
  x_label = "Валентность контакта, центрированный индекс",
  base_label = REFS$appearance)

# 7. Поддержка власти * политика
model_sup <- lm_robust(
  outcome ~ region + motivation + education + employer + gender + age +
    occupation + language + politics * Supporta + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_support <- mod_plot(
  model_sup, "politics",
  condition = list(Supporta = seq(min(d$Supporta, na.rm = TRUE),
                                  max(d$Supporta, na.rm = TRUE),
                                  length.out = 50)),
  title = "Предельные эффекты политики страны при разных значениях поддержки власти",
  x_label = "Уровень поддержки руководства страны",
  base_label = REFS$politics)

# 8. Образование респондента * образование мигранта
model_educ <- lm_robust(
  outcome ~ region + motivation + education * Education_resp + employer +
    gender + age + occupation + language + politics * Supporta + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_educ_resp <- mod_plot(
  model_educ, "education", "Education_resp",
  title = "Предельные эффекты образования мигранта при разных уровнях образования респондента",
  x_label = "Уровень образования респондента",
  base_label = REFS$education)


WE_LEVEL <- "Западная Европа (Германия, Италия, Франция)"

p_author_we <- plot_comparisons(
  model_authoritar,
  variables = list(region = c(REFS$region, WE_LEVEL)),
  condition = "authoritar_index_c") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(title = "Зап. Европа против Восточной Европы",
       x = "Авторитарные установки, центрированный индекс",
       y = "Эффект на вероятность выбора") +
  base_theme +
  theme(strip.text = element_blank())

model_polauthor <- lm_robust(
  outcome ~ politics * authoritar_index_c + region + motivation + education +
    employer + gender + age + occupation + language + appearance,
  data = d, clusters = respondent_id, se_type = "stata")

p_polauthor <- plot_comparisons(
  model_polauthor, variables = "politics",
  condition = "authoritar_index_c") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ contrast, ncol = 3,
             labeller = as_labeller(clean_contrast)) +
  labs(title = "Политика страны (vs параллельный экспорт)",
       x = "Авторитарные установки, центрированный индекс",
       y = "Эффект на вероятность выбора") +
  base_theme

p_author_combo <- p_author_we / p_polauthor +
  plot_layout(heights = c(1, 1.1)) +
  plot_annotation(
    title = "Авторитарные установки как модератор",
    subtitle = paste0(
      "Сверху: эффект «Зап. Европа» против базы «", REFS$region, "»\n",
      "Снизу: эффект каждой политики против базы «", REFS$politics, "». 95% ДИ"),
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  plot.subtitle = element_text(colour = "grey30", size = 9,
                                               lineheight = 1.1,
                                               margin = margin(b = 8))))

save_png <- function(plot_object, file_name, width = 14, height = 9, dpi = 600) {
  ggsave(file.path("plots_png", paste0(file_name, ".png")),
         plot = plot_object, width = width, height = height, dpi = dpi,
         units = "in", bg = "white", device = ragg::agg_png)
}

save_png(p_identity_ethnic, "01_identity_ethnic_region", 15, 9)
save_png(p_identity_civic, "02_identity_civic_region", 15, 9)
save_png(p_trust_c, "03_trust_politics", 14, 8)
save_png(p_author_c, "04_authoritarian_region", 15, 9)
save_png(p_contact_c, "05_contact_appearance", 12, 7)
save_png(p_valence_c, "06_valence_appearance", 12, 7)
save_png(p_support, "07_support_politics", 14, 8)
save_png(p_educ_resp, "08_education_resp", 14, 8)
save_png(p_author_combo, "09_authoritarian_we_and_politics", 12, 10)