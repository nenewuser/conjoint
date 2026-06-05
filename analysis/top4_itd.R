library(readxl)
library(dplyr)
library(ggplot2)
library(estimatr)
library(broom)
library(stringr)

d <- read_excel("long_df_SA.xlsx")
d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)

ATTRS <- c("region", "motivation", "education", "employer", "gender",
           "age", "occupation", "language", "politics", "appearance")

GROUP_LABELS <- c(region = "Регион", motivation = "Мотивация",
                  education = "Образование", employer = "Работодатель",
                  gender = "Пол", age = "Возраст", occupation = "Профессия",
                  language = "Язык", politics = "Политика страны",
                  appearance = "Внешний вид")

d <- d %>% mutate(across(all_of(ATTRS), as.factor))

ref_politics <- "Страна помогает России обходить санкции (параллельный экспорт)"
if (ref_politics %in% levels(d$politics)) {
  d$politics <- relevel(d$politics, ref = ref_politics)
}

ref_region <- "Восточная Европа (Беларусь, Молдова, Украина)"
if (ref_region %in% levels(d$region)) {
  d$region <- relevel(d$region, ref = ref_region)
}

age_numeric <- suppressWarnings(as.numeric(as.character(d$age)))
if (all(!is.na(age_numeric))) {
  d$age <- factor(d$age, levels = as.character(sort(unique(age_numeric))))
}

dir.create("Plots", showWarnings = FALSE)

f_main <- as.formula(paste("outcome ~", paste(ATTRS, collapse = " + ")))
m <- lm_robust(f_main, data = as.data.frame(d),
               clusters = respondent_id, se_type = "stata")

est <- tidy(m) %>% filter(term != "(Intercept)")

attr_of_term <- function(term) {
  hit <- ATTRS[sapply(ATTRS, function(a) startsWith(term, a))]
  if (length(hit) == 0) return(NA_character_)
  hit[which.max(nchar(hit))]
}

est$attr <- sapply(est$term, attr_of_term)
est <- est %>% filter(!is.na(attr))
est$level <- mapply(function(t, a) sub(paste0("^", a), "", t),
                    est$term, est$attr)
est$group <- GROUP_LABELS[est$attr]
est$label <- paste0(est$group, ": ", est$level)

top_pos <- est %>% arrange(desc(estimate)) %>% slice_head(n = 4) %>%
  mutate(side = "Топ-4 наиболее привлекательных")
top_neg <- est %>% arrange(estimate) %>% slice_head(n = 4) %>%
  mutate(side = "Топ-4 наименее привлекательных")

top <- bind_rows(top_neg, top_pos) %>%
  arrange(estimate) %>%
  mutate(label = factor(label, levels = unique(label)),
         side = factor(side, levels = c("Топ-4 наименее привлекательных",
                                        "Топ-4 наиболее привлекательных")))

p <- ggplot(top, aes(estimate, label)) +
  facet_grid(side ~ ., scales = "free_y", space = "free_y", switch = "y") +
  geom_col(fill = "grey70", color = "grey30", width = 0.7) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.25, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous("Эффект на Pr(выбран)",
                     labels = function(z) sprintf("%.2f", z)) +
  scale_y_discrete(NULL, labels = function(z) str_wrap(z, width = 40)) +
  labs(title = "Наиболее и наименее привлекательные характеристики мигрантов",
       subtitle = "AMCE из основной модели (LPM)") +
  theme_bw(10) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = 10, color = "grey20",
                                     margin = margin(b = 6)))

ggsave("Plots/top_attributes_main.png", p, width = 10, height = 6, dpi = 300)
try(ggsave("Plots/top_attributes_main.pdf", p, width = 10, height = 6,
           device = cairo_pdf), silent = TRUE)

# === Парадокс региона × политики: доли выбора в 2×2 ===
WE <- "Западная Европа (Германия, Италия, Франция)"
SANC <- "Страна присоединилась к санкциям против России"

d2 <- d %>%
  mutate(we = ifelse(region == WE, "Зап. Европа", "Не Зап. Европа"),
         sanc = ifelse(politics == SANC, "Санкции", "Другая политика"),
         combo = factor(paste(we, sanc, sep = " | "),
                        levels = c("Зап. Европа | Санкции",
                                   "Зап. Европа | Другая политика",
                                   "Не Зап. Европа | Санкции",
                                   "Не Зап. Европа | Другая политика")))

m_combo <- lm_robust(outcome ~ 0 + combo, data = as.data.frame(d2),
                     clusters = respondent_id, se_type = "stata")

shares <- tidy(m_combo) %>%
  mutate(label = factor(sub("^combo", "", term),
                        levels = levels(d2$combo)),
         n = sapply(label, function(l) sum(d2$combo == l)))

p_shares <- ggplot(shares, aes(label, estimate)) +
  geom_col(fill = "grey70", color = "grey30", width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0.5, color = "grey50", linetype = "dashed") +
  geom_text(aes(label = sprintf("N = %d", n), y = 0.02),
            size = 3, color = "grey30") +
  scale_x_discrete(NULL, labels = function(z) str_wrap(z, width = 18)) +
  scale_y_continuous("Доля выбранных профилей",
                     labels = function(z) sprintf("%.2f", z),
                     limits = c(0, NA)) +
  labs(title = "Доли выбора: регион × политика страны",
       subtitle = "Пунктир на 0.5 — нейтральная вероятность выбора в паре") +
  theme_bw(11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10, color = "grey20",
                                     margin = margin(b = 6)))

ggsave("Plots/shares_we_sanctions.png", p_shares, width = 9, height = 5, dpi = 300)
try(ggsave("Plots/shares_we_sanctions.pdf", p_shares, width = 9, height = 5,
           device = cairo_pdf), silent = TRUE)


mm_rows <- list()
for (a in ATTRS) {
  m_mm <- lm_robust(as.formula(paste("outcome ~ 0 +", a)),
                    data = as.data.frame(d),
                    clusters = respondent_id, se_type = "stata")
  tt <- tidy(m_mm)
  tt$level <- sub(paste0("^", a), "", tt$term)
  tt$attr <- a
  tt$n <- sapply(tt$level, function(l) sum(d[[a]] == l))
  mm_rows[[a]] <- tt
}
mm <- bind_rows(mm_rows) %>%
  mutate(group = GROUP_LABELS[attr],
         label = paste0(group, ": ", level))

mm_top <- mm %>% arrange(desc(estimate)) %>% slice_head(n = 4) %>%
  mutate(side = "Топ-4 наиболее привлекательных")
mm_bot <- mm %>% arrange(estimate) %>% slice_head(n = 4) %>%
  mutate(side = "Топ-4 наименее привлекательных")

mm_top_bot <- bind_rows(mm_bot, mm_top) %>%
  arrange(estimate) %>%
  mutate(label = factor(label, levels = unique(label)),
         side = factor(side, levels = c("Топ-4 наименее привлекательных",
                                        "Топ-4 наиболее привлекательных")))

p_mm <- ggplot(mm_top_bot, aes(estimate, label)) +
  facet_grid(side ~ ., scales = "free_y", space = "free_y", switch = "y") +
  geom_col(fill = "grey70", color = "grey30", width = 0.7) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.25, color = "black") +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_x_continuous("Доля выбора профилей с этим уровнем",
                     labels = function(z) sprintf("%.2f", z),
                     limits = c(0, NA)) +
  scale_y_discrete(NULL, labels = function(z) str_wrap(z, width = 40)) +
  labs(title = "Наиболее и наименее привлекательные характеристики мигрантов",
       subtitle = "Доли выбора. Пунктир на 0.5 - нейтральная вероятность выбора в паре") +
  theme_bw(8) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = 9, color = "grey20",
                                     margin = margin(b = 6)))

ggsave("Plots/top_shares_main.png", p_mm, width = 10, height = 6, dpi = 300)
try(ggsave("Plots/top_shares_main.pdf", p_mm, width = 10, height = 6,
           device = cairo_pdf), silent = TRUE)