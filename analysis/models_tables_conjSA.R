library(readxl)
library(dplyr)
library(estimatr)
library(modelsummary)
library(sandwich)
library(lubridate)

d <- read_excel("long_df_SA.xlsx")
d <- as.data.frame(d)
d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)

ATTRS <- c("region", "motivation", "education", "employer", "gender",
           "age", "occupation", "language", "politics", "appearance")

d <- d %>% mutate(across(all_of(ATTRS), as.factor))

ref_politics <- "Страна помогает России обходить санкции (параллельный экспорт)"
if (ref_politics %in% levels(d$politics)) {
  d$politics <- relevel(d$politics, ref = ref_politics)
} else {
  warning("ref-уровень '", ref_politics, "' не найден в politics, relevel пропущен")
}

ref_region <- "Восточная Европа (Беларусь, Молдова, Украина)"
if (ref_region %in% levels(d$region)) {
  d$region <- relevel(d$region, ref = ref_region)
} else {
  warning("ref-уровень '", ref_region, "' не найден в region, relevel пропущен")
}

age_numeric <- suppressWarnings(as.numeric(as.character(d$age)))
if (all(!is.na(age_numeric))) {
  d$age <- factor(d$age, levels = as.character(sort(unique(age_numeric))))
}

INDEXES <- c("identity_civic_index", "identity_ethnic_index", "trust_index",
             "authoritar_index", "contact_index", "valence_index")
INDEXES <- INDEXES[INDEXES %in% names(d)]
for (v in INDEXES) {
  d[[v]] <- as.numeric(d[[v]])
  d[[paste0(v, "_c")]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
}

if ("Supporta" %in% names(d)) d$Supporta <- as.numeric(d$Supporta)
if ("Age_resp" %in% names(d)) d$Age_resp <- as.numeric(d$Age_resp)

RESP_COVARS <- c("Income", "Locality", "Female_resp", "Education_resp")
RESP_COVARS <- RESP_COVARS[RESP_COVARS %in% names(d)]
for (v in RESP_COVARS) d[[v]] <- as.factor(d[[v]])

dir.create("Tables", showWarnings = FALSE)

if (all(c("t_insert", "t_edit") %in% names(d))) {
  ord <- c("ymd HMS", "ymd HM", "ymd", "ymd HMS z", "ymd HMS p",
           "ymd_HMS", "ymd_HM", "dmy HMS", "dmy HM", "dmy",
           "mdy HMS", "mdy HM", "mdy")
  d$t_insert <- parse_date_time(as.character(d$t_insert), orders = ord, tz = "UTC")
  d$t_edit <- parse_date_time(as.character(d$t_edit), orders = ord, tz = "UTC")
  time_df <- d %>%
    filter(!is.na(t_insert), !is.na(t_edit)) %>%
    group_by(respondent_id) %>%
    summarise(t_insert = min(t_insert, na.rm = TRUE),
              t_edit = max(t_edit, na.rm = TRUE), .groups = "drop") %>%
    mutate(duration_min = as.numeric(difftime(t_edit, t_insert, units = "mins"))) %>%
    filter(!is.na(duration_min), duration_min > 0)
  low <- quantile(time_df$duration_min, 0.50, na.rm = TRUE)
  high <- quantile(time_df$duration_min, 0.95, na.rm = TRUE)
  attentive_ids <- time_df %>%
    filter(duration_min >= low, duration_min <= high) %>% pull(respondent_id)
  d_attentive <- d %>% filter(respondent_id %in% attentive_ids)
} else {
  d_attentive <- d
}

fit_lpm <- function(formula, data) {
  lm_robust(formula, data = as.data.frame(data),
            clusters = respondent_id, se_type = "stata")
}

fit_probit <- function(formula, data) {
  data <- as.data.frame(data)
  data$.cluster_id <- data$respondent_id
  m <- glm(formula, data = data, family = binomial(link = "probit"),
           na.action = na.omit)
  attr(m, "cluster") <- data[rownames(model.frame(m)), ".cluster_id"]
  m
}

fit_lpm_probit <- function(formula, data) {
  list("LPM" = fit_lpm(formula, data), "Probit" = fit_probit(formula, data))
}

vcov_probit_cluster <- function(model) {
  sandwich::vcovCL(model, cluster = attr(model, "cluster"))
}

joint_lpm <- function(model, pattern) {
  terms <- names(coef(model))[grepl(pattern, names(coef(model)))]
  if (length(terms) == 0) stop("Не найдены interaction terms для pattern: ", pattern)
  b <- coef(model)[terms]
  V <- vcov(model)[terms, terms, drop = FALSE]
  q <- length(terms)
  stat <- as.numeric(t(b) %*% qr.solve(V) %*% b / q)
  p <- pf(stat, q, model$df.residual, lower.tail = FALSE)
  c(stat = stat, p = p)
}

joint_probit <- function(model, pattern) {
  terms <- names(coef(model))[grepl(pattern, names(coef(model)))]
  if (length(terms) == 0) stop("Не найдены interaction terms для pattern: ", pattern)
  b <- coef(model)[terms]
  V <- vcov_probit_cluster(model)[terms, terms, drop = FALSE]
  stat <- as.numeric(t(b) %*% qr.solve(V) %*% b)
  p <- pchisq(stat, df = length(terms), lower.tail = FALSE)
  c(stat = stat, p = p)
}

label_single <- function(x) {
  x <- gsub("`", "", x)
  if (x == "(Intercept)") return("Intercept")
  if (x == "identity_ethnic_index_c") return("Этническая идентичность")
  if (x == "identity_civic_index_c") return("Гражданская идентичность")
  if (x == "trust_index_c") return("Институциональное доверие")
  if (x == "authoritar_index_c") return("Авторитаризм")
  if (x == "contact_index_c") return("Частота контакта")
  if (x == "valence_index_c") return("Валентность контакта")
  if (x == "Supporta") return("Поддержка власти")
  if (x == "Age_resp") return("Возраст респондента")
  x <- gsub("^region", "", x)
  x <- gsub("^motivation", "", x)
  x <- gsub("^education", "", x)
  x <- gsub("^employer", "", x)
  x <- gsub("^gender", "", x)
  x <- gsub("^age", "Возраст: ", x)
  x <- gsub("^occupation", "", x)
  x <- gsub("^language", "", x)
  x <- gsub("^politics", "", x)
  x <- gsub("^appearance", "", x)
  x <- gsub("^Income", "Доход: ", x)
  x <- gsub("^Locality", "Тип населённого пункта: ", x)
  x <- gsub("^Female_resp", "Пол респондента: ", x)
  x <- gsub("^Education_resp", "Образование респондента: ", x)
  trimws(x)
}

label_term <- function(term) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  paste(sapply(parts, label_single), collapse = " * ")
}

coef_map_from_model <- function(model, patterns) {
  terms <- names(coef(model))
  keep <- Reduce(`|`, lapply(patterns, function(p) grepl(p, terms)))
  terms <- terms[keep]
  setNames(sapply(terms, label_term), terms)
}

save_modelsummary <- function(..., file_name) {
  args <- list(...)
  
  do.call(
    modelsummary,
    c(args, list(output = paste0("Tables/", file_name, ".html")))
  )
  
  tex_code <- do.call(
    modelsummary,
    c(args, list(output = "latex"))
  )
  
  tex_code <- as.character(tex_code)
  
  tex_code <- gsub("\\\\begin\\{table\\}\\[!h\\]", "", tex_code)
  tex_code <- gsub("\\\\begin\\{table\\}", "", tex_code)
  tex_code <- gsub("\\\\end\\{table\\}", "", tex_code)
  
  tex_code <- gsub(
    "\\\\begin\\{tabular\\}(\\{[^}]+\\})",
    "\\\\begin{longtable}\\1",
    tex_code
  )
  
  tex_code <- gsub(
    "\\\\end\\{tabular\\}",
    "\\\\end{longtable}",
    tex_code
  )
  
  writeLines(tex_code, paste0("Tables/", file_name, ".tex"), useBytes = TRUE)
}
table_main <- function(models, coef_map, title, file_name) {
  save_modelsummary(
    models, stars = TRUE, statistic = "({std.error})",
    coef_map = coef_map,
    vcov = list("LPM" = vcov(models$LPM),
                "Probit" = vcov_probit_cluster(models$Probit)),
    gof_omit = "IC|Log|AIC|BIC|RMSE|Std.Errors",
    title = title, file_name = file_name)
}

table_moderation <- function(models, coef_map, pattern, title, file_name) {
  jt_lpm <- joint_lpm(models$LPM, pattern)
  jt_probit <- joint_probit(models$Probit, pattern)
  add <- data.frame(
    term = c("Joint test (F / χ²)", "p-value"),
    LPM = c(round(jt_lpm["stat"], 3), round(jt_lpm["p"], 3)),
    Probit = c(round(jt_probit["stat"], 3), round(jt_probit["p"], 3))
  )
  save_modelsummary(
    models, stars = TRUE, statistic = "({std.error})",
    coef_map = coef_map,
    vcov = list("LPM" = vcov(models$LPM),
                "Probit" = vcov_probit_cluster(models$Probit)),
    add_rows = add,
    gof_omit = "IC|Log|AIC|BIC|RMSE|Std.Errors",
    title = title, file_name = file_name)
  invisible(add)
}

table_robust_lpm <- function(lpm_full, lpm_attentive, coef_map, pattern,
                             title, file_name) {
  add_full <- joint_lpm(lpm_full, pattern)
  add_att  <- joint_lpm(lpm_attentive, pattern)
  add <- data.frame(
    term = c("Joint test (F)", "p-value"),
    `Полная выборка` = c(round(add_full["stat"], 3), round(add_full["p"], 3)),
    `Внимательные респонденты` = c(round(add_att["stat"], 3), round(add_att["p"], 3)),
    check.names = FALSE
  )
  save_modelsummary(
    list("Полная выборка" = lpm_full,
         "Внимательные респонденты" = lpm_attentive),
    stars = TRUE, statistic = "({std.error})",
    coef_map = coef_map,
    vcov = list(vcov(lpm_full), vcov(lpm_attentive)),
    add_rows = add,
    gof_omit = "IC|Log|AIC|BIC|RMSE|Std.Errors",
    title = title, file_name = file_name)
  invisible(add)
}

ATTR_TERMS <- c("region", "motivation", "education", "employer", "gender",
                "age", "occupation", "language", "politics", "appearance")

f_main <- as.formula(paste("outcome ~", paste(ATTR_TERMS, collapse = " + ")))

f_identity <- outcome ~ region * identity_ethnic_index_c +
  region * identity_civic_index_c +
  motivation + education + employer + gender + age +
  occupation + language + politics + appearance

f_trust <- outcome ~ politics * trust_index_c +
  region + motivation + education + employer + gender + age +
  occupation + language + appearance

f_author <- outcome ~ region * authoritar_index_c +
  motivation + education + employer + gender + age +
  occupation + language + politics + appearance

f_polauthor <- outcome ~ politics * authoritar_index_c +
  region + motivation + education + employer + gender + age +
  occupation + language + appearance

f_contact <- outcome ~ appearance * contact_index_c +
  region + motivation + education + employer + gender + age +
  occupation + language + politics

f_valence <- outcome ~ appearance * valence_index_c +
  region + motivation + education + employer + gender + age +
  occupation + language + politics

f_support <- outcome ~ region + motivation + education + employer + gender + age +
  occupation + language + politics * Supporta + appearance + Supporta

f_intereduc <- outcome ~ region + motivation + education * Education_resp +
  employer + gender + age + occupation + language + politics + appearance +
  Age_resp + Income + Locality + Female_resp + Education_resp

models_main <- fit_lpm_probit(f_main, d)
models_identity <- fit_lpm_probit(f_identity, d)
models_trust <- fit_lpm_probit(f_trust, d)
models_author <- fit_lpm_probit(f_author, d)
models_polauthor <- fit_lpm_probit(f_polauthor, d)
models_contact <- fit_lpm_probit(f_contact, d)
models_valence <- fit_lpm_probit(f_valence, d)
models_support <- fit_lpm_probit(f_support, d)
models_intereduc <- fit_lpm_probit(f_intereduc, d)

coef_main <- coef_map_from_model(models_main$LPM,
                                 c("^region", "^motivation", "^education", "^employer", "^gender", "^age",
                                   "^occupation", "^language", "^politics", "^appearance"))

coef_identity <- coef_map_from_model(models_identity$LPM,
                                     c("^region", "^identity_ethnic_index_c", "^identity_civic_index_c",
                                       ":identity_ethnic_index_c", ":identity_civic_index_c"))

coef_trust <- coef_map_from_model(models_trust$LPM,
                                  c("^politics", "^trust_index_c", ":trust_index_c"))

coef_author <- coef_map_from_model(models_author$LPM,
                                   c("^region", "^authoritar_index_c", ":authoritar_index_c"))

coef_polauthor <- coef_map_from_model(models_polauthor$LPM,
                                      c("^politics", "^authoritar_index_c", ":authoritar_index_c"))

coef_contact <- coef_map_from_model(models_contact$LPM,
                                    c("^appearance", "^contact_index_c", ":contact_index_c"))

coef_valence <- coef_map_from_model(models_valence$LPM,
                                    c("^appearance", "^valence_index_c", ":valence_index_c"))

coef_support <- coef_map_from_model(models_support$LPM,
                                    c("^politics", "^Supporta", ":Supporta"))

coef_intereduc <- coef_map_from_model(models_intereduc$LPM,
                                      c("^education", "^Education_resp", ":Education_resp"))

table_main(models_main, coef_main,
           "Основная модель: эффекты атрибутов профиля",
           "main_lpm_probit")

table_moderation(models_identity, coef_identity, ":identity_ethnic_index_c",
                 "Этническая идентичность * регион",
                 "ethnic_identity_x_region_lpm_probit")
table_moderation(models_identity, coef_identity, ":identity_civic_index_c",
                 "Гражданская идентичность * регион",
                 "civic_identity_x_region_lpm_probit")
table_moderation(models_trust, coef_trust, ":trust_index_c",
                 "Институциональное доверие * политика страны",
                 "trust_x_politics_lpm_probit")
table_moderation(models_author, coef_author, ":authoritar_index_c",
                 "Авторитаризм * регион",
                 "authoritarianism_x_region_lpm_probit")
table_moderation(models_polauthor, coef_polauthor, ":authoritar_index_c",
                 "Авторитаризм * политика страны",
                 "authoritarianism_x_politics_lpm_probit")
table_moderation(models_contact, coef_contact, ":contact_index_c",
                 "Частота контакта * традиционная одежда",
                 "contact_x_appearance_lpm_probit")
table_moderation(models_valence, coef_valence, ":valence_index_c",
                 "Валентность контакта * традиционная одежда",
                 "valence_x_appearance_lpm_probit")
table_moderation(models_support, coef_support, ":Supporta",
                 "Поддержка власти * политика страны",
                 "support_x_politics_lpm_probit")
table_moderation(models_intereduc, coef_intereduc, ":Education_resp",
                 "Образование респондента * образование мигранта",
                 "education_resp_x_education_lpm_probit")

models_main_att <- fit_lpm(f_main, d_attentive)
models_identity_att <- fit_lpm(f_identity, d_attentive)
models_trust_att <- fit_lpm(f_trust, d_attentive)
models_author_att <- fit_lpm(f_author, d_attentive)
models_polauthor_att <- fit_lpm(f_polauthor, d_attentive)
models_contact_att <- fit_lpm(f_contact, d_attentive)
models_valence_att <- fit_lpm(f_valence, d_attentive)
models_support_att <- fit_lpm(f_support, d_attentive)
models_intereduc_att <- fit_lpm(f_intereduc, d_attentive)

save_modelsummary(
  list("Полная выборка" = models_main$LPM,
       "Внимательные респонденты" = models_main_att),
  stars = TRUE, statistic = "({std.error})",
  coef_map = coef_main,
  vcov = list(vcov(models_main$LPM), vcov(models_main_att)),
  gof_omit = "IC|Log|AIC|BIC|RMSE|Std.Errors",
  title = "Устойчивость: основная модель (LPM)",
  file_name = "robust_main_lpm")

table_robust_lpm(models_identity$LPM, models_identity_att, coef_identity,
                 ":identity_ethnic_index_c",
                 "Устойчивость: этническая идентичность * регион (LPM)",
                 "robust_ethnic_identity_x_region_lpm")
table_robust_lpm(models_identity$LPM, models_identity_att, coef_identity,
                 ":identity_civic_index_c",
                 "Устойчивость: гражданская идентичность * регион (LPM)",
                 "robust_civic_identity_x_region_lpm")
table_robust_lpm(models_trust$LPM, models_trust_att, coef_trust,
                 ":trust_index_c",
                 "Устойчивость: институциональное доверие * политика страны (LPM)",
                 "robust_trust_x_politics_lpm")
table_robust_lpm(models_author$LPM, models_author_att, coef_author,
                 ":authoritar_index_c",
                 "Устойчивость: авторитаризм * регион (LPM)",
                 "robust_authoritarianism_x_region_lpm")
table_robust_lpm(models_polauthor$LPM, models_polauthor_att, coef_polauthor,
                 ":authoritar_index_c",
                 "Устойчивость: авторитаризм * политика страны (LPM)",
                 "robust_authoritarianism_x_politics_lpm")
table_robust_lpm(models_contact$LPM, models_contact_att, coef_contact,
                 ":contact_index_c",
                 "Устойчивость: частота контакта * традиционная одежда (LPM)",
                 "robust_contact_x_appearance_lpm")
table_robust_lpm(models_valence$LPM, models_valence_att, coef_valence,
                 ":valence_index_c",
                 "Устойчивость: валентность контакта * традиционная одежда (LPM)",
                 "robust_valence_x_appearance_lpm")
table_robust_lpm(models_support$LPM, models_support_att, coef_support,
                 ":Supporta",
                 "Устойчивость: поддержка власти * политика страны (LPM)",
                 "robust_support_x_politics_lpm")
table_robust_lpm(models_intereduc$LPM, models_intereduc_att, coef_intereduc,
                 ":Education_resp",
                 "Устойчивость: образование респондента * образование мигранта (LPM)",
                 "robust_education_resp_x_education_lpm")