# -----------------------------------------------------
# PART 1
# This script is for downloading data from the workshop.
# 0) Clean variables and Libraries
# 1) Web scraping to access the page
# 2) Save the data in a structure
# 3) Clean data and create variables
# 4) Descriptive statistics
#    4.1) Descriptive statistics
#    4.2) Check variables distributions (histograms)
#    4.3) Statistics on variables to check for correlation
#    4.4) Imputation of missing values
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Good practices, clean variables and libraries
# -----------------------------------------------------

# Clean variables and Libraries
rm(list = ls())

require(pacman)
p_load(rvest, dplyr, tidyr, readr, httr, jsonlite, boot, lfe, ggplot2, skimr)

# -----------------------------------------------------
# 1) Web scraping to access the page
# -----------------------------------------------------

# Access the page
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
pagina <- read_html(url)

# Extract links within the <ul> at the end of the page (relative)
links <- pagina %>%
  html_nodes("ul li a") %>%
  html_attr("href")

# Convert relative links to absolute 
links <- ifelse(grepl("^http", links), links, paste0(url, links))

# -----------------------------------------------------
# 2) Save the data in a structure
# -----------------------------------------------------
# While doing the web scrapping, we found the table in each link is dinamic,
# thus, checking into the structure of the html, we found it creates the table
# obtaining the data through a request to another web page. Therefore we will 
# directly extract the data from this original web

# Create empty final dataset
db <- data.frame()

# Create the url base and end to iterate the 10 chunks
link_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
link_end <- ".html"
numbers <- c(1:10)

upload_data <- function(numero) {
  # Build the URL
  test <- paste0(link_base, numero, link_end)
  # Read the subpage
  subpage_link <- read_html(test)
  # Extract the table
  table <- subpage_link %>%
    html_node("table") %>%
    html_table()
  # Add the data into final db
  db <<- rbind(db, table)
}

lapply(numbers, upload_data)

# -----------------------------------------------------
# 3) Clean data and create variables
# -----------------------------------------------------
# The GEIH database contains a large number of variables, so we must first 
# select those that we specifically need for this project. Our analysis will 
# be restricted to working adults (individuals aged 18+ who are employed) from 
# the GEIH database

#Check the existing names
names(db)

# With the Manuel Fernandez document, we renamed the varibles to make them 
# understandable; delete columns without names
db <- db[, !is.na(names(db)) & names(db) != ""]
db <- db %>%
  rename(
    hombre = sex, parentesco_jefe_hogar = p6050, afiliado_seguridad_social = p6090,
    regimen_salud = p6100, nivel_educativo = p6210, grado_aprobado = p6210s1,
    actividad_semana_pasada = p6240, tiempo_empresa_actual = p6426, ingreso_trabajo_principal = p6500,
    recibio_horas_extras = p6510, valor_horas_extras = p6510s1, incluyo_horas_extras = p6510s2,
    recibio_primas = p6545, valor_primas = p6545s1, incluyo_primas = p6545s2,
    recibio_bonificaciones = p6580, valor_bonificaciones = p6580s1, incluyo_bonificaciones = p6580s2,
    subsidio_alimentacion = p6585s1, valor_subsidio_alimentacion = p6585s1a1,
    incluyo_subsidio_alimentacion = p6585s1a2, subsidio_transporte = p6585s2,
    valor_subsidio_transporte = p6585s2a1, incluyo_subsidio_transporte = p6585s2a2,
    subsidio_familiar = p6585s3, valor_subsidio_familiar = p6585s3a1,
    incluyo_subsidio_familiar = p6585s3a2, subsidio_educativo = p6585s4,
    valor_subsidio_educativo = p6585s4a1, incluyo_subsidio_educativo = p6585s4a2,
    recibio_alimentos_trabajo = p6590, valor_alimentos_trabajo = p6590s1,
    recibio_vivienda_trabajo = p6600, valor_vivienda_trabajo = p6600s1,
    transporte_empresa = p6610, valor_transporte_empresa = p6610s1,
    otros_ingresos_especie = p6620, valor_otros_especie = p6620s1,
    prima_servicios = p6630s1, valor_prima_servicios = p6630s1a1,
    prima_navidad = p6630s2, valor_prima_navidad = p6630s2a1,
    prima_vacaciones = p6630s3, valor_prima_vacaciones = p6630s3a1,
    viaticos_permanentes = p6630s4, valor_viaticos_permanentes = p6630s4a1,
    bonificaciones_anuales = p6630s6, valor_bonificaciones_anuales = p6630s6a1,
    ganancia_neta_mes = p6750, meses_ganancia_corresponde = p6760,
    ganancia_neta_12_meses = p550, tamano_empresa = p6870, cotiza_pension_actual = p6920,
    tiene_segundo_trabajo = p7040, posicion_segundo_trabajo = p7050,
    ingreso_segundo_trabajo = p7070, quiere_trabajar_mas_horas = p7090,
    hizo_diligencias_mas_horas = p7110, disponible_mas_horas = p7120,
    cambiar_por_capacidades = p7140s1, cambiar_por_ingresos = p7140s2,
    diligencias_cambiar_trabajo = p7150, disponible_nuevo_trabajo = p7160,
    primera_vez_o_trabajo_antes = p7310, posicion_ultimo_trabajo = p7350,
    ingresos_trabajo_desocupado = p7422, valor_ingresos_desocupado = p7422s1,
    ingresos_trabajo_inactivo = p7472, valor_ingresos_inactivo = p7472s1,
    recibio_arriendos_pensiones = p7495, arriendos_propiedades = p7500s1,
    valor_arriendos = p7500s1a1, pensiones_jubilaciones = p7500s2,
    valor_pensiones = p7500s2a1, pension_alimenticia = p7500s3,
    valor_pension_alimenticia = p7500s3a1, recibio_transferencias_12m = p7505,
    dinero_hogares_pais = p7510s1, valor_dinero_hogares_pais = p7510s1a1,
    dinero_hogares_exterior = p7510s2, valor_dinero_exterior = p7510s2a1,
    ayudas_instituciones = p7510s3, valor_ayudas_instituciones = p7510s3a1,
    intereses_dividendos = p7510s5, valor_intereses_dividendos = p7510s5a1,
    cesantias_intereses = p7510s6, valor_cesantias = p7510s6a1,
    otros_ingresos_fuentes = p7510s7, valor_otros_ingresos = p7510s7a1,
    poblacion_edad_trabajar = pet, inactivo = ina, ocupado = ocu, desocupado = dsi,
    poblacion_economicamente_activa = pea, inactivos = inac, working_age_population = wap,
    tamano_firma = sizeFirm, total_horas_trabajadas = totalHoursWorked,
    ingreso_actividad_principal_obs = impa, ingreso_segunda_actividad_obs = isa,
    ingreso_especie_obs = ie, ingreso_desocupados_inactivos_obs = imdi,
    ingreso_intereses_dividendos_obs = iof1, ingreso_pensiones_obs = iof2,
    ingreso_ayudas_hogares_obs = iof3h, ingreso_ayudas_instituciones_obs = iof3i,
    ingreso_arriendos_obs = iof6, estado_imputacion_actividad_principal = cclasnr2,
    estado_imputacion_segunda_actividad = cclasnr3, estado_imputacion_especie = cclasnr4,
    estado_imputacion_desocupados_inactivos = cclasnr5, estado_imputacion_intereses = cclasnr6,
    estado_imputacion_pensiones = cclasnr7, estado_imputacion_ayudas = cclasnr8,
    estado_imputacion_arriendos = cclasnr11, ingreso_actividad_principal_imp = impaes,
    ingreso_segunda_actividad_imp = isaes, ingreso_especie_imp = iees,
    ingreso_desocupados_inactivos_imp = imdies, ingreso_intereses_dividendos_imp = iof1es,
    ingreso_pensiones_imp = iof2es, ingreso_ayudas_hogares_imp = iof3hes,
    ingreso_ayudas_instituciones_imp = iof3ies, ingreso_arriendos_imp = iof6es,
    ingreso_total_observado = ingtotob, ingreso_total_imputado = ingtotes,
    ingreso_total_final = ingtot, salario_mensual = y_salary_m,
    salario_mensual_horas_usuales = y_salary_m_hu, ingreso_laboral_mensual = y_ingLab_m,
    ingresos_horas_extras_m = y_horasExtras_m, ingresos_especie_m = y_especie_m,
    ingresos_vivienda_m = y_vivienda_m, otros_ingresos_m = y_otros_m,
    auxilio_alimentacion_m = y_auxilioAliment_m, auxilio_transporte_m = y_auxilioTransp_m,
    subsidio_familiar_m = y_subFamiliar_m, subsidio_educativo_m = y_subEducativo_m,
    primas_mensuales = y_primas_m, bonificaciones_mensuales = y_bonificaciones_m,
    prima_servicios_m = y_primaServicios_m, prima_navidad_m = y_primaNavidad_m,
    prima_vacaciones_m = y_primaVacaciones_m, viaticos_m = y_viaticos_m,
    seguros_accidentes_m = y_accidentes_m, salario_segundo_trabajo_m = y_salarySec_m,
    ingreso_laboral_horas_actuales = y_ingLab_m_ha, ganancia_neta_mensual = y_gananciaNeta_m,
    ganancia_neta_agro_m = y_gananciaNetaAgro_m, ganancia_independiente_m = y_gananciaIndep_m,
    ganancia_independiente_horas_usuales = y_gananciaIndep_m_hu, ingreso_total_mensual = y_total_m,
    ingreso_total_horas_actuales = y_total_m_ha, factor_expansion_anual = fex_c,
    departamento = depto, factor_expansion_departamental = fex_dpto,
    factor_ponderacion = fweight, maximo_nivel_educativo = maxEducLevel,
    educacion_superior = college, registrado_salud = regSalud,
    cotiza_pension = cotPension, relacion_laboral = relab
  )

# Create a variable number of minors
db <- db %>%
  mutate(bin_minor = ifelse(test = age <= 18, yes = 1, no = 0))

db <- db %>%
  group_by(directorio, secuencia_p) %>%
  mutate(num_minors = sum(bin_minor, na.rm = TRUE)) %>%
  select(-bin_minor) %>% 
  ungroup()

# Create a variable to identify the household head
db <- db %>% mutate(bin_head = ifelse(test = parentesco_jefe_hogar == 1, yes = 1, no = 0))

# Create a variable to identify if the household head is female
db <- db %>% mutate(bin_headFemale = bin_head*(1-hombre))

# Keep data with positive hours worked
db <- db %>% filter(total_horas_trabajadas>0)

# Keep only individuals 18 years and older
db <- db %>% filter(age>18)

# Create age squared
db <- db %>% mutate(age2 = age*age)

# skim the number of missing values
db_miss <- skim(db) %>% select(skim_variable, n_missing)

# view missing values as percentage
nobs <- nrow(db) # number of observations
db_miss<- db_miss %>% mutate(p_missing= n_missing/nobs) # new variable of number of NA
db_miss <- db_miss %>% arrange(-n_missing) # descendant order
db_miss<- db_miss %>% filter(n_missing!= 0) # keep only NA
head(db_miss, 10) # Show the 10 first observations

# Delete those variables with more than 80% of missing values
# This db is only for code developing purposes; helping the team understand the data,
# not used in the final report
db_clean <- db %>% select(-all_of(db_miss$skim_variable[db_miss$p_missing > 0.8]))
names(db_clean)

db <- db %>%
  select(num_minors, hombre, num_minors, bin_head,age, bin_headFemale, 
    age2, estrato1,maximo_nivel_educativo, tamano_empresa,
    tiempo_empresa_actual,formal,hoursWorkUsual,
    ingreso_laboral_horas_actuales, oficio
  )

# Apply factor to categorics
db <- db %>%
  mutate(estrato1 = as.factor(estrato1)) %>%
  mutate(maximo_nivel_educativo = as.factor(maximo_nivel_educativo)) %>%
  mutate(tamano_empresa = as.factor(tamano_empresa)) %>%
  mutate(oficio = as.factor(oficio)) %>%
  mutate(log_ingreso_laboral_horas_actuales = log(ingreso_laboral_horas_actuales))


# -----------------------------------------------------
# 4.1) Descriptive statistics
# -----------------------------------------------------
# Independent variable - ingreso_laboral_horas_actuales = Base labor income excluding bonuses, 
# allowances, and subsidies
# Controlled variables = estrato1 - maximo_nivel_educativo - tamano_empresa - tiempo_empresa_actual - 
# formal - age - hoursWorkUsual

## TODO: Una vez ingreso no tenga missing values, eliminar la linea de codigo de ingreso > 0 y que no contenga NA

### FOR GRAPH EXPORTS TO WORK, SET WORKING DIRECTORY TO THE MAIN FOLDER OF THE
### PROJECT, NAMED "Problem-Set-1-Predicting-Income"

# Mean by socioeconomic level (1 to 6)
mean_se_level <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         estrato1 %in% 1:6) %>%
  group_by(estrato1) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_se_level, aes(x = factor(estrato1), y = media)) +
  geom_col(fill = "steelblue") + geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Income by Socioeconomic Level",
       x = "Socioeconomic Level",
       y = "Mean Income by Hour")
  theme_minimal()

# Export graph to "views"
name <- "mean_income by socioeconomic level"
link <- past0("views/", name, ".png")
ggsave(link, plot = last_plot(), width = 8, height = 6)


# Mean by education level (1 to 7)
mean_educ_level <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         estrato1 %in% 1:6) %>%
  group_by(maximo_nivel_educativo) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_educ_level, aes(x = factor(maximo_nivel_educativo), y = media)) +
  geom_col(fill = "steelblue") + geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Education Level",
       x = "Education Level",
       y = "Mean Income by Hour")
theme_minimal()

# Company Size
mean_tamano_empresa <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         !is.na(tamano_empresa)) %>%
  group_by(tamano_empresa) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_tamano_empresa, aes(x = factor(tamano_empresa), y = media)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Company Size",
       x = "Company Size",
       y = "Mean Income by Hour") +
  theme_minimal()

# Time in current company
mean_tiempo_empresa <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         !is.na(tiempo_empresa_actual)) %>%
  mutate(tiempo_grupo = case_when(
    tiempo_empresa_actual < 1 ~ "< 1 year",
    tiempo_empresa_actual >= 1 & tiempo_empresa_actual < 3 ~ "1-3 years",
    tiempo_empresa_actual >= 3 & tiempo_empresa_actual < 5 ~ "3-5 years",
    tiempo_empresa_actual >= 5 ~ "5+ years"
  )) %>%
  group_by(tiempo_grupo) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_tiempo_empresa, aes(x = factor(tiempo_grupo), y = media)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Time in Current Company",
       x = "Time in Company",
       y = "Mean Income by Hour") +
  theme_minimal()

# Formal or informal employment type
mean_formal <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         !is.na(formal)) %>%
  group_by(formal) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_formal, aes(x = factor(formal, labels = c("Informal", "Formal")), y = media)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Employment Type",
       x = "Employment Type",
       y = "Mean Income by Hour") +
  theme_minimal()

# Age
mean_age <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         !is.na(age)) %>%
  mutate(age_grupo = case_when(
    age < 25 ~ "18-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 ~ "55+"
  )) %>%
  group_by(age_grupo) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_age, aes(x = factor(age_grupo), y = media)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Age Group",
       x = "Age Group",
       y = "Mean Income by Hour") +
  theme_minimal()

# 5. Horas trabajo habituales (agrupadas en rangos)
mean_hours <- db %>%
  filter(!is.na(ingreso_laboral_horas_actuales), 
         ingreso_laboral_horas_actuales > 0,
         !is.na(hoursWorkUsual)) %>%
  mutate(hours_grupo = case_when(
    hoursWorkUsual < 20 ~ "Part-time (<20h)",
    hoursWorkUsual >= 20 & hoursWorkUsual < 40 ~ "Part-time (20-39h)",
    hoursWorkUsual >= 40 & hoursWorkUsual <= 48 ~ "Full-time (40-48h)",
    hoursWorkUsual > 48 ~ "Overtime (>48h)"
  )) %>%
  group_by(hours_grupo) %>%
  summarise(media = mean(ingreso_laboral_horas_actuales))

ggplot(mean_hours, aes(x = factor(hours_grupo), y = media)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(media, 0)), vjust = -0.5) +
  labs(title = "Mean Hourly Income by Work Hours Category",
       x = "Work Hours Category",
       y = "Mean Income by Hour") +
  theme_minimal()

# -----------------------------------------------------
# 4.2) Check variables distributions (histograms)
# -----------------------------------------------------

# Total Income
# TODO: put units to title
ggplot(db, aes(x = ingreso_total_mensual)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram Total Income",
       x = "Monthly Income",
       y = "Frequency")

# Total Income / Hours Worked
ggplot(db, aes(x = ingreso_por_hora)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram Income by hour",
       x = "Income by hour",
       y = "Frequency")

ggplot(db, aes(x = log_ingreso_por_hora)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram log Income by hour",
       x = "log Income by hour",
       y = "Frequency")

# Hours Worked
# TODO: put unites to title
# TODO: no me parece tan bien distribuida; sqrt funciona mejor, pero no convence
ggplot(db, aes(x = total_horas_trabajadas)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram Total Hours Worked",
       x = "Hours Worked",
       y = "Frequency")

# Age
# TODO: no sé si la necesitamos realmente
ggplot(db, aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram Age",
       x = "Age",
       y = "Frequency")

# -----------------------------------------------------
# 4.3) Statistics on variables to check for correlation
# -----------------------------------------------------

## association with income 
yvar <- "ingreso_laboral_horas_actuales"
controls <- c(
  "age","estrato1","formal",
  "tamano_empresa","maximo_nivel_educativo",
  "tiempo_empresa_actual", "hoursWorkUsual"
)

# Keep only available variables
vars_keep <- intersect(c(yvar, controls), names(db))
d <- db[, vars_keep]

# Binary controls
bin_controls <- intersect(c("formal"), names(d))

# Helper: coerce binary factor/character/logical to 0/1
to_01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  return(as.integer(x) - 1L)           # level1=0, level2=1
  if (is.character(x)) {
    ux <- unique(na.omit(x))
    if (length(ux) == 2) {
      m <- setNames(c(0L,1L), ux)
      return(unname(m[x]))
    }
  }
  if (is.numeric(x)) return(x)                             # assume already 0/1 or numeric
  return(x)
}

# Build unified association table:
# - For numeric x: Pearson correlation and slope from y ~ x
# - For binary x (0/1): same stats; slope = mean(1) - mean(0)
assoc_y_controls <- bind_rows(lapply(intersect(controls, names(d)), function(v){
  y <- d[[yvar]]
  x <- d[[v]]
  
  # Coerce binaries to 0/1; leave others numeric
  x2 <- if (v %in% bin_controls) to_01(x) else as.numeric(x)
  
  cc <- complete.cases(y, x2)
  y  <- y[cc]; x2 <- x2[cc]
  n  <- length(y)
  
  if (n < 5) {
    return(data.frame(var = v, type = ifelse(v %in% bin_controls, "binary","numeric"),
                      n = n, corr = NA_real_, slope = NA_real_, r2 = NA_real_, p = NA_real_))
  }
  
  fit <- lm(y ~ x2)
  sm  <- summary(fit)
  
  data.frame(
    var   = v,
    type  = ifelse(v %in% bin_controls, "binary", "numeric"),
    n     = n,
    corr  = suppressWarnings(cor(y, x2)),     # Pearson correlation
    slope = unname(coef(fit)[2]),             # change in y per 1-unit in x (for binary: mean diff 1 vs 0)
    r2    = sm$r.squared,
    p     = sm$coefficients[2,4]
  )
})) %>% arrange(p)  # sort by p-value

assoc_y_controls

# Simple visualization: absolute correlation with the outcome
assoc_y_corr_plot <- assoc_y_controls %>%
  mutate(var = reorder(var, abs(corr))) %>%
  ggplot(aes(x = var, y = abs(corr))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Absolute correlation with the outcome",
    x = "Control",
    y = "|corr(y, x)|"
  ) +
  theme_minimal()

assoc_y_corr_plot


# -----------------------------------------------------
# 4.4) Imputation of missing values
# -----------------------------------------------------

# Imputate missing values of income by age group mean
# We use age, job and gender (hombre) because they are variables with 100% data, 
# thus, no N/A conflict. 

# 1st, we use all 3 filters
db <- db %>%
  mutate(age_grupo = case_when(
      age < 25 ~ "18-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 & age < 60 ~ "55-60",
      age >= 60 ~ "60+"
    )) %>%
  group_by(oficio, age_grupo, hombre) %>%
  mutate(
    ingreso_laboral_horas_actuales = ifelse(
      is.na(ingreso_laboral_horas_actuales),
      mean(ingreso_laboral_horas_actuales, na.rm = TRUE),
      ingreso_laboral_horas_actuales
    ),
    log_ingreso_laboral_horas_actuales = ifelse(
      is.na(log_ingreso_laboral_horas_actuales),
      mean(log_ingreso_laboral_horas_actuales, na.rm = TRUE),
      log_ingreso_laboral_horas_actuales
    )
  ) %>%
  ungroup()

# 2nd, we take down filter of gender, because cases appeared where no data for
# all 3 variables was available to impute (same age, job and gender)

db <- db %>%
  mutate(age_grupo = case_when(
    age < 25 ~ "18-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 & age < 60 ~ "55-60",
    age >= 60 ~ "60+"
  )) %>%
  group_by(oficio, age_grupo) %>%
  mutate(
    ingreso_laboral_horas_actuales = ifelse(
      is.na(ingreso_laboral_horas_actuales),
      mean(ingreso_laboral_horas_actuales, na.rm = TRUE),
      ingreso_laboral_horas_actuales
    ),
    log_ingreso_laboral_horas_actuales = ifelse(
      is.na(log_ingreso_laboral_horas_actuales),
      mean(log_ingreso_laboral_horas_actuales, na.rm = TRUE),
      log_ingreso_laboral_horas_actuales
    )
  ) %>%
  ungroup()

# Eliminate few missing values that have no close age & job group
db <- db %>% filter(!is.na(ingreso_laboral_horas_actuales))

