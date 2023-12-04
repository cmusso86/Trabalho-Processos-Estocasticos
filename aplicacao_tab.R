library(heemod)

# ------------------------------------------------------------------------
# Parâmetros
# ------------------------------------------------------------------------
par_mod <- define_parameters(
  age_base    = 25,
  age_cycle = model_time + age_base,
  p_death_all = 262828 / 214300000, # https://cieges.conass.org.br/paineis/listagem/situacao-de-saude-da-populacao/painel-de-analise-do-excesso-de-mortalidade-por-causas-naturais-no-brasil
  p_death_disease = 0.2       , # Probabilidade de morte por suicídio em TAB
  p_death_symp = combine_probs(
     p_death_all,
     p_death_disease),
  p_disease_base = 0.9       , # Probabilidade da doença na população geral
  med_effect = 0.5            , # Efeito da medicação
  prevalencia = 0.04,
  p_disease_med = p_disease_base * med_effect , # Probabilidade recaída durante tratamento
  cost_hospit_cycle = 33090  , # Custo de cada ciclo de hospitalização
  p_cured_base = 0.2,
  p_cured_med = 0.7,
  cost_med = 1904,
  dr = 0.05,
  qaly_disease = 0.5)

# ------------------------------------------------------------------------
# Transicoes
# ------------------------------------------------------------------------
mat_base <- define_transition(
  state_names = c("geral", "pre", "symp", "death"),

  C,     prevalencia, 0 ,               p_death_all,
  0,     C,           p_disease_base,   p_death_all,
  0,    p_cured_base,        C           ,   p_death_symp,
  0,    0,             0,                1)

mat_med <- define_transition(
  state_names = c("geral", "pre", "symp", "death"),

  C,     prevalencia, 0 ,               p_death_all,
  0,     C,           p_disease_med,   p_death_all,
  0,    p_cured_med,        C           ,   p_death_symp,
  0,    0,             0,                1)




# ------------------------------------------------------------------------
# Valores de estados
# ------------------------------------------------------------------------
# custo_medicacao - custo do tratamento
# custo_internacao - custo da internação
# custo_total - custo total
# qaly - anos de vida relacionados a saúde ajustados pela qualidade de vida
#        1 - um ano em perfeita saúde
#        0 - morte


state_geral <- define_state(
  cost_treat = 0,
  cost_hospit = 0, # good health => no hospital expenses
  cost_total = 0,
  qaly = 1)

state_pre <- define_state(
  cost_treat = dispatch_strategy(
    base = 0, # no treatment => no treatment cost
    med = cost_med),
  cost_hospit = 0, # good health => no hospital expenses
  cost_total = discount(cost_treat + cost_hospit, r = dr),
  qaly = 1)

state_symp <- define_state(
  cost_treat = 0,
  cost_hospit = cost_hospit_cycle,
  cost_total = discount(cost_treat + cost_hospit, r = dr),
  qaly = qaly_disease)

state_death <- define_state(
  cost_treat = 0,
  cost_hospit = 0,
  cost_total = 0,
  qaly = 0)

# ------------------------------------------------------------------------
# Estrategias
# ------------------------------------------------------------------------
strat_base <- define_strategy(
  transition = mat_base,

  geral = state_geral,
  pre = state_pre,
  symp = state_symp,
  death = state_death)

strat_med <- define_strategy(
  transition = mat_med,

  geral = state_geral,
  pre = state_pre,
  symp = state_symp,
  death = state_death)

# ------------------------------------------------------------------------
# Estrategias
# ------------------------------------------------------------------------
res_mod <- run_model(
  parameters = par_mod,

  base = strat_base,
  med = strat_med,

  cycles = 20,

  cost = cost_total,
  effect = qaly,

  method = "life-table")

res_mod

plot(res_mod)


# ------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------

plot(res_mod) +
  xlab("Tempo (anos)") +
  ylab("Nº de indivíduos") +
  theme_light() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )


