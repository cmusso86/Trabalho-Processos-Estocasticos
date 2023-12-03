library(heemod)

# ------------------------------------------------------------------------
# Parâmetros
# ------------------------------------------------------------------------
par_mod <- define_parameters(
  age_base    = 20,
  age_cycle = model_time + age_base,
  p_death_all = 262828 / 214300000, # https://cieges.conass.org.br/paineis/listagem/situacao-de-saude-da-populacao/painel-de-analise-do-excesso-de-mortalidade-por-causas-naturais-no-brasil
  p_death_disease = 0.2       , # Probabilidade de morte por suicídio em TAB
  p_death_symp = combine_probs(
     p_death_all,
     p_death_disease),
  p_disease_base = 0.01       , # Probabilidade da doença na população geral
  med_effect = 0.5            , # Efeito da medicação
  p_disease_med = p_disease_base * med_effect , # Probabilidade recaída durante tratamento
  cost_hospit_cycle = 100000  , # Custo de cada ciclo de hospitalização
  p_cured = 0.001,
  cost_med = 5000,
  dr = 0.05,
  qaly_disease = 0.5)

  # p_doença = 0.03           , # Prevalencia na populacao (%)
  # p_recaida = 0.9           , # Probabilidade de novos eventos no futuro
  # p_refratario = 0.1        , # Probabilidade de curso refratario

  # efeito_tto = 0.1          , # Efeito do tratamento
  # p_doenca_medicado = p_recaida * efeito_tto ,# Probabilidade de recaída após medicação
  # p_cura = 0.001            , # Probabilidade de reversão para estágio assintomático
  # custo_medicacao = 6000    ,
  # custo_internacao = 100000 ,
  # dr = 0.05                 , # Discount rate
  # qaly_doenca = 0.5         , # QALY para um ano no estado sintomático


  # p_internacao = 0.30,
  # p_desinternacao = 0.20
  )

# ------------------------------------------------------------------------
# Transicoes
# ------------------------------------------------------------------------
mat_base <- define_transition(
  state_names = c("pre", "symp", "death"),

  C,                 p_disease_base,   p_death_all,
  p_cured,           C           ,     p_death_symp,
  0,                 0,                1)

mat_med <- define_transition(
  state_names = c("pre", "symp", "death"),

  C,                 p_disease_med,    p_death_all,
  p_cured,           C           ,     p_death_symp,
  0,                 0,                1)



# ------------------------------------------------------------------------
# Valores de estados
# ------------------------------------------------------------------------
# custo_medicacao - custo do tratamento
# custo_internacao - custo da internação
# custo_total - custo total
# qaly - anos de vida relacionados a saúde ajustados pela qualidade de vida
#        1 - um ano em perfeita saúde
#        0 - morte

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

  pre = state_pre,
  symp = state_symp,
  death = state_death)

strat_med <- define_strategy(
  transition = mat_med,

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

  cycles = 10,

  cost = cost_total,
  effect = qaly,

  method = "life-table")

res_mod
