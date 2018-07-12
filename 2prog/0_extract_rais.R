library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(haven)
library(readxl)
library(ggplot2)
library(tidyr)
library(lubridate)
library(aod)
library(nnet)


rais_pes.file <- list.files("//storage6/bases/DADOS/RESTRITO/RAIS/csv", pattern = 'brasil20', full.names = T) %>% 
  grep(pattern = '\\.csv', value = T)
rais_pes.file <- rais_pes.file[7:17]

rais_pes <- data.table()

# Número de linhas.
n = -1

# codemun capitais:
capitais <- c(280030,150140,310620,140010,500270,510340,410690,420540,230440,520870,530010,250750,160030,270430,
              130260,240810,172100,431490,110020,261160,120040,330455,292740,211130,355030,221100,320530)

# Vínculos de interesse:
selec_vinc <- data.table('tipo_vinculo' = c(10,15,30,31), 'I' = 1)

for(i in rais_pes.file){
  
  ano_rais <- substr(i,48,51) %>% as.numeric
  rais_ano <- fread(i, nrows = n)
  
  # Seleciona somente os vínculos padrão de trabalhadores privado e público: CLT e Estatutário. Exclui temporários.  
  rais_ano <- selec_vinc[rais_ano, on = 'tipo_vinculo'][I == 1]
  rais_ano <- rais_ano[codemun %in% capitais, .(tipo_estab, id_estab, cnpj_raiz, clas_cnae20, tamestab, uf, codemun, 
                                                cpf, idade, genero, raca_cor, grau_instr,tipo_vinculo, data_adm, 
                                                data_deslig, horas_contr, salario,ind_horas_extras)]
  
  rais_ano[, ano := ano_rais]
  
  rais_pes <- rbindlist(list(rais_pes, rais_ano), fill = T)
  
  rm(rais_ano)
  gc()
  
}

### Adequa vars:

rais_pes[, .N, genero]
rais_pes[genero == 1, mulher := 0]
rais_pes[genero == 2, mulher := 1]

rais_pes[, dt_adm := as.Date(data_adm, '%m/%d/%Y')]
rais_pes[, dt_deslig := as.Date(data_deslig, '%m/%d/%Y')]

raca <- fread('C:/Users/B2252934/Documents/Git/HCExternalities_in_firms/3work/dicionarios/raca_cor.csv')
raca[, raca_cor_desc := as.factor(raca_cor_desc)]
rais_pes <- raca[rais_pes, on = 'raca_cor']

rais_pes[, Fundamental_1 := 0]
rais_pes[, Fundamental_2 := 0]
rais_pes[, Medio := 0]
rais_pes[, Superior := 0]
rais_pes[, Mestrado := 0]
rais_pes[, Doutorado := 0]
rais_pes[grau_instr >= 3, Fundamental_1 := 1]
rais_pes[grau_instr >= 5, Fundamental_2 := 1]
rais_pes[grau_instr >= 7, Medio := 1]
rais_pes[grau_instr >= 9, Superior := 1]
rais_pes[grau_instr >= 10, Mestrado := 1]
rais_pes[grau_instr >= 11, Doutorado := 1]




### Traz dicionário do grau de instrução da Rais.

dic_instr.file <- '//storage4/bases/RESTRITO/aval_MCMV/data/RAIS/conversao_anos_educacao.csv'
dic_instr <- fread(dic_instr.file)
dic_instr[, grau_instr_desc := grau_instr_desc %>% as.factor]

rais_pes <- dic_instr[rais_pes, on = 'grau_instr']


### Corrige uf de 2015:

uf_corr <- rais_pes[ano != 2015, .N, .(uf, codemun)][order(uf, codemun)][,.('uf2' = uf, codemun)]
rais_pes <- uf_corr[rais_pes, on = 'codemun']
rais_pes[, .N, .(uf, uf2)][order(uf, uf2)]
rais_pes[, uf := uf2 %>% as.factor()]
rais_pes[, uf2 := NULL]


### Marca servidores públicos:

rais_pes[, serv := 0]
rais_pes[tipo_vinculo %in% c(30,31), serv := 1]
rais_pes[, .N, serv]
rais_pes[, .N, .(serv, cpf)][, .(num_set = .N), .(cpf)][, .N, num_set]


### Tabelas descritivas simples:

rais_pes[, .N, .(cpf, ano)][, .(num_obs=.N), cpf][, .N, num_obs][order(num_obs)]
rais_pes[, .N, .(tipo_vinculo)]
rais_pes[, .N, .(cpf, tipo_vinculo)][, .(num_vinculos=.N), .(cpf)][, .N, num_vinculos]
rais_pes[, .(num_vinc = .N), .(cpf, ano)][, .N, num_vinc]



### Define vínculo-ano principal:

rais_pes[, dt_min := as.Date(paste0(ano,'-01-01'))]
rais_pes[, dt_max := as.Date(paste0(ano,'-12-31'))]

rais_pes[is.na(dt_adm), dt_adm := dt_min]
rais_pes[, dt_adm_min := pmax(dt_adm, dt_min)]
rais_pes[, dt_deslig_max := pmax(dt_deslig, dt_max , na.rm = T)]

rais_pes[, tempo_trab := dt_deslig_max - dt_adm_min]
rais_pes <- rais_pes[order(-horas_contr, -salario, -tempo_trab)] %>% unique(by = c('cpf','ano'))

str(rais_pes)
rais_pes[, c('Descrição', 'data_adm', 'data_deslig', 'dt_min', 'dt_max', 'dt_adm_min', 'dt_deslig_max') := NULL]
rais_pes %>% setcolorder(c('cpf','ano','uf','codemun','genero','raca_cor','idade','grau_instr','grau_instr_desc','serv'))
rais_pes %>% setkey(cpf,ano)
rais_pes %>% object.size() %>% print(units='Gb')  # 
rais_pes <- rais_pes[!is.na(cpf)]

rais_pes %>% saveRDS('//storage4/bases/RESTRITO/aval_MCMV/data/RAIS/pub_to_priv_trans.rds')


### Cria variável de mudança do setor:

rais_pes[, l_serv := shift(serv), cpf]
rais_pes[, l_horas_ext := shift(ind_horas_extras), cpf]

rais_pes[, pub_to_priv := 0]
rais_pes[serv == 0 & l_serv == 1, pub_to_priv := 1]
rais_pes[, priv_to_pub := 0]
rais_pes[serv == 1 & l_serv == 0, priv_to_pub := 1]


rais_pes[serv == 1 & l_serv == 1, trans_val := 1]
rais_pes[serv == 0 & l_serv == 1, trans_val := 2]
rais_pes[serv == 0 & l_serv == 0, trans_val := 3]
rais_pes[serv == 1 & l_serv == 0, trans_val := 4]


### Cria variável de pós-graduação recentemente obtida:

rais_pes[, .N, .(Doutorado, cpf)][, .N, cpf][, .N, N]
rais_pes[, instr_max := max(grau_instr, na.rm = T), cpf]
rais_pes[, instr_min := min(grau_instr, na.rm = T), cpf]
rais_pes[instr_min < 10 & Mestrado == 1, ano_obt_mestre := min(ano), cpf]
rais_pes[instr_min < 11 & Doutorado == 1, ano_obt_doc := min(ano), cpf]

rais_pes[ano > ano_obt_mestre, anos_desde_mest := ano - ano_obt_mestre]
rais_pes[ano <= ano_obt_mestre | is.na(ano_obt_mestre), anos_desde_mest := 0]
rais_pes[ano > ano_obt_doc, anos_desde_doc := ano - ano_obt_doc]
rais_pes[ano <= ano_obt_doc | is.na(ano_obt_doc), anos_desde_doc := 0]

rais_pes[, pos_recente := 0]
rais_pes[(anos_desde_mest > 0 & anos_desde_mest < 5) | (anos_desde_doc > 0 & anos_desde_doc < 5), pos_recente := 1]
rais_pes[, pos_recente2 := 0]
rais_pes[(anos_desde_mest > 4 & anos_desde_mest < 9) | (anos_desde_doc > 4 & anos_desde_doc < 9), pos_recente2 := 1]


### Teste regs:

logit <- glm(pub_to_priv ~ mulher + raca_cor_desc + Fundamental_1 + Fundamental_2 + Medio 
                         + Superior + Mestrado + pos_recente + pos_recente2 + l_horas_ext + uf + factor(ano),
             data = rais_pes[l_serv == 1],
             family = 'binomial')
summary(logit)



logit2 <- glm(priv_to_pub ~ mulher + raca_cor_desc + Fundamental_1 + Fundamental_2 + Medio 
             + Superior + Mestrado + pos_recente + pos_recente2 + l_horas_ext + uf + factor(ano),
             data = rais_pes[l_serv == 0],
             family = 'binomial')
summary(logit2)


mnl <- multinom(trans_val ~ mulher + raca_cor_desc + Fundamental_1 + Fundamental_2 + Medio 
           + Superior + Mestrado + pos_recente + pos_recente2 + l_horas_ext + uf + factor(ano),
           data = rais_pes)
  
  





### Algumas estatísticas e gráficos:


rais_samp[, .N, anos_desde_mest]
rais_samp[, .N, anos_desde_doc]

rais_samp[, mean(ind_horas_extras, na.rm = T), .(trans_val)]

rais_samp[, .N, dist_last_obs]

rais_samp[, dist_last_obs := (ano - shift(ano)), cpf]

rais_samp[, .N, .(cpf, ano)]

histogram_data <- rais_samp[, .N, .(cpf, ano)][, .('num_anos' = .N), cpf]
histogram_data[, .N, num_anos][order(num_anos)]
histogram_data %>% ggplot(aes(x = num_anos)) + geom_bar()

instr_graph <- rais_samp[, .N, .(ano, grau_instr)][order(ano, grau_instr)][!is.na(ano)]
instr_graph[, total_ano := sum(N), ano]
instr_graph[, prop_instr := N/total_ano]
instr_graph[, grau_instr := grau_instr %>% as.factor()]
instr_graph %>% ggplot(aes(x = ano, y = prop_instr, color = grau_instr)) + geom_line(size = 1)

rais_samp[, mean(grau_instr), ano]

rais_samp[ano == 2007, .N, .(grau_instr,grau_instr_desc)][order(grau_instr)]




