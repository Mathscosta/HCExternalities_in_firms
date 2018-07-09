library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(haven)
library(readxl)
library(ggplot2)
library(tidyr)



rais_pes.file <- list.files("//storage6/bases/DADOS/RESTRITO/RAIS/csv", pattern = 'brasil20', full.names = T) %>% 
  grep(pattern = '\\.csv', value = T)
rais_pes.file <- rais_pes.file[8:17]

rais_pes <- data.table()

n = -1

for(i in rais_pes.file){
  
  ano_rais <- substr(i,48,51) %>% as.numeric
  rais_ano <- fread(i, nrows = n)
  rais_ano <- rais_ano[codemun == 431490]  #filtra para o municipio de porto alegre (RS)
  rais_ano[, ano := ano_rais]
  
  rais_pes <- rbindlist(list(rais_pes, rais_ano), fill = T)
  
}

rais_pes[, .N, .(cpf, ano)]

histogram_data <- rais_pes[, .N, .(cpf, ano)][, .('num_anos' = .N), cpf]
histogram_data[, .N, num_anos][order(num_anos)]
histogram_data %>% ggplot(aes(x = num_anos)) + geom_bar()

instr_graph <- rais_pes[, .N, .(ano, grau_instr)][order(ano, grau_instr)][!is.na(ano)]
instr_graph[, total_ano := sum(N), ano]
instr_graph[, prop_instr := N/total_ano]
instr_graph[, grau_instr := grau_instr %>% as.factor()]
instr_graph %>% ggplot(aes(x = ano, y = prop_instr, color = grau_instr)) + geom_line(size = 1)

rais_pes[, mean(grau_instr), ano]

rais_pes[, .N, .(id_estab, cpf)]
rais_pes[, .N, .(id_estab, cpf)][, .N, cpf]
rais_pes[, .N, .(id_estab, cpf)][, .('num_empresas' = .N), cpf][, .N, num_empresas][order(num_empresas)]


rais_pes[, cbo_gg := cbo2002 %>% str_pad(5,'left',0) %>% substr(1,1) ]
rais_pes[, .N, cbo_gg][order(cbo_gg)]
rais_pes[, total_cbo := .N, cbo_gg]
tab_cbo_instr <- rais_pes[, .N, .(cbo_gg, total_cbo, grau_instr)][, N/total_cbo, .(cbo_gg,grau_instr)][order(cbo_gg, grau_instr)] %>% spread(grau_instr, V1)


