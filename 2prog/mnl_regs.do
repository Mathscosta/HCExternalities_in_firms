jkbgliukbhrtg

cd "HCExternalities_in_firms\3work\regs"
set more off, permanently

import delimited "//storage4/bases/RESTRITO/aval_MCMV/data/RAIS/pub_to_priv_trans.csv", clear
compress

encode raca_cor_desc, generate(raca_cor_enc)
encode uf, generate(uf_enc)
rename mestrado pos_grad

xtset cpf ano, yearly

* Regressões:

logit pub_to_priv i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext i.uf_enc i.ano if l_serv == 1, cluster(cpf) robust
		outreg2 using single_regs, replace ctitle(Logit 1) keep(i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext) tex
margins i.pos_grad i.pos_recente i.pos_recente2, at(pos_grad == 1) post
		outreg2 using margens, replace ctitle(Logit 1) tex
		
logit priv_to_pub i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext i.uf_enc i.ano if l_serv == 0, cluster(cpf) robust
		outreg2 using single_regs, append ctitle(Logit 2) keep(i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext) tex
margins i.pos_grad i.pos_recente i.pos_recente2, at(pos_grad == 1) post
		outreg2 using margens, append ctitle(Logit 2) tex
		
probit pub_to_priv i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext i.uf_enc i.ano if l_serv == 1, cluster(cpf) robust
		outreg2 using single_regs, append ctitle(Probit 1) keep(i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext) tex
margins i.pos_grad i.pos_recente i.pos_recente2, at(pos_grad == 1) post
		outreg2 using margens, append ctitle(Probit 1) tex
		
probit priv_to_pub i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext i.uf_enc i.ano if l_serv == 0, cluster(cpf) robust
		outreg2 using single_regs, append ctitle(Probit 2) keep(i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext) tex
margins i.pos_grad i.pos_recente i.pos_recente2, at(pos_grad == 1) post
		outreg2 using margens, append ctitle(Probit 2) tex
		
		
mlogit trans_val i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext i.uf_enc i.ano, base(3) cluster(cpf) robust
		outreg2 using mnl_reg, replace ctitle(Multinomial Logit) keep(i.mulher i.fundamental_1 i.fundamental_2 i.medio i.superior i.pos_grad i.pos_recente i.pos_recente2 i.l_horas_ext) tex
margins i.pos_grad i.pos_recente i.pos_recente2, at(pos_grad == 1) post
		outreg2 using margens_mnl, replace ctitle(Multinom) tex

