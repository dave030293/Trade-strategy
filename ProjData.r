Pro <- na.omit(ProjData) #去除空值 注意: 只接受NA，要先去excel把缺值全轉成NA
tsmc_ret=-diff(log(Pro$TSMC), lag=1) #價格轉成log報酬
chain_ret=-diff(log(Pro$Chain_tech), lag=1)
guc_ret=-diff(log(Pro$GUC), lag=1)
Asr_ret=-diff(log(Pro$Asrock), lag=1)
tul_ret=-diff(log(Pro$TUL), lag=1)
nvda_ret=-diff(log(Pro$NVDA), lag=1)
amd_ret=-diff(log(Pro$AMD), lag=1)
intc_ret=-diff(log(Pro$INTC), lag=1)
bic_ret=-diff(log(Pro$Bitcoin), lag=1)
eth_ret=-diff(log(Pro$Ethereum), lag=1)
lit_ret=-diff(log(Pro$Litecoin), lag=1)
usd_ret=-diff(log(Pro$USDX), lag=1)
gold_ret=-diff(log(Pro$Gold), lag=1)
oil_ret=-diff(log(Pro$crude_oil), lag=1)
us_ret=-diff(log(Pro$SP500), lag=1)
tw_ret=-diff(log(Pro$TW_index), lag=1)

#相關係數矩陣
a=data.frame(tsmc_ret,chain_ret,amd_ret,Asr_ret,bic_ret,eth_ret,gold_ret,guc_ret,intc_ret,lit_ret,nvda_ret,oil_ret,tul_ret,tw_ret,usd_ret,us_ret)
amt=data.matrix(a) 
cormtx=cor(amt,use="complete.obs", method="kendall")

#迴歸模型
model1=lm(formula= tsmc_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + tw_ret)
summary(model1)

model2=lm(formula= tsmc_ret ~ bic_ret + eth_ret + lit_ret)
summary(model2)

model3=lm(formula= tsmc_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret )
summary(model3)

model4=lm(formula= chain_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + tw_ret)
summary(model4)

model5=lm(formula= chain_ret ~ bic_ret + eth_ret + lit_ret )
summary(model5)

model6=lm(formula= chain_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret)
summary(model6)

model7=lm(formula= guc_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + tw_ret)
summary(model7)

model8=lm(formula= guc_ret ~ bic_ret + eth_ret + lit_ret )
summary(model8)

model9=lm(formula= guc_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model9)

model10=lm(formula= Asr_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + tw_ret)
summary(model10)

model11=lm(formula= Asr_ret ~ bic_ret + eth_ret + lit_ret )
summary(model11)

model12=lm(formula= Asr_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model12)

model13=lm(formula= tul_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + tw_ret)
summary(model13)

model14=lm(formula= tul_ret ~ bic_ret + eth_ret + lit_ret )
summary(model14)

model15=lm(formula= tul_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model15)

model16=lm(formula= amd_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + us_ret)
summary(model16)

model17=lm(formula= amd_ret ~ bic_ret + eth_ret + lit_ret )
summary(model17)

model18=lm(formula= amd_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model18)

model19=lm(formula= intc_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + us_ret)
summary(model19)

model20=lm(formula= intc_ret ~ bic_ret + eth_ret + lit_ret )
summary(model20)

model21=lm(formula= intc_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model21)

model22=lm(formula= nvda_ret ~ bic_ret + eth_ret + lit_ret + usd_ret + gold_ret + oil_ret + us_ret)
summary(model22)

model23=lm(formula= nvda_ret ~ bic_ret + eth_ret + lit_ret )
summary(model23)

model24=lm(formula= nvda_ret ~ bic_ret + eth_ret + lit_ret+usd_ret + gold_ret + oil_ret )
summary(model24)

require(zoo)
bic_ret = zoo(c(bic_ret))
eth_ret = zoo(c(eth_ret))
lit_ret = zoo(c(lit_ret))
gold_ret = zoo(c(gold_ret))
oil_ret = zoo(c(oil_ret))
us_ret = zoo(c(us_ret))
usd_ret = zoo(c(usd_ret))
tw_ret = zoo(c(tw_ret))

#加入落後項的迴歸方程式
require(dynlm)

model25=dynlm(nvda_ret ~lag(bic_ret,-1) + lag(eth_ret,-1)  + lag(lit_ret, -1) + lag(usd_ret,-1) + lag(gold_ret, - 1) + lag(oil_ret, - 1) )
summary(model25)


#VAR
library(tseries)
library(vars)

tsmc_ret_ts= data.frame(tsmc_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
amd_ret_ts= data.frame(amd_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
Asr_ret_ts= data.frame(Asr_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
chain_ret_ts= data.frame(chain_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
guc_ret_ts= data.frame(guc_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
intc_ret_ts= data.frame(intc_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
nvda_ret_ts= data.frame(nvda_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)
tul_ret_ts= data.frame(tul_ret,bic_ret,eth_ret,gold_ret,lit_ret,oil_ret,tw_ret,usd_ret,us_ret)

#做成data frame是因為他只能加一個變數，不能設定y和x，所以會全部做出來
#以下VAR結果只須看第一個公司股票對所有因子

mod=VAR(y=tsmc_ret_ts,p=2,ic = AIC)
summary(mod)
mod1=VAR(y=amd_ret_ts,p=2,ic = AIC)
summary(mod1)
mod2=VAR(y=Asr_ret_ts,p=2,ic = AIC)
summary(mod2)
mod3=VAR(y=chain_ret_ts,p=2,ic = AIC)
summary(mod3)
mod4=VAR(y=guc_ret_ts,p=2,ic = AIC)
summary(mod4)
mod5=VAR(y=intc_ret_ts,p=2,ic = AIC)
summary(mod5)
mod6=VAR(y=nvda_ret_ts,p=2,ic = AIC)
summary(mod6)
mod7=VAR(y=tul_ret_ts,p=2,ic = AIC)
summary(mod7)


#Granger因果關係
grangertest(bic_ret,tsmc_ret,na.action = na.omit)
grangertest(eth_ret,tsmc_ret,na.action = na.omit)
grangertest(lit_ret,tsmc_ret,na.action = na.omit)

grangertest(bic_ret,chain_ret,na.action = na.omit)
grangertest(eth_ret,chain_ret,na.action = na.omit)
grangertest(lit_ret,chain_ret,na.action = na.omit)

grangertest(bic_ret,amd_ret,na.action = na.omit)
grangertest(eth_ret,amd_ret,na.action = na.omit)
grangertest(lit_ret,amd_ret,na.action = na.omit)

grangertest(bic_ret,Asr_ret,na.action = na.omit) #很顯著
grangertest(eth_ret,Asr_ret,na.action = na.omit)
grangertest(lit_ret,Asr_ret,na.action = na.omit)

grangertest(bic_ret,guc_ret,na.action = na.omit) #顯著
grangertest(eth_ret,guc_ret,na.action = na.omit)
grangertest(lit_ret,guc_ret,na.action = na.omit)

grangertest(bic_ret,intc_ret,na.action = na.omit) 
grangertest(eth_ret,intc_ret,na.action = na.omit)
grangertest(lit_ret,intc_ret,na.action = na.omit)

grangertest(bic_ret,nvda_ret,na.action = na.omit) 
grangertest(eth_ret,nvda_ret,na.action = na.omit)
grangertest(lit_ret,nvda_ret,na.action = na.omit)

grangertest(bic_ret,tul_ret,na.action = na.omit) #很顯著
grangertest(eth_ret,tul_ret,na.action = na.omit)
grangertest(lit_ret,tul_ret,na.action = na.omit)

#VAR公式的所有函數
VAR(x, p = 1, type = c("const", "trend", "both", "none"),
    season = NULL, exogen = NULL, lag.max = NULL,
    ic = c("AIC", "HQ", "SC", "FPE"))
# S3 method for varest
print(x, digits = max(3, getOption("digits") - 3), ...)
#選擇落後期數
VARselect(amt, lag.max = 30, type="const")

#nn

install.packages("neuralnet")
library(neuralnet)


#把每天股價報酬轉為虛擬變數y做為預測結果的正確答案(漲=1，跌=0)
amd_ret_d = zoo(amd_ret)
Asr_ret_d = zoo(Asr_ret)
chain_ret_d = zoo(chain_ret)
guc_ret_d = zoo(guc_ret)
intc_ret_d = zoo(intc_ret)
nvda_ret_d = zoo(nvda_ret)
tsmc_ret_d = zoo(tsmc_ret)
tul_ret_d = zoo(tul_ret)

amd_ret_d=ifelse(amd_ret_d>0,1,0)
Asr_ret_d=ifelse(Asr_ret_d>0,1,0)
chain_ret_d=ifelse(chain_ret_d>0,1,0)
guc_ret_d=ifelse(guc_ret_d>0,1,0)
intc_ret_d=ifelse(intc_ret_d>0,1,0)
nvda_ret_d=ifelse(nvda_ret_d>0,1,0)
tsmc_ret_d=ifelse(tsmc_ret_d>0,1,0)
tul_ret_d=ifelse(tul_ret_d>0,1,0)

lagged = data.frame(lag(bic_ret,-1),lag(eth_ret,-1),lag(lit_ret, -1),lag(usd_ret,-1),lag(gold_ret,-1),lag(oil_ret,-1),lag(us_ret,-1),lag(tw_ret,-1))
amd=data.frame(amd_ret_d,c(lag(amd_ret,-1)))
amd_ret_nn = merge(data.frame(amd, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

Asr=data.frame(Asr_ret_d,c(lag(Asr_ret,-1)))
Asr_ret_nn = merge(data.frame(Asr, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

chain=data.frame(chain_ret_d,c(lag(chain_ret,-1)))
chain_ret_nn = merge(data.frame(chain, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

guc=data.frame(guc_ret_d,c(lag(guc_ret,-1)))
guc_ret_nn = merge(data.frame(guc, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

intc=data.frame(intc_ret_d,c(lag(intc_ret,-1)))
intc_ret_nn = merge(data.frame(intc, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

nvda=data.frame(nvda_ret_d,c(lag(nvda_ret,-1)))
nvda_ret_nn = merge(data.frame(nvda, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

tul=data.frame(tul_ret_d,c(lag(tul_ret,-1)))
tul_ret_nn = merge(data.frame(tul, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

tsmc=data.frame(tsmc_ret_d,c(lag(tsmc_ret,-1)))
tsmc_ret_nn = merge(data.frame(tsmc, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]


#抓隨機80%樣本當訓練用
id = sample(1:nrow(amd_ret_nn),0.8*nrow(amd_ret_nn))
tr.amd = amd_ret_nn[id,]
ts.amd = amd_ret_nn[-id,]

id1 = sample(1:nrow(Asr_ret_nn),0.8*nrow(Asr_ret_nn))
tr.Asr = Asr_ret_nn[id1,]
ts.Asr = Asr_ret_nn[-id1,]

id2 = sample(1:nrow(chain_ret_nn),0.8*nrow(chain_ret_nn))
tr.chain = chain_ret_nn[id2,]
ts.chain = chain_ret_nn[-id2,]

id3 = sample(1:nrow(guc_ret_nn),0.8*nrow(guc_ret_nn))
tr.guc = guc_ret_nn[id3,]
ts.guc = guc_ret_nn[-id3,]

id4 = sample(1:nrow(intc_ret_nn),0.8*nrow(intc_ret_nn))
tr.intc = intc_ret_nn[id4,]
ts.intc = intc_ret_nn[-id4,]

id5 = sample(1:nrow(nvda_ret_nn),0.8*nrow(nvda_ret_nn))
tr.nvda = nvda_ret_nn[id5,]
ts.nvda = nvda_ret_nn[-id5,]

id6 = sample(1:nrow(tul_ret_nn),0.8*nrow(tul_ret_nn))
tr.tul = tul_ret_nn[id6,]
ts.tul = tul_ret_nn[-id6,]

id7 = sample(1:nrow(tsmc_ret_nn),0.8*nrow(tsmc_ret_nn))
tr.tsmc = tsmc_ret_nn[id7,]
ts.tsmc = tsmc_ret_nn[-id7,]


#建構倒傳遞NN模型
attach(tr.amd)
tr.amd<- na.omit(tr.amd)
result1=neuralnet(amd_ret_d ~ c.lag.amd_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,9),data=tr.amd)
result_num1=neuralnet(amd_ret ~ c.lag.amd_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,9),data=tr.amd)

#print(result1)
#plot(result1)

tr.Asr<- na.omit(tr.Asr)
result2=neuralnet(Asr_ret_d ~  c.lag.Asr_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,8),data=tr.Asr)
result_num2=neuralnet(Asr_ret ~ c.lag.amd_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,9),data=tr.Asr)

tr.chain<- na.omit(tr.chain)
result3=neuralnet(chain_ret_d ~ c.lag.chain_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,8),data=tr.chain)

tr.guc<- na.omit(tr.guc)
result4=neuralnet(guc_ret_d ~ c.lag.guc_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10),data=tr.guc)

tr.intc<- na.omit(tr.intc)
result5=neuralnet(intc_ret_d ~ c.lag.intc_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10),data=tr.intc)

tr.nvda<- na.omit(tr.nvda)
result6=neuralnet(nvda_ret_d ~ c.lag.nvda_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(13,11),data=tr.nvda)

tr.tul<- na.omit(tr.tul)
result7=neuralnet(tul_ret_d ~ c.lag.tul_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10),data=tr.tul)

tr.tsmc<- na.omit(tr.tsmc)
result8=neuralnet(tsmc_ret_d ~ c.lag.tsmc_ret...1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10),data=tr.tsmc)

#計算預測正確率(testing+training)
amdpredx=compute(result1,ts.amd[,2:10])
amdpredy=ifelse(amdpredx$net.result>0,1,0)
ctable=table(ts.amd$amd_ret_d,amdpredy)
sum(diag(ctable))/sum(ctable)

amdpred99=compute(result1,tr.amd[,2:10])
amdpred00=ifelse(amdpred99$net.result>0,1,0)
ctable.amd=table(tr.amd$amd_ret_d,amdpred00)
sum(diag(ctable.amd))/sum(ctable.amd)

Asrpred1=compute(result2,ts.Asr[,2:10])
Asrpred2=ifelse(Asrpred1$net.result>0,1,0)
ctable1=table(ts.Asr$Asr_ret_d,Asrpred2)
sum(diag(ctable1))/sum(ctable1)

Asrpred99=compute(result2,tr.Asr[,2:10])
Asrpred00=ifelse(Asrpred99$net.result>0,1,0)
ctable.Asr=table(tr.Asr$Asr_ret_d,Asrpred00)
sum(diag(ctable.Asr))/sum(ctable.Asr)

chainpred1=compute(result3,ts.chain[,2:10])
chainpred2=ifelse(chainpred1$net.result>0,1,0)
ctable2=table(ts.chain$chain_ret_d,chainpred2)
sum(diag(ctable2))/sum(ctable2)

chainpred99=compute(result3,tr.chain[,2:10])
chainpred00=ifelse(chainpred99$net.result>0,1,0)
ctable.chain=table(tr.chain$chain_ret_d,chainpred00)
sum(diag(ctable.chain))/sum(ctable.chain)

gucpred1=compute(result4,ts.guc[,2:10])
gucpred2=ifelse(gucpred1$net.result>0,1,0)
ctable3=table(ts.guc$guc_ret_d,gucpred2)
sum(diag(ctable3))/sum(ctable3)

gucpred99=compute(result4,tr.guc[,2:10])
gucpred00=ifelse(gucpred99$net.result>0,1,0)
ctable.guc=table(tr.guc$guc_ret_d,gucpred00)
sum(diag(ctable.guc))/sum(ctable.guc)

intcpred1=compute(result5,ts.intc[,2:10])
intcpred2=ifelse(intcpred1$net.result>0,1,0)
ctable4=table(ts.intc$intc_ret_d,intcpred2)
sum(diag(ctable4))/sum(ctable4)

intcpred99=compute(result5,tr.intc[,2:10])
intcpred00=ifelse(intcpred99$net.result>0,1,0)
ctable.intc=table(tr.intc$intc_ret_d,intcpred00)
sum(diag(ctable.intc))/sum(ctable.intc)

nvdapred1=compute(result6,ts.nvda[,2:10])
nvdapred2=ifelse(nvdapred1$net.result>0,1,0)
ctable5=table(ts.nvda$nvda_ret_d,nvdapred2)
sum(diag(ctable5))/sum(ctable5)

nvdapred99=compute(result6,tr.nvda[,2:10])
nvdapred00=ifelse(nvdapred99$net.result>0,1,0)
ctable.nvda=table(tr.nvda$nvda_ret_d,nvdapred00)
sum(diag(ctable.nvda))/sum(ctable.nvda)

tulpred1=compute(result7,ts.tul[,2:10])
tulpred2=ifelse(tulpred1$net.result>0,1,0)
ctable6=table(ts.tul$tul_ret_d,tulpred2)
sum(diag(ctable6))/sum(ctable6)

tulpred99=compute(result7,tr.tul[,2:10])
tulpred00=ifelse(tulpred99$net.result>0,1,0)
ctable.tul=table(tr.tul$tul_ret_d,tulpred00)
sum(diag(ctable.tul))/sum(ctable.tul)

tsmcpred1=compute(result8,ts.tsmc[,2:10])
tsmcpred2=ifelse(tsmcpred1$net.result>0,1,0)
ctable7=table(ts.tsmc$tsmc_ret_d,tsmcpred2)
sum(diag(ctable7))/sum(ctable7)

tsmcpred99=compute(result8,tr.tsmc[,2:10])
tsmcpred00=ifelse(tsmcpred99$net.result>0,1,0)
ctable.tsmc=table(tr.tsmc$tsmc_ret_d,tsmcpred00)
sum(diag(ctable.tsmc))/sum(ctable.tsmc)


#設計交易策略

library(quantmod)
library(dplyr)
install.packages("anytime")
library(anytime)
Pro$Date =  as.POSIXct(Pro$Date)
amdpred=neuralnet::compute(result1,amd_ret_nn[,-1])
amdpredict=na.omit(round(amdpred$net.result))
Asrpred=neuralnet::compute(result2,Asr_ret_nn[,-1])
Asrpredict=na.omit(round(Asrpred$net.result))
chainpred=neuralnet::compute(result3,chain_ret_nn[,-1])
chainpredict=na.omit(round(chainpred$net.result))
gucpred=neuralnet::compute(result4,guc_ret_nn[,-1])
gucpredict=na.omit(round(gucpred$net.result))
intcpred=neuralnet::compute(result5,intc_ret_nn[,-1])
intcpredict=na.omit(round(intcpred$net.result))
nvdapred=neuralnet::compute(result6,nvda_ret_nn[,-1])
nvdapredict=na.omit(round(nvdapred$net.result))
tulpred=neuralnet::compute(result7,tul_ret_nn[,-1])
tulpredict=na.omit(round(tulpred$net.result))
tsmcpred=neuralnet::compute(result8,tsmc_ret_nn[,-1])
tsmcpredict=na.omit(round(tsmcpred$net.result))

NNtable=data.frame(amdpredict,Asrpredict,chainpredict,gucpredict,intcpredict,nvdapredict,tulpredict,tsmcpredict)


#策略1 當沖: 假設昨日收盤=今日開盤，以落後一期的資料判斷隔日漲跌，會漲就開盤買收盤賣
amd_ret1=amd_ret[1:804]
amdprofit = amdpredict*(amd_ret1+log(0.997/1.003))#買賣成本各為0.003
amd_EC = 1+cumsum(amdprofit) 
plot(amd_EC)
#print(profit)
amd_meanRet <- mean(amdprofit)
amd_sdRet <- sd(amdprofit) 
amd_tradeNums <- sum(amdpredict) 
amd_winRatio <- sum(as.numeric(amdprofit>0))/amd_tradeNums
amd_maxRet <- max(amdprofit)
amd_minRet <- min(amdprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(amd_meanRet*100,2)," %\n",
           "交易次數: ",amd_tradeNums," 次\n",
           "勝率: ",round(amd_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(amd_sdRet*100,2)," %\n",
           "最大報酬率: ",round(amd_maxRet*100,2)," %\n",
           "最小報酬率: ",round(amd_minRet*100,2)," %\n"))
          
Asr_ret1=Asr_ret[1:804]
Asrprofit = Asrpredict*(Asr_ret1+log(0.997/1.003))#買賣成本各為0.003
Asr_EC = 1+cumsum(Asrprofit) 
plot(Asr_EC)
#print(profit)
Asr_meanRet <- mean(Asrprofit)
Asr_sdRet <- sd(Asrprofit) 
Asr_tradeNums <- sum(Asrpredict) 
Asr_winRatio <- sum(as.numeric(Asrprofit>0))/Asr_tradeNums
Asr_maxRet <- max(Asrprofit)
Asr_minRet <- min(Asrprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(Asr_meanRet*100,2)," %\n",
           "交易次數: ",Asr_tradeNums," 次\n",
           "勝率: ",round(Asr_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(Asr_sdRet*100,2)," %\n",
           "最大報酬率: ",round(Asr_maxRet*100,2)," %\n",
           "最小報酬率: ",round(Asr_minRet*100,2)," %\n"))

chain_ret1=chain_ret[1:804]
chainprofit = chainpredict*(chain_ret1+log(0.997/1.003))#買賣成本各為0.003
chain_EC = 1+cumsum(chainprofit) 
plot(chain_EC)
#print(profit)
chain_meanRet <- mean(chainprofit)
chain_sdRet <- sd(chainprofit) 
chain_tradeNums <- sum(chainpredict) 
chain_winRatio <- sum(as.numeric(chainprofit>0))/chain_tradeNums
chain_maxRet <- max(chainprofit)
chain_minRet <- min(chainprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(chain_meanRet*100,2)," %\n",
           "交易次數: ",chain_tradeNums," 次\n",
           "勝率: ",round(chain_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(chain_sdRet*100,2)," %\n",
           "最大報酬率: ",round(chain_maxRet*100,2)," %\n",
           "最小報酬率: ",round(chain_minRet*100,2)," %\n"))

guc_ret1=guc_ret[1:804]
gucprofit = gucpredict*(guc_ret1+log(0.997/1.003))#買賣成本各為0.003
guc_EC = 1+cumsum(gucprofit) 
plot(guc_EC)
#print(profit)
guc_meanRet <- mean(gucprofit)
guc_sdRet <- sd(gucprofit) 
guc_tradeNums <- sum(gucpredict) 
guc_winRatio <- sum(as.numeric(gucprofit>0))/guc_tradeNums
guc_maxRet <- max(gucprofit)
guc_minRet <- min(gucprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(guc_meanRet*100,2)," %\n",
           "交易次數: ",guc_tradeNums," 次\n",
           "勝率: ",round(guc_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(guc_sdRet*100,2)," %\n",
           "最大報酬率: ",round(guc_maxRet*100,2)," %\n",
           "最小報酬率: ",round(guc_minRet*100,2)," %\n"))

intc_ret1=intc_ret[1:804]
intcprofit = intcpredict*(intc_ret1+log(0.997/1.003))#買賣成本各為0.003
intc_EC = 1+cumsum(intcprofit) 
plot(intc_EC)
#print(profit)
intc_meanRet <- mean(intcprofit)
intc_sdRet <- sd(intcprofit) 
intc_tradeNums <- sum(intcpredict) 
intc_winRatio <- sum(as.numeric(intcprofit>0))/intc_tradeNums
intc_maxRet <- max(intcprofit)
intc_minRet <- min(intcprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(intc_meanRet*100,2)," %\n",
           "交易次數: ",intc_tradeNums," 次\n",
           "勝率: ",round(intc_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(intc_sdRet*100,2)," %\n",
           "最大報酬率: ",round(intc_maxRet*100,2)," %\n",
           "最小報酬率: ",round(intc_minRet*100,2)," %\n"))

nvda_ret1=nvda_ret[1:804]
nvdaprofit = nvdapredict*(nvda_ret1+log(0.997/1.003))#買賣成本各為0.003
nvda_EC = 1+cumsum(nvdaprofit) 
plot(nvda_EC)
#print(profit)
nvda_meanRet <- mean(nvdaprofit)
nvda_sdRet <- sd(nvdaprofit) 
nvda_tradeNums <- sum(nvdapredict) 
nvda_winRatio <- sum(as.numeric(nvdaprofit>0))/nvda_tradeNums
nvda_maxRet <- max(nvdaprofit)
nvda_minRet <- min(nvdaprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(nvda_meanRet*100,2)," %\n",
           "交易次數: ",nvda_tradeNums," 次\n",
           "勝率: ",round(nvda_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(nvda_sdRet*100,2)," %\n",
           "最大報酬率: ",round(nvda_maxRet*100,2)," %\n",
           "最小報酬率: ",round(nvda_minRet*100,2)," %\n"))

tsmc_ret1=tsmc_ret[1:804]
tsmcprofit = tsmcpredict*(tsmc_ret1+log(0.997/1.003))#買賣成本各為0.003
tsmc_EC = 1+cumsum(tsmcprofit) 
plot(tsmc_EC)
#print(profit)
tsmc_meanRet <- mean(tsmcprofit)
tsmc_sdRet <- sd(tsmcprofit) 
tsmc_tradeNums <- sum(tsmcpredict) 
tsmc_winRatio <- sum(as.numeric(tsmcprofit>0))/tsmc_tradeNums
tsmc_maxRet <- max(tsmcprofit)
tsmc_minRet <- min(tsmcprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(tsmc_meanRet*100,2)," %\n",
           "交易次數: ",tsmc_tradeNums," 次\n",
           "勝率: ",round(tsmc_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(tsmc_sdRet*100,2)," %\n",
           "最大報酬率: ",round(tsmc_maxRet*100,2)," %\n",
           "最小報酬率: ",round(tsmc_minRet*100,2)," %\n"))

tul_ret1=tul_ret[1:804]
tulprofit = tulpredict*(tul_ret1+log(0.997/1.003))#買賣成本各為0.003
tul_EC = 1+cumsum(tulprofit) 
plot(tul_EC)
#print(profit)
tul_meanRet <- mean(tulprofit)
tul_sdRet <- sd(tulprofit) 
tul_tradeNums <- sum(tulpredict) 
tul_winRatio <- sum(as.numeric(tulprofit>0))/tul_tradeNums
tul_maxRet <- max(tulprofit)
tul_minRet <- min(tulprofit)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(tul_meanRet*100,2)," %\n",
           "交易次數: ",tul_tradeNums," 次\n",
           "勝率: ",round(tul_winRatio*100,2)," %\n",
           "報酬率標準差: ",round(tul_sdRet*100,2)," %\n",
           "最大報酬率: ",round(tul_maxRet*100,2)," %\n",
           "最小報酬率: ",round(tul_minRet*100,2)," %\n"))

#用return來做signal

amd1=data.frame(amd_ret,c(lag(amd_ret,k=-1)))
amd_ret_nn1 = merge(data.frame(amd1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

Asr1=data.frame(Asr_ret,c(lag(Asr_ret,k=-1)))
Asr_ret_nn1 = merge(data.frame(Asr1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

chain1=data.frame(chain_ret,c(lag(chain_ret,k=-1)))
chain_ret_nn1 = merge(data.frame(chain1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

guc1=data.frame(guc_ret,c(lag(guc_ret,k=-1)))
guc_ret_nn1 = merge(data.frame(guc1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

intc1=data.frame(intc_ret,c(lag(intc_ret,k=-1)))
intc_ret_nn1 = merge(data.frame(intc1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

nvda1=data.frame(nvda_ret,c(lag(nvda_ret,k=-1)))
nvda_ret_nn1 = merge(data.frame(nvda1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

tul1=data.frame(tul_ret,c(lag(tul_ret,k=-1)))
tul_ret_nn1= merge(data.frame(tul1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]

tsmc1=data.frame(tsmc_ret,c(lag(tsmc_ret,k=-1)))
tsmc_ret_nn1 = merge(data.frame(tsmc1, row.names=NULL), data.frame(lagged, row.names=NULL), by = 0, all = TRUE)[-1]


#抓隨機80%樣本當訓練用 (Ret 重複一次)
idk = sample(1:nrow(amd_ret_nn1),0.8*nrow(amd_ret_nn1))
tr.amd1 = amd_ret_nn1[idk,]
ts.amd1 = amd_ret_nn1[-idk,]

id1k = sample(1:nrow(Asr_ret_nn1),0.8*nrow(Asr_ret_nn1))
tr.Asr1 = Asr_ret_nn1[id1k,]
ts.Asr1 = Asr_ret_nn1[-id1k,]

id2k = sample(1:nrow(chain_ret_nn1),0.8*nrow(chain_ret_nn1))
tr.chain1 = chain_ret_nn1[id2k,]
ts.chain1 = chain_ret_nn1[-id2k,]

id3k = sample(1:nrow(guc_ret_nn1),0.8*nrow(guc_ret_nn1))
tr.guc1 = guc_ret_nn1[id3k,]
ts.guc1 = guc_ret_nn1[-id3k,]

id4k = sample(1:nrow(intc_ret_nn1),0.8*nrow(intc_ret_nn1))
tr.intc1 = intc_ret_nn1[id4k,]
ts.intc1 = intc_ret_nn1[-id4k,]

id5k = sample(1:nrow(nvda_ret_nn1),0.8*nrow(nvda_ret_nn1))
tr.nvda1 = nvda_ret_nn1[id5k,]
ts.nvda1 = nvda_ret_nn1[-id5k,]

id6k = sample(1:nrow(tul_ret_nn1),0.8*nrow(tul_ret_nn1))
tr.tul1 = tul_ret_nn1[id6k,]
ts.tul1 = tul_ret_nn1[-id6k,]

id7k = sample(1:nrow(tsmc_ret_nn1),0.8*nrow(tsmc_ret_nn1))
tr.tsmc1 = tsmc_ret_nn1[id7k,]
ts.tsmc1 = tsmc_ret_nn1[-id7k,]


#建構倒傳遞NN模型  (Ret 重複一次)
tr.amd1<- na.omit(tr.amd1)
result_num1=neuralnet(amd_ret ~ c.lag.amd_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,9,10),data=tr.amd1)

tr.Asr1<- na.omit(tr.Asr1)
result_num2=neuralnet(Asr_ret ~  c.lag.Asr_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,8,10),data=tr.Asr1)

tr.chain1<- na.omit(tr.chain1)
result_num3=neuralnet(chain_ret ~ c.lag.chain_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(8,8,10),data=tr.chain1)

tr.guc1<- na.omit(tr.guc1)
result_num4=neuralnet(guc_ret ~ c.lag.guc_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10,10),data=tr.guc1)

tr.intc1<- na.omit(tr.intc1)
result_num5=neuralnet(intc_ret ~ c.lag.intc_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10,10),data=tr.intc1)

tr.nvda1<- na.omit(tr.nvda1)
result_num6=neuralnet(nvda_ret ~ c.lag.nvda_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(13,11,10),data=tr.nvda1)

tr.tul1<- na.omit(tr.tul1)
result_num7=neuralnet(tul_ret ~ c.lag.tul_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10,10),data=tr.tul1)

tr.tsmc1<- na.omit(tr.tsmc1)
result_num8=neuralnet(tsmc_ret ~ c.lag.tsmc_ret..k....1..+lag.bic_ret...1.+lag.eth_ret...1.+lag.lit_ret...1.+lag.usd_ret...1.+lag.gold_ret...1.+lag.oil_ret...1.+lag.us_ret...1.+lag.tw_ret...1., hidden = c(10,10,10),data=tr.tsmc1)

amdpred1=neuralnet::compute(result_num1,amd_ret_nn1[,-1])
amdpredict1=na.omit(amdpred1$net.result)
Asrpred1=neuralnet::compute(result_num2,Asr_ret_nn1[,-1])
Asrpredict1=na.omit(Asrpred1$net.result)
chainpred1=neuralnet::compute(result_num3,chain_ret_nn1[,-1])
chainpredict1=na.omit(chainpred1$net.result)
gucpred1=neuralnet::compute(result_num4,guc_ret_nn1[,-1])
gucpredict1=na.omit(gucpred1$net.result)
intcpred1=neuralnet::compute(result_num5,intc_ret_nn1[,-1])
intcpredict1=na.omit(intcpred1$net.result)
nvdapred1=neuralnet::compute(result_num6,nvda_ret_nn1[,-1])
nvdapredict1=na.omit(nvdapred1$net.result)
tulpred1=neuralnet::compute(result_num7,tul_ret_nn1[,-1])
tulpredict1=na.omit(tulpred1$net.result)
tsmcpred1=neuralnet::compute(result_num8,tsmc_ret_nn1[,-1])
tsmcpredict1=na.omit(tsmcpred1$net.result)

NNtable1=data.frame(amdpredict1,Asrpredict1,chainpredict1,gucpredict1,intcpredict1,nvdapredict1,tulpredict1,tsmcpredict1)

#計算MSE
install.packages("reticulate")
library("reticulate")
install.packages("os")
conda_create("r-reticulate")
conda_install("r-reticulate","numpy")



mse_amd=sum((amdpredict1-amd_ret1[1:803])**2)/length(amd_ret1)
rmse_amd=mse_amd ** 0.5
mse_Asr=sum((Asrpredict1-Asr_ret1[1:803])**2)/length(Asr_ret1)
rmse_Asr=mse_Asr ** 0.5
mse_chain=sum((chainpredict1-chain_ret1[1:803])**2)/length(chain_ret1)
rmse_chain=mse_chain ** 0.5
mse_guc=sum((gucpredict1-guc_ret1[1:803])**2)/length(guc_ret1)
rmse_guc=mse_guc ** 0.5
mse_intc=sum((intcpredict1-intc_ret1[1:803])**2)/length(intc_ret1)
rmse_intc=mse_intc ** 0.5
mse_nvda=sum((nvdapredict1-nvda_ret1[1:803])**2)/length(nvda_ret1)
rmse_nvda=mse_nvda ** 0.5
mse_tsmc=sum((tsmcpredict1-tsmc_ret1[1:803])**2)/length(tsmc_ret1)
rmse_tsmc=mse_tsmc ** 0.5
mse_tul=sum((tulpredict1-tul_ret1[1:803])**2)/length(tul_ret1)
rmse_tul=mse_tul ** 0.5

mae_amd=sum(abs(amdpredict1-amd_ret1[1:803]))/length(amd_ret1)
mae_Asr=sum(abs(Asrpredict1-Asr_ret1[1:803]))/length(Asr_ret1)
mae_chain=sum(abs(chainpredict1-chain_ret1[1:803]))/length(chain_ret1)
mae_guc=sum(abs(gucpredict1-guc_ret1[1:803]))/length(guc_ret1)
mae_intc=sum(abs(intcpredict1-intc_ret1[1:803]))/length(intc_ret1)
mae_nvda=sum(abs(nvdapredict1-nvda_ret1[1:803]))/length(nvda_ret1)
mae_tsmc=sum(abs(tsmcpredict1-tsmc_ret1[1:803]))/length(tsmc_ret1)
mae_tul=sum(abs(tulpredict1-tul_ret1[1:803]))/length(tul_ret1)

install.packages("DescTools")
library(DescTools)
library(forecast)
U_amd=TheilU(amd_ret1[1:803],amdpredict1, type =1, na.rm = FALSE)
U_Asr=TheilU(Asr_ret1[1:803],Asrpredict1, type = 1, na.rm = FALSE)
U_chain=TheilU(chain_ret1[1:803],chainpredict1, type = 1, na.rm = FALSE)
U_guc=TheilU(guc_ret1[1:803],gucpredict1, type = 1, na.rm = FALSE)
U_intc=TheilU(intc_ret1[1:803],intcpredict1, type = 1, na.rm = FALSE)
U_nvda=TheilU(nvda_ret1[1:803],nvdapredict1, type = 1, na.rm = FALSE)
U_tsmc=TheilU(tsmc_ret1[1:803],tsmcpredict1, type = 1, na.rm = FALSE)
U_tul=TheilU(tul_ret1[1:803],tulpredict1, type = 1, na.rm = FALSE)

U2_amd=TheilU(amd_ret1[1:803],amdpredict1, type =2, na.rm = FALSE)
U2_Asr=TheilU(Asr_ret1[1:803],Asrpredict1, type = 2, na.rm = FALSE)
U2_chain=TheilU(chain_ret1[1:803],chainpredict1, type = 2, na.rm = FALSE)
U2_guc=TheilU(guc_ret1[1:803],gucpredict1, type = 2, na.rm = FALSE)
U2_intc=TheilU(intc_ret1[1:803],intcpredict1, type = 2, na.rm = FALSE)
U2_nvda=TheilU(nvda_ret1[1:803],nvdapredict1, type = 2, na.rm = FALSE)
U2_tsmc=TheilU(tsmc_ret1[1:803],tsmcpredict1, type = 2, na.rm = FALSE)
U2_tul=TheilU(tul_ret1[1:803],tulpredict1, type = 2, na.rm = FALSE)
#策略2 當沖: 假設昨日收盤=今日開盤，以落後一期的資料預測隔日報酬，預測超過賺1%以上就進場
amd_ret1=amd_ret[1:804]
amdprofit1 = ifelse(amdpredict1>0.012,amd_ret1+log(0.997/1.003),0)#買賣成本各為0.003
amd_EC1 = 1+cumsum(amdprofit1) 
plot(amd_EC1)

amd_meanRet1 <- mean(amdprofit1)
amd_sdRet1 <- sd(amdprofit1) 
amd_tradeNums1 <- sum(as.numeric(amdpredict1>0.012)) 
amd_winRatio1 <- sum(as.numeric(amdprofit1>0))/amd_tradeNums1
amd_maxRet1 <- max(amdprofit1)
amd_minRet1 <- min(amdprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(amd_meanRet1*100,2)," %\n",
           "交易次數: ",amd_tradeNums1," 次\n",
           "勝率: ",round(amd_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(amd_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(amd_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(amd_minRet1*100,2)," %\n"))

Asr_ret1=Asr_ret[1:804]
Asrprofit1 = ifelse(Asrpredict1>0.012,Asr_ret1+log(0.997/1.003),0)#買賣成本各為0.003
Asr_EC1 = 1+cumsum(Asrprofit1) 
plot(Asr_EC1)

Asr_meanRet1 <- mean(Asrprofit1)
Asr_sdRet1 <- sd(Asrprofit1) 
Asr_tradeNums1 <- sum(as.numeric(Asrpredict1>0.012)) 
Asr_winRatio1 <- sum(as.numeric(Asrprofit1>0))/Asr_tradeNums1
Asr_maxRet1 <- max(Asrprofit1)
Asr_minRet1 <- min(Asrprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(Asr_meanRet1*100,2)," %\n",
           "交易次數: ",Asr_tradeNums1," 次\n",
           "勝率: ",round(Asr_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(Asr_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(Asr_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(Asr_minRet1*100,2)," %\n"))

chain_ret1=chain_ret[1:804]
chainprofit1 = ifelse(chainpredict1>0.012,chain_ret1+log(0.997/1.003),0)#買賣成本各為0.003
chain_EC1 = 1+cumsum(chainprofit1) 
plot(chain_EC1)

chain_meanRet1 <- mean(chainprofit1)
chain_sdRet1 <- sd(chainprofit1) 
chain_tradeNums1 <- sum(as.numeric(chainpredict1>0.012)) 
chain_winRatio1 <- sum(as.numeric(chainprofit1>0))/chain_tradeNums1
chain_maxRet1 <- max(chainprofit1)
chain_minRet1 <- min(chainprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(chain_meanRet1*100,2)," %\n",
           "交易次數: ",chain_tradeNums1," 次\n",
           "勝率: ",round(chain_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(chain_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(chain_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(chain_minRet1*100,2)," %\n"))

guc_ret1=guc_ret[1:804]
gucprofit1 = ifelse(gucpredict1>0.012,guc_ret1+log(0.997/1.003),0)#買賣成本各為0.003
guc_EC1 = 1+cumsum(gucprofit1) 
plot(guc_EC1)

guc_meanRet1 <- mean(gucprofit1)
guc_sdRet1 <- sd(gucprofit1) 
guc_tradeNums1 <- sum(as.numeric(gucpredict1>0.012)) 
guc_winRatio1 <- sum(as.numeric(gucprofit1>0))/guc_tradeNums1
guc_maxRet1 <- max(gucprofit1)
guc_minRet1 <- min(gucprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(guc_meanRet1*100,2)," %\n",
           "交易次數: ",guc_tradeNums1," 次\n",
           "勝率: ",round(guc_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(guc_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(guc_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(guc_minRet1*100,2)," %\n"))

intc_ret1=intc_ret[1:804]
intcprofit1 = ifelse(intcpredict1>0.012,intc_ret1+log(0.997/1.003),0)#買賣成本各為0.003
intc_EC1 = 1+cumsum(intcprofit1) 
plot(intc_EC1)

intc_meanRet1 <- mean(intcprofit1)
intc_sdRet1 <- sd(intcprofit1) 
intc_tradeNums1 <- sum(as.numeric(intcpredict1>0.012)) 
intc_winRatio1 <- sum(as.numeric(intcprofit1>0))/intc_tradeNums1
intc_maxRet1 <- max(intcprofit1)
intc_minRet1 <- min(intcprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(intc_meanRet1*100,2)," %\n",
           "交易次數: ",intc_tradeNums1," 次\n",
           "勝率: ",round(intc_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(intc_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(intc_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(intc_minRet1*100,2)," %\n"))

nvda_ret1=nvda_ret[1:804]
nvdaprofit1 = ifelse(nvdapredict1>0.012,nvda_ret1+log(0.997/1.003),0)#買賣成本各為0.003
nvda_EC1 = 1+cumsum(nvdaprofit1) 
plot(nvda_EC1)

nvda_meanRet1 <- mean(nvdaprofit1)
nvda_sdRet1 <- sd(nvdaprofit1) 
nvda_tradeNums1 <- sum(as.numeric(nvdapredict1>0.012)) 
nvda_winRatio1 <- sum(as.numeric(nvdaprofit1>0))/nvda_tradeNums1
nvda_maxRet1 <- max(nvdaprofit1)
nvda_minRet1 <- min(nvdaprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(nvda_meanRet1*100,2)," %\n",
           "交易次數: ",nvda_tradeNums1," 次\n",
           "勝率: ",round(nvda_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(nvda_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(nvda_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(nvda_minRet1*100,2)," %\n"))

tsmc_ret1=tsmc_ret[1:804]
tsmcprofit1 = ifelse(tsmcpredict1>0.012,tsmc_ret1+log(0.997/1.003),0)#買賣成本各為0.003
tsmc_EC1 = 1+cumsum(tsmcprofit1) 
plot(tsmc_EC1)

tsmc_meanRet1 <- mean(tsmcprofit1)
tsmc_sdRet1 <- sd(tsmcprofit1) 
tsmc_tradeNums1 <- sum(as.numeric(tsmcpredict1>0.012)) 
tsmc_winRatio1 <- sum(as.numeric(tsmcprofit1>0))/tsmc_tradeNums1
tsmc_maxRet1 <- max(tsmcprofit1)
tsmc_minRet1 <- min(tsmcprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(tsmc_meanRet1*100,2)," %\n",
           "交易次數: ",tsmc_tradeNums1," 次\n",
           "勝率: ",round(tsmc_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(tsmc_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(tsmc_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(tsmc_minRet1*100,2)," %\n"))

tul_ret1=tul_ret[1:804]
tulprofit1 = ifelse(tulpredict1>0.012,tul_ret1+log(0.997/1.003),0)#買賣成本各為0.003
tul_EC1 = 1+cumsum(tulprofit1) 
plot(tul_EC1)

tul_meanRet1 <- mean(tulprofit1)
tul_sdRet1 <- sd(tulprofit1) 
tul_tradeNums1 <- sum(as.numeric(tulpredict1>0.012)) 
tul_winRatio1 <- sum(as.numeric(tulprofit1>0))/tul_tradeNums1
tul_maxRet1 <- max(tulprofit1)
tul_minRet1 <- min(tulprofit1)
cat(paste0("*********策略回測績效*********\n",
           "平均報酬率: ",round(tul_meanRet1*100,2)," %\n",
           "交易次數: ",tul_tradeNums1," 次\n",
           "勝率: ",round(tul_winRatio1*100,2)," %\n",
           "報酬率標準差: ",round(tul_sdRet1*100,2)," %\n",
           "最大報酬率: ",round(tul_maxRet1*100,2)," %\n",
           "最小報酬率: ",round(tul_minRet1*100,2)," %\n"))

#簡單報酬
buyprice_amd = ifelse(amdpredict==1,dplyr::lead(Pro$AMD),0)
sellprice_amd= ifelse(amdpredict==1,Pro$AMD,0)
#amdprofit=sellprice_amd*(1-0.003)/(buyprice_amd*(1+0.003))-1
amd_fc = function(x){ifelse(amdpredict1>0.015,1,0)}
amd_bsig = lag(apply(Pro[1:803,]),1,amd_fc)
amd_ssig = apply( )    
