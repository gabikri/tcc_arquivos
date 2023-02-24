setwd("C:\\Users\\gabri\\Dropbox\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")
#setwd("C:\\Users\\pchai\\Dropbox\\UFSC\\ALUNOS\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")

rm(list=ls())

#Código Exemplo
# XRPUSD
OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES
df.contrato = read.csv("./CMFUTURES/CM_XRPUSD_211231-1d.csv", stringsAsFactors = F)
df.spot = read.csv("./SPOT/CMINDEX_XRPUSD-1d.csv", stringsAsFactors = F)
dia.vencimento = as.Date("2021-12-31")
file.name = "./FULL/FULL_CM_XRPUSD_211231-1d.csv"

JUNTACONTRATOESPOT = function(df.contrato, df.spot, dia.vencimento, OFFSET, file.name){
  df.contrato=df.contrato[df.contrato$OpenTime<=dia.vencimento,]
  df.spot = df.spot[c("OpenTime","Close")]
  colnames(df.spot) = c("OpenTime","Spot") # trocando o nome do "Close"
  df.spot=df.spot[c("OpenTime", "Spot")] # VAMOS TIRAR AS COLUNAS ``SUPÉRFULAS''.
  df.contrato=df.contrato[c("OpenTime","Close")]
  
  df.full = merge.data.frame(df.contrato, df.spot, by=c("OpenTime"), all.y=F, all.x=F)

  df.full$OpenTime = as.Date(df.full$OpenTime)
  TTM = dia.vencimento-df.full$OpenTime
  df.full$TTM = as.double(TTM)
  df.full = df.full[1:(dim(df.full)[1]-OFFSET),]
  write.csv(df.full, file=file.name, row.names=F)
  print(file.name)
}
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# PRIMEIRO PARA BTCUSD
OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/BTCUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# BTCUSD_200925.csv
df.contrato = read.csv("./CMFUTURES/CM_BTCUSD_200925-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-09-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BTCUSD_200925-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# BTCUSD_201225
df.contrato = read.csv("./CMFUTURES/CM_BTCUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BTCUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# BTCUSD_210326
df.contrato = read.csv("./CMFUTURES/CM_BTCUSD_210326-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BTCUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# BTCUSD_210625
df.contrato = read.csv("./CMFUTURES/CM_BTCUSD_210625-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BTCUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# BTCUSD_210924
df.contrato = read.csv("./CMFUTURES/CM_BTCUSD_210924-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BTCUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("200925", "201225", "210326", "210625", "210924")
read.name = paste("./FULL/FULL_CM_BTCUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_BTCUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_BTCUSD-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)
carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)

# AGORA ADAUSD

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/ADAUSD-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# CM_ADAUSD_200925-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_200925-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-09-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_200925-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_ADAUSD_201225-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_ADAUSD_210326-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_210326-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_ADAUSD_210625-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_210625-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_ADAUSD_210924-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_210924-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_ADAUSD_211231-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_ADAUSD_211231-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-12-31") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ADAUSD_211231-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("200925", "201225", "210326", "210625", "210924", "211231")
read.name = paste("./FULL/FULL_CM_ADAUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_ADAUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_ADAUSD-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)
carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)



###BCHUSD

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/BCHUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# CM_BCHUSD_201225-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BCHUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BCHUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BCHUSD_210326-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BCHUSD_210326-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BCHUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BCHUSD_210625-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BCHUSD_210625-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BCHUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BCHUSD_210924-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BCHUSD_210924-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BCHUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BCHUSD_211231-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BCHUSD_211231-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-12-31") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BCHUSD_211231-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("201225", "210326", "210625", "210924", "211231")
read.name = paste("./FULL/FULL_CM_BCHUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_BCHUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_BCHUSD-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)
carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)



###BNBUSD

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/BNBUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# CM_BNBUSDT_201225-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BNBUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BNBUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BNBUSDT_210326-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BNBUSD_210326-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BNBUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BNBUSDT_210625-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BNBUSD_210625-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BNBUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BNBUSDT_210924-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BNBUSD_210924-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BNBUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_BNBUSDT_211231-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_BNBUSD_211231-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-12-31") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_BNBUSD_211231-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("201225", "210326", "210625", "210924", "211231")
read.name = paste("./FULL/FULL_CM_BNBUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_BNBUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_BNBUSDT-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)

carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)

###DOTUSD

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/DOTUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# CM_DOTUSD_201225-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_DOTUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_DOTUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_DOTUSD_210326-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_DOTUSD_210326-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_DOTUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_DOTUSD_210625-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_DOTUSD_210625-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_DOTUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_DOTUSD_210924-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_DOTUSD_210924-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_DOTUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# CM_DOTUSD_211231-1d.csv
df.contrato = read.csv("./CMFUTURES/CM_DOTUSD_211231-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2021-12-31") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_DOTUSD_211231-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("201225", "210326", "210625", "210924", "211231")
read.name = paste("./FULL/FULL_CM_DOTUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_DOTUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_DOTUSDT-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)
carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)


#ETHUSDT

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/ETHUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# ETHUSD_200925.csv
df.contrato = read.csv("./CMFUTURES/CM_ETHUSD_200925-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-09-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ETHUSD_200925-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# ETHUSD_201225
df.contrato = read.csv("./CMFUTURES/CM_ETHUSD_201225-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-12-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ETHUSD_201225-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# ETHUSD_210326
df.contrato = read.csv("./CMFUTURES/CM_ETHUSD_210326-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-03-26") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ETHUSD_210326-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# ETHUSD_210625
df.contrato = read.csv("./CMFUTURES/CM_ETHUSD_210625-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-06-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ETHUSD_210625-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# ETHUSD_210924
df.contrato = read.csv("./CMFUTURES/CM_ETHUSD_210924-1d.csv", stringsAsFactors = F) # ISSO TEM QUE MUDAR
dia.vencimento = as.Date("2021-09-24") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_ETHUSD_210924-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

# AGORA EU VOU JUNTAR OS ARQUIVOS SALVOS. DAVA PRA FAZER DE UMA VEZ, MAS ASSIM É MAIS CLARO
vencimentos = c("200925", "201225", "210326", "210625", "210924")
read.name = paste("./FULL/FULL_CM_ETHUSD_",vencimentos[1],"-1d.csv",sep="")
df.full = read.csv(read.name, header=T, stringsAsFactors = F)
for (jj in 2:length(vencimentos)){
  read.name = paste("./FULL/FULL_CM_ETHUSD_",vencimentos[jj],"-1d.csv",sep="")
  df.full = rbind(df.full, read.csv(read.name, header=T, stringsAsFactors = F))
}
write.csv(df.full,"./FULL/FULL_CM_ETHUSD-1d.csv",row.names=F)
a = df.full$Close-df.full$Spot
max(a) # maior diferença entre futuro e spot na tabela. É um jeito de procurar erros.
which.max(a)
carry = (log(df.full$Spot)-log(df.full$Close))/df.full$TTM
ts.plot(100*carry)

#LINKUSDT

OFFSET = 7 # SAO OS ULTIMOS DIAS QUE VAMOS REMOVER DOS CONTRATOS PARA ENCADEAR AS SERIES. NAO PRECISA MUDAR
df.spot = read.csv("./SPOT/LINKUSDT-1d.csv", stringsAsFactors = F) # ESSES JÁ SAO TODOS OS SPOT PRICES JUNTOS, NAO PRECISA MUDAR PARA O MESMO ATIVO.

# LINKUSD_200925.csv
df.contrato = read.csv("./CMFUTURES/CM_LINKUSD_200925-1d.csv", stringsAsFactors = F) # ESSE É O CONTRATO, MUDA.
dia.vencimento = as.Date("2020-09-25") # ISSO MUDA TAMBEM
file.name = "./FULL/FULL_CM_LINKUSD_200925-1d.csv" # ISSO MUDA
JUNTACONTRATOESPOT(df.contrato, df.spot, dia.vencimento, OFFSET, file.name)

