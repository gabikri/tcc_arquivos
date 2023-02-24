# setwd("C:\\Users\\pchai\\Dropbox\\UFSC\\ALUNOS\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")
setwd("C:\\Users\\gabri\\Dropbox\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")
rm(list=ls())


library(dplyr)
require(curl)
require(lubridate)

# Baixa futures CM
# A primeira função baixa cada arquivo individualmente, a segunda junta os que são do mesmo contrato.

downloadQuarterlyCOINMFutureKlines = function(connectServer, destFile){
  destFile = paste("RAW/",destFile,sep="")
  curl_download(connectServer,destFile) #baixa o arquivo
  unzip(destFile, exdir="./RAW") # tira do "*.zip"
  COINM.futures.colnames = c("OpenTime", "Open", "High", "Low", "Close", "Volume", "CloseTime", "BaseAssetVolume", "NumberOfTrades", "TakerBuyVolume", "TakerBuyBaseAssetVolume", "Ignore")
  df = read.csv(gsub(".zip", ".csv", destFile), header=FALSE)
  colnames(df) =  COINM.futures.colnames
  df$OpenTime = lubridate::as_datetime(df$OpenTime/1000) #ajusta os dados de tempo
  df$CloseTime = lubridate::as_datetime(df$CloseTime/1000)
  tmp = gsub("RAW","CMFUTURES",destFile) # presta atenção aqui que o nome dos arquivos vão ser os mesmos pros contratos UM e CM, então tem que colocar em outra pasta. 
  tmp = gsub(".zip",".csv",tmp)
  write.csv(df, tmp, row.names=FALSE)
  print(tmp)
}

BAIXA_CMFUTURE = function(listaURL, fileName){
  for (kk in 1:dim(listaURL)[1]){
    connectServer = listaURL$fullURL[kk]
    destFile = listaURL$fileName[kk]
    downloadQuarterlyCOINMFutureKlines(connectServer, destFile)
  }
  # Agora vamos abrir cada um dos .csv e fazer uma única tabela grande
  listaURL = listaURL[nrow(listaURL):1,] # do jeito que eu escrevi as listas, tem que inverter a ordem dos meses para juntar as tabelas
  big.df = read.csv(paste("./CMFUTURES/",gsub(".zip",".csv",listaURL$fileName[1]),sep=""),stringsAsFactors = F); 
  for (kk in 2:dim(listaURL)[1]){ #
    tmp = read.csv(paste("./CMFUTURES/",gsub(".zip",".csv",listaURL$fileName[kk]),sep=""),stringsAsFactors = F); 
    big.df = rbind(big.df, tmp)
  }
  write.csv(big.df,paste("./CMFUTURES/",fileName,sep=""),row.names=F)
  print(paste("./CMFUTURES/",fileName,sep=""))
}

# CM ADAUSD
listaURL = read.csv("./LISTAS/CM_ADAUSD_200925-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ADAUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ADAUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ADAUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ADAUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ADAUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ADAUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


## BCHUSD

listaURL = read.csv("./LISTAS/CM_BCHUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BCHUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BCHUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


##BNBUSD


listaURL = read.csv("./LISTAS/CM_BNBUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_BNBUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BNBUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


##DOTUSD

listaURL = read.csv("./LISTAS/CM_DOTUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_DOTUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_DOTUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


##ETHUSD

listaURL = read.csv("./LISTAS/CM_ETHUSD_200925-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_ETHUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_ETHUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


##LINKUSD

listaURL = read.csv("./LISTAS/CM_LINKUSD_200925-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LINKUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LINKUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


#LTCUSD

listaURL = read.csv("./LISTAS/CM_LTCUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_LTCUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_LTCUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

#XRPUSD

listaURL = read.csv("./LISTAS/CM_XRPUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

listaURL = read.csv("./LISTAS/CM_XRPUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_XRPUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)


# CM BTC
# CM_BTCUSD_200925-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_200925-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)
# CM_BTCUSD_201225-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_201225-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_201225-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)
# CM_BTCUSD_210326-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_210326-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_210326-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)
# CM_BTCUSD_210625-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_210625-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_210625-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)
# CM_BTCUSD_210924-1m
listaURL = read.csv("./LISTAS/CM_BTCUSD_210924-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_210924-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)
# CM_BTCUSD_211231-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_211231-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_211231-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

# CM_BTCUSD_220325-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_220325-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_220325-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)

# CM_BTCUSD_220624-1d
listaURL = read.csv("./LISTAS/CM_BTCUSD_220624-1d.csv", header=T, stringsAsFactors = F)
fileName = "CM_BTCUSD_220624-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)



# CM ETH
# CM_ETHUSD_200925-1d
listaURL = read.csv("./LISTAS/CM_ETHUSD_200925-1d.csv", header=T, stringsAsFactors = F) # Tem um detalhe que acontece. às vezes os arquivos têm cotações após a expiração do contrato. Elas são "artificiais" e tem que tirar. Dá pra ajustar o código ou fazer na mão. Se você prestar atenção e não incluir os meses após o vencimento do contrato na "lista", também resolve.
fileName = "CM_ETHUSD_200925-1d.csv"
BAIXA_CMFUTURE(listaURL, fileName)