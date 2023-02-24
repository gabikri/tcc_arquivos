setwd("C:\\Users\\gabri\\Dropbox\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")
#setwd("C:\\Users\\pchai\\Dropbox\\UFSC\\ALUNOS\\MONOGRAFIA_GABRIELAKRISCINSKI\\DADOS")
rm(list=ls())
library(dplyr)
library(curl)
library(lubridate)

listaURL = read.csv("./LISTAS/SPOT_LINKUSDT-1d.csv")
juntaPrecoSpot = function(listaURL){
  downloadSpotKlines = function(connectServer, destFile){
    destFile = paste("RAW/",destFile,sep="")
    curl_download(connectServer,destFile) #baixa o arquivo
    unzip(destFile, exdir="./RAW") # tira do "*.zip"
    spot.colnames = c("OpenTime", "Open", "High", "Low", "Close", "Volume", "CloseTime", "QuoteAssetVolume", "NumberOfTrades", "TakerBuyBaseAssetVolume", "TakerBuyQuoteAssetVolume", "Ignore") # isso vÃªm daqui: https://github.com/binance/binance-public-data/
    df = read.csv(gsub(".zip", ".csv", destFile), header=FALSE)
    colnames(df) =  spot.colnames
    df$OpenTime = lubridate::as_datetime(df$OpenTime/1000) #ajusta os dados de tempo
    df$CloseTime = lubridate::as_datetime(df$CloseTime/1000)
    tmp = gsub("RAW","SPOT",destFile)
    tmp = gsub(".zip",".csv",tmp)
    write.csv(df, tmp, row.names=FALSE)
    return(tmp)
  }
  for (kk in 1:dim(listaURL)[1]){
    downloadSpotKlines(listaURL[kk,1],listaURL[kk,2])
  }
  tabelas = list()
  tabelas[[1]] = read.csv(paste("./SPOT/",gsub(".zip",".csv",listaURL[dim(listaURL)[1],2]),sep=""), stringsAsFactors = F)
  for (kk in 1:(dim(listaURL)[1]-1)){
    fileName = gsub(".zip",".csv",listaURL[dim(listaURL)[1]-kk,2])
    tabelas[[kk+1]] = read.csv(paste("./SPOT/",fileName,sep=""), stringsAsFactors = F)
  }
  df.out = tabelas[[1]]
  for (kk in 2:dim(listaURL)[1]){
    df.out = rbind(df.out,tabelas[[kk]])
  }
  out.name = gsub("-.*", "\\1",listaURL[1,2])
  out.name = paste(out.name,"-1d.csv",sep="")
  write.csv(df.out,file=paste("./SPOT/",out.name,sep=""), row.names = F)
}
juntaPrecoSpot(listaURL)



downloadIndexPriceKlines = function(connectServer, destFile){
  destFile = paste("RAW/",destFile,sep="")
  curl_download(connectServer,destFile) #baixa o arquivo
  unzip(destFile, exdir="./RAW") # tira do "*.zip"
  spot.colnames = c("OpenTime", "Open", "High", "Low", "Close", "Volume", "CloseTime", "QuoteAssetVolume", "NumberOfTrades", "TakerBuyBaseAssetVolume", "TakerBuyQuoteAssetVolume", "Ignore") # isso vêm daqui: https://github.com/binance/binance-public-data/
  df = read.csv(gsub(".zip", ".csv", destFile), header=FALSE)
  colnames(df) =  spot.colnames
  df$OpenTime = lubridate::as_datetime(df$OpenTime/1000) #ajusta os dados de tempo
  df$CloseTime = lubridate::as_datetime(df$CloseTime/1000)
  tmp = gsub("RAW","SPOT",destFile)
  tmp = gsub(".zip",".csv",tmp)
  write.csv(df, tmp, row.names=FALSE)
  return(tmp)
}
BAIXA_CMINDEXPRICE = function(listaURL, filename){
  for (kk in 1:dim(listaURL)[1]){
    connectServer = listaURL$fullURL[kk]
    destFile = listaURL$fileName[kk]
    downloadIndexPriceKlines(connectServer, destFile)
  }
  listaURL = listaURL[nrow(listaURL):1,] # do jeito que eu escrevi as listas, tem que inverter a ordem dos meses para juntar as tabelas
  big.df = read.csv(paste("./SPOT/",gsub(".zip",".csv",listaURL$fileName[1]),sep=""),stringsAsFactors = F); 
  for (kk in 2:dim(listaURL)[1]){ #
    tmp = read.csv(paste("./SPOT/",gsub(".zip",".csv",listaURL$fileName[kk]),sep=""),stringsAsFactors = F); 
    big.df = rbind(big.df, tmp)
  }
  write.csv(big.df,paste("./SPOT/",fileName,sep=""),row.names=F)
  print(paste("./SPOT/",fileName,sep=""))
}


# CMINDEX ADAUSDT
listaURL = read.csv("./LISTAS/SPOT_ADAUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "ADAUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)

# CMINDEX BCHUSD
listaURL = read.csv("./LISTAS/SPOT_BCHUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "BCHUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)


# CMINDEX BNBUSDT
listaURL = read.csv("./LISTAS/SPOT_BNBUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "BNBUSDT-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)

# CMINDEX DOTUSD
listaURL = read.csv("./LISTAS/SPOT_DOTUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "DOTUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)


# CMINDEX ETHUSD
listaURL = read.csv("./LISTAS/SPOT_ETHUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "ETHUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)

# CMINDEX LINKUSD
listaURL = read.csv("./LISTAS/SPOT_LINKUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "LINKUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)

# CMINDEX XRPUSD
listaURL = read.csv("./LISTAS/SPOT_XRPUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "XRPUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)

# CMINDEX BTCUSD
listaURL = read.csv("./LISTAS/SPOT_BTCUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "BTCUSD-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)


# CMINDEX LTCUSD
listaURL = read.csv("./LISTAS/SPOT_LTCUSDT-1d.csv", header=T, stringsAsFactors = F)
fileName = "LTCUSDT-1d.csv"
BAIXA_CMINDEXPRICE(listaURL, fileName)
