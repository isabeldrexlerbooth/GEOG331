library(terra)
library(tidyterra)
library(FedData)
nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:\\idrexlerbooth\\data")
nlcd_meve16
