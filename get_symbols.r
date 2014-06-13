
get.symbols = function(dir) {
 
  symbols = list()
  symbols[["cyndi"]]   = c("aapl", "adksx", "gsra", "hyls","tlt", "tsla", "vlu")
  
  symbols[["rosey"]]   = c("fipfx", "ucmcx", "usaax", "uscgx", "uscrx", "vwelx")
  
  symbols[["folio"]]   = c("ANN", "ADKSX", "ARMH", "BMO", "BOH", "DVY", "EAT", "ENB", "EWC", 
                        "EWS", "HE", "IYR", "JWN", "LQD", "POT", "THI", "TNH", "TRP", "TU", "TLT")
  
  symbols[["mitre"]]  = c("FCNKX", "AMANX", "FAGIX", "FICDX", "FXSIX", "FDIKX")
  
  symbols[["smm"]]    = c("ECON","XLV","TLT","IEI","LQD","IEMG","MBB","EWC","EZU",
                        "EPP","EWD","EWL","EWU","IBB","TIP","EMLC","PCY","PGX","XLF","JNK","BWX","VCR",
                        "VDC","VDE","VIS","VGT","BIV","VAW","BSV","VOX","DXJ")
  
  
  symbols[["sp500"]]  = get.sp500.symbols()
  symbols
  }
