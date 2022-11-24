

tmp <-
  gadgetlikelihood('likelihood',gd,missingOkay = TRUE) %>% 
  ## Write a penalty component to the likelihood file
  gadget_update("penalty",
                name = "bounds",
                weight = "0.5",
                data = data.frame(
                  switch = c("default"),
                  power = c(2),
                  upperW=10000,
                  lowerW=10000,
                  stringsAsFactors = FALSE)) %>%
  
  gadget_update("understocking",
                name = "understocking",
                weight = "100") %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.pse",
                weight = 1,
                data = ldist.pse[[1]],
                fleetnames = c("pse"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.trol",
                weight = 1,
                data = ldist.trol[[1]],
                fleetnames = c("trol"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.bb",
                weight = 1,
                data = ldist.bb[[1]],
                fleetnames = c("bb"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.gil",
                weight = 1,
                data = ldist.gil[[1]],
                fleetnames = c("gil"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.lln",
                weight = 1,
                data = ldist.lln[[1]],
                fleetnames = c("lln"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.other",
                weight = 1,
                data = ldist.other[[1]],
                fleetnames = c("other"),
                stocknames =stock_names) %>% 
  
  gadget_update("catchdistribution",
                name = "ldist.hln",
                weight = 1,
                data = ldist.hln[[1]],
                fleetnames = c("hln"),
                stocknames =stock_names) %>% 
  
  gadget_update("surveyindices",
                name = "cpue1",
                weight = 1,
                data = cpue1[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names)

  tmp  %>% 
    write.gadget.file(gd)
