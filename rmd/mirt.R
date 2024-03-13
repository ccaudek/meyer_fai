library("mirt")

# https://rstudio-pubs-static.s3.amazonaws.com/357155_6674780326ef4afba5f009d17a85d4ae.html


model.rsm <- 'fake news = 1-5' 
results.rsm <- mirt(
  data=mysubscale1, 
  model=model.rsm, 
  itemtype="rsm", 
  verbose=FALSE
)
coef.rsm <- coef(results.rsm, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)


plot(results.rsm, type = 'trace', which.items = c(1, 2, 3, 4, 5), 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(results.rsm, type = 'infotrace', which.items = c(1, 2, 3, 4, 5), 
     main = "", par.settings = simpleTheme(lwd=2))


plot(results.rsm, type = 'info', theta_lim = c(-1, 12), lwd=2)                           

plot(results.rsm, type = 'SE', theta_lim = c(-1,12), lwd=2)                           

