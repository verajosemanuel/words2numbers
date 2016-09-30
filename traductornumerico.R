# The script assume the text is previously cleaned from extraneous words or symbols
# The upper limit is in the millions range.


# sample dataframe. Add your own for testing.

df <- data.frame( 
var1 = c("UNO","DOS MIL CUATROCIENTOS DIECISIETE","CUATRO MIL SEISCIENTOS TREINTA Y OCHO","setentaydosmil ciento cuarentaycuatro","novecientos ochenta y nueve millones seiscientos treinta y dos mil doscientos cuarenta y ocho"),
var2 = c("DOS","CINCO MIL NOVECIENTOS VEINTE","DOS MILLONES QUINIENTOS VEINTISIETEMIL DOSCIENTOS CUARENTA Y CINCO","cientoveintisietemil cuatrocientos ochenta y dos","Dos mil cuatrocientos noventa y seis"),
var3 = c("tres","veintiDOS MIL seiscientos cincuentayocho","noventaynueve mil ciento dos","ochocientos veinte","Novecientos ochenta y siete millones ochocientos sesenta y cinco mil cuatrocientos cincuenta y seis"),
var4 = c("cuatro","trescientos treintaydosmil cientoveintiocho","doscientosmil","noventa y nueve","Doscientos treinta y cinco mil ciento sesentaynueve"))


# function for replacing words with their numerical counterpart

reemplazar <- function(x) {

  x <- as.data.frame(sapply(x,gsub,pattern = "ONCE", replacement = "+11", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DOCE", replacement = "+12", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "TRECE", replacement = "+13", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CATORCE", replacement = "+14", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "QUINCE", replacement = "+15", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DIECISEIS", replacement = "+16", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DIECISIETE|DIEZ Y SIETE", replacement = "+17", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DIECIOCHO", replacement = "+18", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DIECINUEVE", replacement = "+19", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "VEINTE", replacement = "+20", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "VEINTI", replacement = "+20", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "TREINTA", replacement = "+30", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CUARENTA", replacement = "+40", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CINCUENTA", replacement = "+50", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SESENTA", replacement = "+60", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SETENTA", replacement = "+70", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "OCHENTA", replacement = "+80", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "NOVENTA", replacement = "+90", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "DOSCIENTOS", replacement = "+200", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "TRESCIENTOS", replacement = "+300", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CUATROCIENTOS", replacement = "+400", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "QUINIENTOS", replacement = "+500", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SEISCIENTOS", replacement = "+600", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SETECIENTOS", replacement = "+700", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "OCHOCIENTOS", replacement = "+800", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "NOVECIENTOS", replacement = "+900", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "UNO", replacement = "+1", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DOS", replacement = "+2", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "TRES", replacement = "+3", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CUATRO", replacement = "+4", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CINCO", replacement = "+5", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SEIS", replacement = "+6", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "SIETE", replacement = "+7", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "OCHO", replacement = "+8", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "NUEVE", replacement = "+9", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "MILLONES", replacement = ")*(1000000)+(0", ignore.case = T))
  x <- as.data.frame(sapply(x,gsub,pattern = "MILLON", replacement = ")*(1000000)+(0", ignore.case = T))
  x <- as.data.frame(sapply(x,gsub,pattern = "MIL", replacement = ")*(1000)+(0", ignore.case = T))
  x <- as.data.frame(sapply(x,gsub,pattern = "CIENTO", replacement = "+100", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "CIEN", replacement = "+100", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "DIEZ", replacement = "+10", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "Y", replacement = "", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = " ", replacement = "", ignore.case = T) )
  
  x <- as.data.frame(sapply(x,gsub,pattern = "^", replacement = "(0", ignore.case = T) )
  x <- as.data.frame(sapply(x,gsub,pattern = "$", replacement = ")", ignore.case = T) )

 return(x)
}

# replace dataframe

newdf <- reemplazar(df)


# change dataframe colnames for better describing their actual content (math expression)

names(newdf) <- c("expr1","expr2","expr3","expr4")


# eval+parse only returned the last operation, so a function did not worked (yet)

df$res1 <- sapply(newdf$expr1, function(x) eval(parse(text = paste0("",x))))
df$res2 <- sapply(newdf$expr2, function(x) eval(parse(text = paste0("",x))))
df$res3 <- sapply(newdf$expr3, function(x) eval(parse(text = paste0("",x))))
df$res4 <- sapply(newdf$expr4, function(x) eval(parse(text = paste0("",x))))


# concat original and parsed data frames

res <- cbind(df,newdf)


# reorder columns for better checking accuracy

res <- res[c("var1","expr1","res1","var2","expr2","res2","var3","expr3","res3","var4","expr4","res4")]

res


# if an Excel file is needed

library(rio)

export(res, file = "cifrasyletras.xlsx" , format = "xlsx" )
