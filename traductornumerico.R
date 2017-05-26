# ASSUMPTIONS:
# text is previously extracted from documents and cleaned &
# removed extraneous words or symbols, then placed into a data frame
# Quantities MUST be written in a correct Spanish (this is not a grammar tool)
# The upper limit is up to the millions range.


# memory cleaning

rm(list = ls())

# sample dataframe. Add your own for testing.

df <- data.frame( 
  var1 = c("UNO","DOS MIL CUATROCIENTOS Doce","CUATRO MIL SEISCIENTOS TREINTA Y OCHO","setentaydosmil ciento cuarentaycuatro",
           "novecientos ochenta y nueve millones seiscientos treinta y dos mil doscientos cuarenta y ocho","ciento veinte",
           "veintemil cuatrocientos siete","tresmil trescientos treinta y tres","cientounmildos","tres",
           "veintiDOS MIL seiscientos cincuentayocho", "noventaynueve mil ciento dos","ochocientos veinte",
           "Novecientos ochenta y siete millones ochocientos sesenta y cinco mil cuatrocientos cincuenta y seis",
           "cuatromil setentaydos","cincuentamil veinticinco","dos millones trescientos dos mil","mil cuatrocientos cincuenta y dos",
           "docemil cuatrocientos seis","trescientoscatorcemil"),
  var2 = c("DOS","CINCO MIL NOVECIENTOS VEINTE","DOS MILLONES QUINIENTOS VEINTISIETEMIL DOSCIENTOS CUARENTA Y CINCO",
           "cientoveintisietemil cuatrocientos ochenta y dos","Dos mil cuatrocientos noventa y seis","dosmil cuarenta",
           "treintamil uno", "veintitresmil doscientos cuarenta","ochocientos treinta y ocho","cuatro",
           "trescientos treintaydosmil cientoveintiocho","doscientosmil","noventa y nueve","Doscientos treinta y cinco mil ciento sesentaynueve",
           "diezmil ciento ochenta y nueve","cuarentamil trescientos dos","ciento veintiunmil cuatrocientos nueve","novecientosmil novecientos noventa y nueve",
           "ciento oncemil veintiocho","cuatrocientosveintiochomil doce")
)

library(magrittr)

reemplazar <- function(x) {
  
 x[] <- gsub("^mil", "1000)+", x , ignore.case = T) %>%
    gsub("once", "+11", . , ignore.case = T) %>%
    gsub("doce", "+12", . , ignore.case = T ) %>%
    gsub("trece", "+13", . , ignore.case = T) %>%
    gsub("catorce", "+14", . , ignore.case = T) %>%
    gsub("quince", "+15", . , ignore.case = T) %>% 
    gsub("dieciseis", "+16", . , ignore.case = T) %>%
    gsub("diecisiete|diez y siete", "+17", . , ignore.case = T) %>%
    gsub("dieciocho", "+18", . , ignore.case = T) %>%
    gsub("diecinueve", "+19", . , ignore.case = T) %>%
    gsub("veinte|veinti", "+20", . , ignore.case = T) %>%
    gsub("treinta", "+30", . , ignore.case = T) %>%
    gsub("cuarenta", "+40", . , ignore.case = T) %>%
    gsub("cincuenta", "+50", . , ignore.case = T) %>%
    gsub("sesenta", "+60", . , ignore.case = T) %>%
    gsub("setenta", "+70", . , ignore.case = T) %>%
    gsub("ochenta", "+80", . , ignore.case = T) %>%
    gsub("noventa", "+90", . , ignore.case = T) %>%
    gsub("doscientos", "+200", . , ignore.case = T) %>%
    gsub("trescientos", "+300", . , ignore.case = T) %>%
    gsub("cuatrocientos", "+400", . , ignore.case = T) %>%
    gsub("quinientos", "+500", . , ignore.case = T) %>%
    gsub("seiscientos", "+600", . , ignore.case = T) %>%
    gsub("setecientos", "+700", . , ignore.case = T) %>%
    gsub("ochocientos", "+800", . , ignore.case = T) %>%
    gsub("novecientos", "+900", . , ignore.case = T) %>%
    gsub("uno", "+1", . , ignore.case = T) %>%
    gsub("dos", "+2", . , ignore.case = T) %>%
    gsub("tres", "+3", . , ignore.case = T) %>%
    gsub("cuatro", "+4", . , ignore.case = T) %>%
    gsub("cinco", "+5", . , ignore.case = T) %>%
    gsub("seis", "+6", . , ignore.case = T) %>%
    gsub("siete", "+7", . , ignore.case = T) %>%
    gsub("ocho", "+8", . , ignore.case = T) %>%
    gsub("nueve", "+9", . , ignore.case = T) %>%
    gsub("millones", ")*(1000000)+(0", . , ignore.case = T) %>%
    gsub("millon", ")*(1000000)+(0", . , ignore.case = T) %>%
    gsub("mil", ")*(1000)+(0", . , ignore.case = T) %>%
    gsub("ciento", "+100", . , ignore.case = T) %>%
    gsub("cien", "+100", . , ignore.case = T) %>%
    gsub("diez", "+10", . , ignore.case = T) %>%
    gsub("un", "+1", . , ignore.case = T) %>%
    gsub("Y", "", . , ignore.case = T) %>%
    gsub(" ", "", . , ignore.case = T) %>%
    gsub("^", "(0", . , ignore.case = T) %>%
    gsub("$", ")", . , ignore.case = T) %>%
    gsub("\\(0\\(", "", . , ignore.case = T ) %>%
    gsub("\\+\\+", "\\+\\(", . , ignore.case = T ) %>%
    gsub("\\)\\+\\)", "\\)", . , ignore.case = T )

  return(Vectorize(eval(parse( text = x))))

}


# replace dataframe

newdf <- reemplazar(df)


# change dataframe colnames for better describing their actual content (math expression)

names(newdf) <- c("expr1","expr2")


# eval+parse only returned the last operation, so apply on a data frame did not worked
# splitted and executed using sapply, one for each column 

for (i in 1:ncol(newdf)) {
  
  evaluar <- paste0("newdf$res", i , " <- sapply(newdf$expr",i,", function(x) eval(parse(text = paste0('',x))))")
  # print(evaluar)
  eval(parse( text = evaluar ))
  
}

res <- as.data.frame(cbind(df,newdf))

res <- res[c("var1","expr1","res1","var2","expr2","res2")]

res

# if an Excel file is needed

library(rio)

export(res, file = "cifrasyletras.xlsx" , format = "xlsx" )
