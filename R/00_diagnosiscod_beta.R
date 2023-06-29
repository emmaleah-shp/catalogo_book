library(pacman)
pacman::p_load(tidyselect, sqldf, plyr, glue, knitr, stringr, magrittr, readxl,
               sf, ggmap, ggplot2, tidyverse, flextable, data.table, 
               scales, dplyr, devtools, tinytex, writexl, janitor)
library(arcgisbinding)
arc.check_product()
options(scipen=999)
path<-"E:/GDB_DATAMET.gdb" # path a la geodatabase
shps1 <-st_layers("E:/GDB_DATAMET.gdb")$name # path para hacer la lista de shapefiles en el GDB

# dir.create(path)

# catalogo<-read_excel("C:/Users/Elite Center/OneDrive/Documents/OCUC/catalogo3_ejcopy.xlsx", sheet = "Hoja todo", trim_ws = TRUE)
cat_og<-read_excel("C:/Users/Elite Center/OneDrive/Documents/OCUC/catalogo3_ejcopy.xlsx", sheet = "Hoja todo", trim_ws = TRUE)
names(cat_og)<-make_clean_names(names(cat_og))
# 1, 10, 20

library(stringi)
catalogo$esta_en_la_carpeta<-catalogo$esta_en_la_carpeta %>% 
  iconv(to='ASCII//TRANSLIT') %>% 
  str_to_lower()

catalogo<-catalogo %>% filter(calidad=="idoneo")
tt<-catalogo[1,]

tt$nombre_del_shape
nombre<-paste("BAS_", tt$nombre_del_shape, sep="")
vv<-catalogo %>% group_by(ambito) %>% dplyr::summarise(count=n())
vv$ambito<-str_replace_all(vv$ambito, " ", "_")
vv<-vv$ambito %>% as.character()
vv

# bookdown::gitbook:
#   css: style.css
# config:
#   toc:
#   before: |
#   <li><a href="./">A Minimal Book Example</a></li>
#   after: |
#   <li><a href="https://github.com/rstudio/bookdown" target="blank">Published with bookdown</a></li>
#   edit: https://github.com/USERNAME/REPO/edit/BRANCH/%s
# download: ["pdf", "epub"]
# bookdown::pdf_book:
#   includes:
#   in_header: preamble.tex
# latex_engine: xelatex
# citation_package: natbib
# keep_tex: yes
# bookdown::epub_book: default
