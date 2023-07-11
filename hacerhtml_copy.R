pacman::p_load(tidyselect, tidyverse, dplyr, glue, knitr, stringr, magrittr, 
               sf, ggmap, ggplot2, flextable, data.table, sp, 
               scales, devtools, tinytex, writexl, webshot, readr)
library(arcgisbinding)
arc.check_product()
options(scipen=999)


#whitespace
wsp1<-function(x){
  num<-str_count(x, "\\s+") - str_count(x, "\\w")
  c<-sum(num>0)
  c
}

whitespacedf<-function(df){
  dfx<- df %>% as.data.frame() %>% dplyr::select(where(is.character))%>% na.exclude() %>%
    dplyr::summarise(across(.cols = everything(), .fns = ~ wsp1(.x), .names = "{.col}"))
  dff<-dfx %>% dplyr::summarise(across(.cols = everything(), .fns = ~ sum(.x!=0), .names = "{.col}")) %>% t()
  dff<-as.data.frame(dff) %>% rownames_to_column()
  names(dff)[c(2)]<-"whitespace"
  dff
}

# unique 
n_distinct<- function(x){
  length(unique(x))
}

# Diagnosticar
diagnosticar<- function(df){
  dfx<-df %>% as.data.frame() %>% dplyr::select(!last_col()) # CAN also use select(!c("geometry"))
  tip<-data.frame(sapply(dfx,typeof)) %>%
    rownames_to_column()
  vac<- dfx %>% dplyr::summarise(across(.cols = everything(), 
                                        .fns = ~ sum(is.na(.x)), .names = "{.col}")) %>% t()
  vac<- as.data.frame(vac) %>% rownames_to_column()
  stch<- merge(tip,vac, by="rowname") %>% as.data.frame()
  ws<-whitespacedf(dfx)
  stch<-merge(stch, ws, by="rowname", all.x=TRUE)
  uniq<- dfx %>% dplyr::summarise(across(everything(), .fns = ~ n_distinct(.x), .names ="{.col}")) %>% 
    t() %>% as.data.frame() %>% rownames_to_column()
  stch<-merge(stch, uniq, by="rowname")
  stch[is.na(stch)] <- 0
  stch<- stch %>% dplyr::mutate(pc_completo = (nrow(dfx) - (V1.x + whitespace)) / (nrow(dfx)) *100,
                                pc_repetido=(nrow(dfx) - V1.y) / (nrow(dfx)) *100)
  names(stch)[c(2:5)]<-c("tipo","vacios","whitespace","n_unico")
  stch
}


require(stringr)
require(sf)
path<-"E:/GDB_DATAMET.gdb"
shps1 <-st_layers(glue("{path}"))$name
shps1<-sort(shps1)
shps1<-shps1[c(1:3,6:9, 39:43, 52:56, 77:80, 83, 99:103, 117:119)]

amb<-str_extract(shps1, "^\\w{3}")
amb<-amb %>% str_replace_all("MOV", "movilidad") %>% 
  str_replace_all("BAS", "informacion_base") %>% 
  str_replace_all("SEG", "seguridad") %>% 
  str_replace_all("RIE", "riesgos_y_emergencias") %>% 
  str_replace_all("PLA", "planificacion") %>% 
  str_replace_all("MED", "medio_ambiente")


# for(i in 1:length(shps1)){
#   titulo_capa<-shps1[i]
#   capa<-
#     titulo <- stringr::str_c(str_replace_all(titulo_capa, '_', ' '), 'Diagnosis', sep=' ')
#   output_file  <- str_glue("{titulo_capa}_diagnosis.pdf")
#   rmarkdown::render(
#     input         = "plantilla.Rmd", # pathname de la plantilla 
#     output_format = "pdf_document",
#     output_file   = output_file,
#     output_dir    = "datos_pdf/",
#     params        = list(
#       titulo = titulo,
#       show_code      = FALSE
#     )
#   )
# }

library(stringr)
library(rmarkdown)

for (i in 1:length(shps1)) { #length(shps1)
  titulo_capa <- shps1[i]
  titulo_sitio <- stringr::str_to_title(stringr::str_replace_all(titulo_capa, '_', ' '))
  titulo <- substring(titulo_sitio, first = 4) %>% str_squish()
  nombre<- substring(titulo_capa, first = 5) %>% str_squish()
  amb_title <-stringr::str_replace_all(amb[i], '_', ' ') %>% str_to_title()
  od <- "~/OCUC/PROYECTOS/catalogo_book/_book"
  
  rmarkdown::render(
    input = "~/OCUC/PROYECTOS/catalogo_book/_book/skele.Rmd",
    output_format = "all",
    output_dir = od,
    output_file= str_glue("{amb[i]}_{nombre}"),
    output_options = list(
      lib_dir = "~/OCUC/PROYECTOS/catalogo_book/_book/_book/libs",
      css = "~/OCUC/PROYECTOS/catalogo_book/_book/_book/libs/bs4_book-1.0.0/bs4_book.css"
    ),
    params = list(title = titulo, 
                  titulo_capa= titulo_capa, 
                  amb_title= amb_title)
  )
}


# 
# #may have to publish it as an RMD file seperately???? instead of as a qmd file
# for(i in 1:15){ #length(shps1)
#   titulo_capa<-shps1[i]
#   titulo_sitio <- stringr::str_c(str_replace_all(titulo_capa, '_', ' ')) %>% str_to_title()
#   output_name  <- str_glue("{titulo_capa}_diagnosis.html")
#   fecha<-format(Sys.time(), '%d %B, %Y')
#   titulo<- substring(titulo_sitio, first = 4)
#   rmarkdown::render(
#     input         = "~/OCUC/PROYECTOS/diagnosis_sitio/htmlplantilla.Rmd", # pathname de la plantilla 
#     output_file   = output_name,
#     output_format = "html_document",
#     output_dir = str_glue("posts/{amb[i]}/"),
#     path<-"E:/GDB_DATAMET.gdb",
#     params= list(
#       title = titulo,
#       show_code = FALSE)
#   )
# }

# rmarkdown::draft(
#   template = "~/OCUC/PROYECTOS/catalogo_book/inst/rmarkdown/templates/capa_plantilla",
#   file = str_glue("{od}/{titulo_capa}.Rmd"),
#   edit = FALSE,
#   params = list(titulo = titulo, show_code = FALSE)
# )
# fecha <- format(Sys.time(), '%d %B, %Y')
# output_name <- str_glue("{titulo_capa}.Rmd")




# set.seed(3)
# cn<-capa %>% st_drop_geometry() %>% sample_n(10)
# cn<-cn  %>% t() %>% data.frame() %>% rownames_to_column()
# names(cn)[1]<-"Nombre_columna"
# cn <- cn %>%
#   rowwise() %>%
#   mutate(Observaciones = paste(c_across(2:ncol(.)), collapse = ", ")) %>%
#   ungroup()
# cn<- cn %>% select(Nombre_columna, Observaciones)
# 
# library(gt)
# 
# cnt <- cn |>
#   gt() |>
#   tab_header(
#     title = "Muestra de 10 filas de los datos",
#     subtitle = str_glue("Ámbito {amb_title}")) |>
#   tab_style(
#     style = cell_text(size = px(12)),
#     locations = cells_body(
#       columns = Observaciones)) |>
#   tab_style(
#     style = list(
#       cell_text(weight = "bold", size = px(12)), 
#       cell_fill(color="lightgrey")),
#     locations = cells_body(
#       columns = Nombre_columna))
# 
# cnt

# colourer <- col_bin(
#   palette = c("#DD3E2C","transparent"),
#   domain = c(0,100), 
#   bins = c(0, 99,100))
# 
# colourer2 <- col_numeric(
#   palette = c("#ABD9E9","#FFFFBF"),
#   domain = c(0,100))
# 
# ft <- flextable(capa_diagnosis)
# ft <- theme_booktabs(ft, bold_header = TRUE) %>% 
#   flextable::align(align = "center", part="all") %>%
#   bg(bg = colourer, j = ~ pc_completo, part = "body") %>%
#     bg(bg = colourer2, j = ~ pc_repetido, part = "body") %>%
#   colformat_double(j = ~ pc_completo, digits = 2) %>% 
#   colformat_double(j = ~ pc_repetido, digits = 2) %>% 
#   fontsize(part = "all", size=9) 
# 
# # autofit(ft)
# ft