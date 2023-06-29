library(methods)
library(knitr)
opts_chunk$set(
        background = "#FCFCFC", # code chunk color in latex
        comment = "#>",
        collapse = TRUE,
        echo = TRUE, 
        ft.tabcolsep=0,
        # The following line speeds-up the build.
        # Uncomment it to avoid cached data (which can cause issues):
        cache = TRUE,
        fig.pos = "t",
        fig.path = "figures/",
        fig.align = "center",
        fig.width = 6,
        fig.asp = 0.618,  # 1 / phi
        fig.show = "hold",
        out.width = "100%",
        dpi = 105 # this creates 2*105 dpi at 6in, which is 300 dpi at 4.2in, see the  EmilHvitfeldt/smltar repo
)
# https://github.com/EmilHvitfeldt/smltar/issues/114
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
        # this hook is used only when the linewidth option is not NULL
        if (!is.null(n <- options$linewidth)) {
                x = knitr:::split_lines(x)
                # any lines wider than n should be wrapped
                if (any(nchar(x) > n)) x = strwrap(x, width = n)
                x = paste(x, collapse = '\n')
        }
        hook_output(x, options)
},
               crop = knitr::hook_pdfcrop)

set.seed(2023)
options(digits = 3)
options(dplyr.print_min = 4, dplyr.print_max = 4)
# Hide proj4 warnings:
options("rgdal_show_exportToProj4_warnings" = "none")


pacman::p_load(tidyselect, tidyverse, dplyr, glue, knitr, stringr, magrittr, 
               sf, ggmap, ggplot2, flextable, data.table, sp, 
               scales, devtools, tinytex, writexl, webshot, readr)
# library(arcgisbinding)
# arc.check_product()
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