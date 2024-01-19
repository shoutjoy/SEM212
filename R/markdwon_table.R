#' A tool that outputs data to the viewer
#' @export
#' @param lm_data data.frame or etc data
#' @param caption table title
#' @param digits table values roundings
#' @param font_size table font size
#' @param full_width table wide and fit values
#' @param table tbble type 'paper', 'classic', 'dark', 'minimal'
#' @param show 'markdown' is output to the viewer, and 'data' is output to the console.
#' @param format 'markdown' and 'html'. default 'html'.
#' @param row.names row names in table(Set if necessary)
#' @param col.names column names in table (Set if necessary)
#' @param align Table value align
#' @param label Table label
#' @param format.args format.args
#' @param escape escape
#' @param table.attr table.attr
#' @param booktabs booktabs
#' @param longtable longtable
#' @param valign valign
#' @param position position
#' @param centering Align cener in Table
#' @example markdown_table(mtcars[1:5,], table="minimal")





#quick markdown_table , kable(format="html) using cfa2()-----------

markdown_table <- function(lm_data,
                           caption = "<Table  > capiton title",
                           digits = 2,
                           font_size= 18,
                           full_width= F,
                           table ="classic",
                           show="data",
                           format="html",
                           row.names = NA,
                           col.names = NA,
                           align=NULL,
                           label = NULL,
                           format.args = list(),
                           escape = TRUE,
                           table.attr = "",
                           booktabs = FALSE,
                           longtable = FALSE,
                           valign = "t",
                           position = "",
                           centering = TRUE
){
  library(tidyverse)
  library(kableExtra)
  library(broom)
  #논문에 넣을때 복사하여 넣을 것
  cat(" *** : p < .001, ** : p < .01, * : p < .05")

  if(show =="lm"){
    #논문 테이블 Viewer
    lm_data %>%
      tidy() %>% # tibble data
      mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "")))) %>%
      kbl(digits = digits,
          caption =  caption) %>%
      kable_classic(full_width=full_width, font_size= font_size)
  }else if(show =="p_add"){
    lm_data %>%as.data.frame() %>%
      mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "")))) %>%
      kbl(digits = digits,
          caption =  caption) %>%
      kable_classic(full_width=full_width,font_size= font_size)
  }else if(show =="data"){
    lm_data<- lm_data %>% as.data.frame() %>%
      kbl(digits = digits,
          format= format,
          # caption =  caption,
          row.names = row.names,
          col.names = col.names,
          align = align,
          caption = caption,
          label = label,
          format.args = list(),
          escape = escape,
          table.attr = table.attr,
          booktabs = booktabs,
          longtable = FALSE,
          valign = valign,
          position = position,
          centering = TRUE
      )

    if(table=="paper"){
      lm_data %>%  kable_paper(full_width = full_width,
                               font_size =  font_size)
    }else if(table =="classic"){
      lm_data %>% kable_classic(full_width = full_width,
                                font_size =  font_size)
    }else if(table =="dark"){
      lm_data %>% kable_material_dark(full_width = full_width,
                                      font_size =  font_size)
    }else if(table =="minimal"){
      lm_data %>% kable_minimal(full_width = full_width,
                                font_size =  font_size)
    }

  }
}
# ?kable_classic
#data에 대한 것을 Viewer로 출력
# ?kbl

