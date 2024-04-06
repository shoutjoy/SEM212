#' Correspondence analysis visualization functions
#'
#' @param dataset data
#' @param typekey filter
#' @param selcol_3 col3
#' @param type all
#' @param arrows c(F,T)
#' @param selcol_1 col1
#' @param selcol_2 col2
#' @param area 지역
#' @param addtext add text
#' @param xlim range c(0, 0.3)
#' @param ylim rangec(-0.3, 0.4)
#' @param size_text 5
#' @param opt opt 1~ 6
kge_ca_analysis= function(dataset,
                      typekey,
                      selcol_3 = "w1f",
                      type="all",
                      arrows = c(F,T),
                      selcol_1 = "성조",
                      selcol_2 = "onset",
                      area = "부산",
                      addtext = "",
                      xlim = NULL, #c(0, 0.3),
                      ylim = NULL, #c(-0.3, 0.4)
                      size_text= 5,
                      opt=1

){



  if(selcol_3=="a3"){
    data = dataset %>%
      rename(onset = a1,
             # coda = a3,
             Coda = selcol_3
      )

    selcol_3 ="Coda"

  }else{
    data = dataset %>%
      rename(onset = a1,
             # coda = a3,
             Weight = selcol_3
      )
    selcol_3="Weight"
  }


  #1,2,4와 3,5,6이 같음
  if(opt==1){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_1,"+", selcol_2, "+", selcol_3) ))
  }else if(opt==2){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_1,"+", selcol_3, "+", selcol_2) ))
  }else if(opt==3){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_2,"+", selcol_1, "+", selcol_3) ))

  }else if(opt==4){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_2,"+", selcol_3, "+", selcol_1) ))
  }else if(opt==5){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_3,"+", selcol_1, "+", selcol_2) ))
  }else if(opt==6){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_3,"+", selcol_2, "+", selcol_1) ))
  }

  df = df0%>% ca::mjca()
  # df_graph = df$cols
  explain =   round((df$inertia.e[1] +df$inertia.e[2])*100 , 2)

  g = df %>% plot(arrows = arrows, col = 1:ncol(df0),
                  main = paste( typekey,
                                "에 관한 ",selcol_1,"과 ",selcol_2," 다중대응분석의 총 설명력(",
                                explain , "%)"),
                  xlim = xlim,
                  ylim = ylim,
                  cex.lab = 1.2,
                  cex.sub = 1.2,cex=2,
                  pch = 17, bg=1:3)

  # graph_df= g$cols

  graph_df = g$cols %>% data.frame() %>%
    rownames_to_column("var") %>%
    separate(var, c("factor", "level"), remove = FALSE, sep=":") %>%
    select(-1)


  graph_df_md = graph_df %>%
    jjstat::markdown_table(caption = paste0(typekey,
                                            "(",area,")-",addtext,
                                            "-다중대응분석 좌표점(",explain,"%)" ),
                           digits = 4)

  gplot = g$cols %>% data.frame() %>%
    rownames_to_column("var") %>%
    separate(var, c("factor", "level"), remove = FALSE, sep=":") %>%
    ggplot(aes(x= Dim1, y=Dim2))+
    geom_point(aes(color= factor), size=4, show.legend = FALSE)+
    ggrepel::geom_text_repel(aes(label= var),
                             # geom_text(aes(label= var),
                             size = size_text,
                             vjust = -0.7,
                             hjust = -0.1)+
    theme_bw()+
    geom_vline(xintercept = 0, linetype=2)+
    geom_hline(yintercept = 0, linetype=2)+
    labs(
      x = paste0("Dim1(", round(df$inertia.e[1]*100, 2),"%)"),
      y = paste0("Dim2(", round(df$inertia.e[2]*100, 2),"%)"),
      title = paste0("다중 대응분석(multiple Correspondence Analysis): ",
                     selcol_1,", ",
                     selcol_2,", ",
                     selcol_3,"에 대한 ",
                     "총 설명력(",explain,"%)")
    )+
    theme(axis.text = element_text(size=12))



  # res =  list( df, g, df0)
  res =  list(ca_res=df,
              graph_df= g$cols,
              graph_df_md = graph_df_md,
              g=g,
              gplot= gplot)

  switch(type,
         res = res,
         all = res,
         ca_res=df,
         graph_df_md = graph_df_md,
         graph_df= g$cols,
         g=g,
         gplot= gplot
  )
}
