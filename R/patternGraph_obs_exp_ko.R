
#' patternGraph_obs_exp
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param xlab xlab
#' @param ylab ylab
#' @param type res
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp_ko()
#'
#' }
patternGraph_obs_exp_ko = function(data,
                                   raw = TRUE,
                                   Ncol = NULL,
                                   yadd = 0.5,
                                   strip_size = 16,
                                   axis_size = 16,
                                   text_size = 13,
                                   size_bartext = 5,
                                   xlab = "성조형",
                                   ylab = "관측기대비율",
                                   type="g"
){

  # g = patternGraph1(data, raw = FALSE)

  data1 <- data

  data_long0 <- data1 %>% as.data.frame() %>%
    rownames_to_column("syllabic") %>%
    pivot_longer(
      # names_to = "accent",
      # values_to = "ratio",
      names_to = "성조형",values_to = '관측기대비율',
      cols=2: (ncol(data1)+1) )

  data_long_df <- data1 %>%
    long_df(
      # names_to = "accent", values_to="Freq",
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic")%>%
    jjstat::Round()

  data_long_oe <- data1 %>%obs_exp_table() %>%
    long_df(
      # names_to = "accent", values_to="ratio",
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic") %>%
    jjstat::Round()

  data_long_sig =  data1%>% p_sig_cal()%>%
    long_df(
      # names_to = "accent", values_to="star",
      names_to = "성조형",values_to = 'star',
      rowname = "syllabic")

  data_long_p =  data1%>% p_value_cal()%>%
    long_df(
      # names_to = "accent", values_to="p.value",
      names_to = "성조형",values_to = 'p.value',
      rowname = "syllabic")

  if(raw){
    #contigency table인 경우
    data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))

  }else{
    #관측/기대 표가 들어 온경우  kge_chisq_table에서 사용시
    data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))
  }




  g = g0 +geom_bar(stat = "identity", aes( fill = 성조형),
                   position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label =  Sig ),
              hjust = -0.1, size = size_bartext)+
    ylim(0,max(data_long_oe[, 3])+ yadd)+
    labs(x = xlab, y = ylab)+
    coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(size= text_size),
          axis.title = element_text(size= axis_size),
          strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = Ncol)

  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g= g

  switch(type, res = res, g=g)
}






#' patternGraph_obs_exp
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param xlab xlab
#' @param ylab ylab
#' @param type res
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp()
#'
#' }
patternGraph_obs_exp = function(data,
                                raw = TRUE,
                                Ncol = NULL,
                                yadd = 0.3,
                                # values_to = NULL,
                                # names_to =NULL,
                                strip_size = 16,
                                axis_size = 15,
                                text_size = 13,
                                size_bartext=4,
                                xlab = "성조형",
                                ylab = "관측기대비율",
                                type="g"
){

  # g = patternGraph1(data, raw = FALSE)

  data1 <- data

  data_long0 <- data1 %>% as.data.frame() %>%
    rownames_to_column("syllabic") %>%
    pivot_longer(names_to = "accent", values_to = "ratio",
                 cols=2: (ncol(data1)+1) )

  data_long_df <- data1 %>%
    long_df(names_to = "accent", values_to="Freq",
            rowname = "syllabic")

  data_long_oe <- data1 %>%obs_exp_table() %>%
    long_df(names_to = "accent", values_to="ratio",
            rowname = "syllabic") %>%
    jjstat::Round()

  data_long_sig =  data1%>% p_sig_cal()%>%
    long_df(names_to = "accent", values_to="star",
            rowname = "syllabic")

  data_long_p =  data1%>% p_value_cal()%>%
    long_df(names_to = "accent", values_to="p.value",
            rowname = "syllabic")



  if(raw){
    #contigency table인 경우
    data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, ratio, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = accent, y = ratio))

  }else{
    #관측/기대 표가 들어 온경우  kge_chisq_table에서 사용시
    data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, Freq, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = accent, y = Freq))
  }




  g = g0 + geom_bar(stat = "identity", aes( fill = accent),
                    position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    # geom_text(aes(label =  paste(round(ratio,2), star ) ),
    geom_text(aes(label =  Sig ),
              hjust = -0.1, size = size_bartext)+
    ylim(0,max(data_long_oe[, 3])+ yadd)+
    labs(x = xlab, y = ylab)+
    coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(size= text_size),
          axis.title = element_text(size= axis_size),
          strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = Ncol)

  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g= g

  switch(type, res = res, g=g)
}





#' patternGraph2
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param type g
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp()
#'
#' }
patternGraph2 = function(data, type="g",
                         raw = TRUE,
                         Ncol = NULL,
                         yadd = 0.45,
                         # values_to = NULL,
                         # names_to =NULL,
                         strip_size = 16,
                         axis_size = 16,
                         text_size = 14,
                         size_bartext=5

){

  # g = patternGraph1(data, raw = FALSE)

  data1 <- data$data_graph %>% t()

  data_long0 <- data1 %>% as.data.frame() %>%
    rownames_to_column("성조형") %>%
    pivot_longer(
      # names_to = "성조형",
      # values_to = "ratio",
      names_to = "음절",values_to = '관측기대비율',
      cols=2: (ncol(data1)+1) )

  data_long_df <- data1 %>%
    long_df(
      # names_to = "성조형", values_to="Freq",
      names_to = "음절",values_to = '관측기대비율',
      rowname = "성조형")%>%
    jjstat::Round()

  data_long_oe <- data1 %>%obs_exp_table() %>%
    long_df(
      # names_to = "성조형", values_to="ratio",
      names_to = "음절",values_to = '관측기대비율',
      rowname = "성조형") %>%
    jjstat::Round()

  data_long_sig =  data1%>% p_sig_cal()%>%
    long_df(
      # names_to = "성조형", values_to="star",
      names_to = "음절",values_to = 'star',
      rowname = "성조형")

  data_long_p =  data1%>% p_value_cal()%>%
    long_df(
      # names_to = "성조형", values_to="p.value",
      names_to = "음절",values_to = 'p.value',
      rowname = "성조형")

  if(raw){
    #contigency table인 경우
    data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 음절, y = 관측기대비율))

  }else{
    #관측/기대 표가 들어 온경우  kge_chisq_table에서 사용시
    data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 음절, y = 관측기대비율))
  }




  g = g0 +geom_bar(stat = "identity", aes( fill = 음절),
                   position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label =  Sig ),
              hjust = -0.1, size = size_bartext)+
    ylim(0,max(data_long_oe[, 3])+ yadd)+
    coord_flip()+
    theme_bw()+
    theme(
      axis.text.y =  element_text(size= text_size + 2),
      axis.text.x =  element_text(size= text_size - 1.5),
      axis.title = element_text(size= axis_size),
      strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ 성조형 , ncol = Ncol)

  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g= g

  switch(type,
         res = res,
         all = res,
         g=g)
}

