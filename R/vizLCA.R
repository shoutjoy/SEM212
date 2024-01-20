<<<<<<< HEAD

#' viz LCA Latent Class Analysis
#'
#' @param model_lca poLCA data
#' @param type res1, res2
#' @param text_angle text angle
#' @param class_name class name
#' @param intercept intercepts
#' @param Break c(0, 20,30, 50, 100)
#' @param size_y_text size =10
#' @param sel 1:20 Select range if necessary
#'
#' @return LCA result, plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(election)
#' library(glca)
#' data(gss08)
#' gss08na <- gss08 %>% drop_na()
#' gss08na
#' fm1 <- cbind(MORALG,CARESG,KNOWG,MORALB,CARESB,
#'              KNOWB,LEADB,DISHONB,INTELB)~ PARTY
#'
#' poLCA(formula=fm1,data=elec,nclass=3,nrep=10,graphs = T)%>% vizLCA()
#'
#' poLCA(cbind(DEFECT, HLTH,RAPE,POOR,SINGLE, NOMORE)~ SEX,
#'       data = gss08na, nclass = 3, graphs = TRUE ) %>% vizLCA()

#' poLCA1 <-  poLCA(cbind(DEFECT, HLTH,RAPE,POOR,SINGLE, NOMORE)~ SEX,
#'       data = gss08na, nclass = 3, graphs = TRUE ) %>% vizLCA()
#' poLCA1 %>% vizLCA()
#' poLCA1 %>% vizLCA(class_name = c("A","B","C"))
#' poLCA1 %>% vizLCA("df2") %>% print(n=Inf)
#' poLCA1 %>% vizLCA("g")
#' poLCA1 %>% vizLCA("pattern") %>%
#'   arrange( desc(observed)) %>% top_n(10)
#' poLCA1 %>% plot()
#' poLCA1 %>% vizLCA("g1")
#' poLCA1 %>% vizLCA("g2")
#' poLCA1 %>% vizLCA("g3")
#' poLCA1 %>% vizLCA("g4")
#' poLCA1 %>% vizLCA("class")
#' poLCA1 %>% vizLCA("pattern")
#' poLCA1 %>% vizLCA("pattern_data") %>% top_n(10)
#' poLCA1 %>% vizLCA("class") %>%
#'    dplyr::select(1,2,3,4,5,6,7,8,9)
#'
#' model_n3
#' model_n3 %>% vizLCA()
#' model_n3 %>% vizLCA(class_name = c("낙태반대","조건부찬성","낙태찬성"))
#' model_n3 %>% vizLCA("res1",size_y_text = 12)
#' model_n3 %>% vizLCA("class")
#' model_n3 %>% vizLCA("df1")
#' model_n3 %>% vizLCA("df2")
#' model_n3 %>% vizLCA("df3")
#' model_n3 %>% vizLCA("df3")
#' model_n3 %>% vizLCA("g")
#' model_n3 %>% vizLCA("g1")
#' model_n3 %>% vizLCA("g2")
#' model_n3 %>% vizLCA("g3") #error nothing
#' model_n3 %>% vizLCA("g4") #error  nothing
#' model_n3 %>% vizLCA("pattern_data")
#' }
vizLCA = function(model_lca, #poLCA result
                  type="res2",
                  text_angle = 90,
                  class_name = NULL,
                  intercept = 10,
                  Break = c(0, 20,30, 50, 100),
                  size_y_text = 10,
                  sel=1:20){

  library(tidyverse)

  model_lca  <-  model_lca
  # Item response probability
  item_resp_prob = model_lca$probs

  class_number = nrow(item_resp_prob[[1]])


  # Visualization ------- Class check

  p = bind_rows(Class = paste0("Class",1:class_number),
                class_population_shares= model_lca$P) #%>%
  # arrange(class_population_shares)


  #pattern
  pattern_grp0 = model_lca$predcell
  pattern_grp = model_lca$predcell %>%
    arrange(desc(observed))


  #Creating patterns
  ddd = list()
  for(i in 1:nrow(model_lca$predcell)){
    ddd = bind_rows(
      ddd, tibble(
        pattern = str_c("(",
                        paste0(model_lca$predcell[i,
                                                  1: (ncol(model_lca$predcell)-2)],
                               collapse = ",") ,")"  )))
  }
  # ddd
  pattern_data = bind_cols(pattern = ddd,
                           n = model_lca$predcell$observed) %>%
    arrange(desc(n)) %>%slice(sel)

  #visualization pattern
  g_patten = pattern_data%>%
    ggplot(aes(x = reorder(pattern, n) , y = n))+
    geom_bar(stat="identity")+
    geom_hline(yintercept = intercept, color= "red", linetype=2)+
    geom_text(aes(label = n), hjust =-0.4)+
    scale_y_continuous(breaks = Break )+
    coord_flip()+
    labs(x="responsePattern\n", y = "freqency",
         title = "Frequency of occurrence by reaction pattern")+
    theme_bw()



  N = nrow(model_lca$predcell)
  log_likelihood =  model_lca$llik

  model_fit = bind_cols(AIC = model_lca$aic ,
                        BIC = model_lca$bic,
                        Gsq = model_lca$Gsq,
                        Chisq = model_lca$Chisq,
                        npar = model_lca$npar
  ) %>% mutate(LL2 = AIC -2*npar, #Log Likelood
               CAIC = LL2 + (1 + log(N ))*npar
  ) #%>%  dplyr::select(1,2,3,4,7)
  # # consistent AIC  CAIC
  interp = c("The smaller the AIC value, the better the BIC value. However, AIC does not reflect the sample size, and BIC does reflect the sample size. Therefore, you can see the value of Consistence AIC (CAIC), which is an AIC calculated by reflecting the table size, and the smaller the value, the better the model.")

  table_n = ncol(model_lca$y)
  # item_resp_prob
  # Counting item response probability

  summary_t_1 = do.call(rbind, model_lca$probs) %>%
    as_tibble() %>%
    mutate(item = rep(names(model_lca$probs),
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])),
                        ncol(model_lca$y) )) %>%
    `colnames<-`(c( paste0("r",1:ncol(model_lca$probs[[1]]) ),
                    "item","Class"))


  summary_t_2 <- summary_t_1 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )

  summary_t_3 = do.call(rbind, model_lca$probs) %>%
    as_tibble() %>%
    mutate(item = rep(names(model_lca$probs),
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])),
                        ncol(model_lca$y) ))


  summary_t_4 <- summary_t_3 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )

  r_name = unique(model_lca$y[,1]) %>% as.character()

  # class type
  # wide data  summary
  wide_summary = summary_t_1 %>%
    # spread(key= Class, value = YES)
    pivot_wider(names_from = Class,
                values_from  = -c("item","Class"))
  # wide_summary <- wide_summary0 %>%
  # dplyr::select(1: )
  if(is.null(model_lca$coeff)){
    logit_reg=NULL
  }else{
    if(class_number==3){
      logit_reg = rbind(
        cbind(model_lca$coeff[1] ,model_lca$coeff.se[1]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("2/1 (intercept)")),
        cbind(model_lca$coeff[2] ,model_lca$coeff.se[2]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("2/1 corvariance")),
        cbind(model_lca$coeff[3] ,model_lca$coeff.se[3]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("3/1 (intercept)")),
        cbind(model_lca$coeff[4] ,model_lca$coeff.se[4]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("3/1 corvariance"))
      ) %>%data.frame() %>%  rownames_to_column() %>%
        mutate(t = Coefficient/Std.error,
               p.value = 1-pt(abs(t), model_lca$resid.df)) %>%
        p_mark_sig()
    }
  }


  #visualization
  bar= apply(model_lca$posterior, 1, which.max) %>%
    table() %>%
    tibble() %>%
    `colnames<-`("freq") %>%
    mutate(freq = as.factor(freq),
           Class = str_c("Class", 1:ncol(model_lca$posterior) )) %>%
    ggplot()+
    geom_bar(aes(x=Class, y=freq), stat = "identity")+
    geom_text(aes(x=Class, y=freq, label = freq),
              vjust=-.5, size= 5)


  #bar graph
  g = summary_t_2 %>%
    ggplot()+
    geom_bar(aes(x = item, y = r1, fill= item),
             stat = 'identity',
             show.legend = FALSE)+
    labs(x= "측정문항", y = "YES응답확률")+
    coord_flip()+
    theme_bw()+theme(legend.position = "top")+
    theme(axis.text.y = element_text(size= size_y_text))+
    facet_wrap(~ Class_p)


  #line graph

  g2 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r1), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r1, color= item),
               size=3, alpha= 0.8)+
    labs(x= "estimateItem", y = "YES_Response",
         caption = r_name[1])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)

  g3 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r2), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r2, color= item),
               size=3, alpha= 0.8)+
    labs(x= "estimateItem", y = "YES_Response",
         caption = r_name[2])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)

  if(is.null(summary_t_2$r3)){
    g4 = "No variables"
    g4
  }else{
    g4 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r3), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r3, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "estimateItem", y = "YES_Response",
           caption = r_name[3])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }

  if(is.null(summary_t_2$r4)){
    g5 = "No variables"
    g5
  }else{
    g5 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r4), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r4, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "estimateItem", y = "YES_Response",
           caption = r_name[4])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }


  # library(gridExtra)

  res1=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,
    # source2= summary_t_2, # Should not be printed, for review only
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz2_Class_group = g
  )
  res2=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,

    # source2= summary_t_2,  # Should not be printed, for review only
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    Number_of_patterns = N,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz4_Class_group = g5,
    viz3_Class_group = g4,
    viz2_Class_group = g3,
    viz1_Class_main = g2
  )

  switch(type,
         res1 = res1,
         res2 = res2,
         class = wide_summary,
         model_fit = model_fit,
         df1 = summary_t_1,
         df2 = summary_t_2,
         df3 = summary_t_3,
         df4 = summary_t_4,
         bar = bar,
         llik = log_likelihood,
         logit_reg = logit_reg,
         pattern =  model_lca$predcell,
         pattern_data = pattern_data,
         pattern_g = g_patten,
         g = g,
         g1 = g2,
         g2 = g3,
         g3 = g4,
         g4 = g5
  )
  # res

}
=======

#' viz LCA Latent Class Analysis
#'
#' @param model_lca poLCA data
#' @param type res1, res2
#' @param text_angle text angle
#' @param class_name class name
#' @param intercept intercepts
#' @param Break c(0, 20,30, 50, 100)
#' @param size_y_text size =10
#' @param sel 1:20 Select range if necessary
#'
#' @return LCA result, plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(election)
#' library(glca)
#' data(gss08)
#' gss08na <- gss08 %>% drop_na()
#' gss08na
#' fm1 <- cbind(MORALG,CARESG,KNOWG,MORALB,CARESB,
#'              KNOWB,LEADB,DISHONB,INTELB)~ PARTY
#'
#' poLCA(formula=fm1,data=elec,nclass=3,nrep=10,graphs = T)%>% vizLCA()
#'
#' poLCA(cbind(DEFECT, HLTH,RAPE,POOR,SINGLE, NOMORE)~ SEX,
#'       data = gss08na, nclass = 3, graphs = TRUE ) %>% vizLCA()

#' poLCA1 <-  poLCA(cbind(DEFECT, HLTH,RAPE,POOR,SINGLE, NOMORE)~ SEX,
#'       data = gss08na, nclass = 3, graphs = TRUE ) %>% vizLCA()
#' poLCA1 %>% vizLCA()
#' poLCA1 %>% vizLCA(class_name = c("A","B","C"))
#' poLCA1 %>% vizLCA("df2") %>% print(n=Inf)
#' poLCA1 %>% vizLCA("g")
#' poLCA1 %>% vizLCA("pattern") %>%
#'   arrange( desc(observed)) %>% top_n(10)
#' poLCA1 %>% plot()
#' poLCA1 %>% vizLCA("g1")
#' poLCA1 %>% vizLCA("g2")
#' poLCA1 %>% vizLCA("g3")
#' poLCA1 %>% vizLCA("g4")
#' poLCA1 %>% vizLCA("class")
#' poLCA1 %>% vizLCA("pattern")
#' poLCA1 %>% vizLCA("pattern_data") %>% top_n(10)
#' poLCA1 %>% vizLCA("class") %>%
#'    dplyr::select(1,2,3,4,5,6,7,8,9)
#'
#' model_n3
#' model_n3 %>% vizLCA()
#' model_n3 %>% vizLCA(class_name = c("낙태반대","조건부찬성","낙태찬성"))
#' model_n3 %>% vizLCA("res1",size_y_text = 12)
#' model_n3 %>% vizLCA("class")
#' model_n3 %>% vizLCA("df1")
#' model_n3 %>% vizLCA("df2")
#' model_n3 %>% vizLCA("df3")
#' model_n3 %>% vizLCA("df3")
#' model_n3 %>% vizLCA("g")
#' model_n3 %>% vizLCA("g1")
#' model_n3 %>% vizLCA("g2")
#' model_n3 %>% vizLCA("g3") #error nothing
#' model_n3 %>% vizLCA("g4") #error  nothing
#' model_n3 %>% vizLCA("pattern_data")
#' }
vizLCA = function(model_lca, #poLCA result
                  type="res2",
                  text_angle = 90,
                  class_name = NULL,
                  intercept = 10,
                  Break = c(0, 20,30, 50, 100),
                  size_y_text = 10,
                  sel=1:20){



  model_lca  <-  model_lca
  # Item response probability
  item_resp_prob = model_lca$probs

  class_number = nrow(item_resp_prob[[1]])


  # Visualization ------- Class check

  p = bind_rows(Class = paste0("Class",1:class_number),
                class_population_shares= model_lca$P) #%>%
  # arrange(class_population_shares)


  #pattern
  pattern_grp0 = model_lca$predcell
  pattern_grp = model_lca$predcell %>%
    arrange(desc(observed))


  #Creating patterns
  ddd = list()
  for(i in 1:nrow(model_lca$predcell)){
    ddd = bind_rows(
      ddd, tibble(
        pattern = str_c("(",
                        paste0(model_lca$predcell[i,
                                                  1: (ncol(model_lca$predcell)-2)],
                               collapse = ",") ,")"  )))
  }
  # ddd
  pattern_data = bind_cols(pattern = ddd,
                           n = model_lca$predcell$observed) %>%
    arrange(desc(n)) %>%slice(sel)

  #visualization pattern
  g_patten = pattern_data%>%
    ggplot(aes(x = reorder(pattern, n) , y = n))+
    geom_bar(stat="identity")+
    geom_hline(yintercept = intercept, color= "red", linetype=2)+
    geom_text(aes(label = n), hjust =-0.4)+
    scale_y_continuous(breaks = Break )+
    coord_flip()+
    labs(x="responsePattern\n", y = "freqency",
         title = "Frequency of occurrence by reaction pattern")+
    theme_bw()



  N = nrow(model_lca$predcell)
  log_likelihood =  model_lca$llik

  model_fit = bind_cols(AIC = model_lca$aic ,
                        BIC = model_lca$bic,
                        Gsq = model_lca$Gsq,
                        Chisq = model_lca$Chisq,
                        npar = model_lca$npar
  ) %>% mutate(LL2 = AIC -2*npar, #Log Likelood
               CAIC = LL2 + (1 + log(N ))*npar
  ) #%>%  dplyr::select(1,2,3,4,7)
  # # consistent AIC  CAIC
  interp = c("The smaller the AIC value, the better the BIC value. However, AIC does not reflect the sample size, and BIC does reflect the sample size. Therefore, you can see the value of Consistence AIC (CAIC), which is an AIC calculated by reflecting the table size, and the smaller the value, the better the model.")

  table_n = ncol(model_lca$y)
  # item_resp_prob
  # Counting item response probability

  summary_t_1 = do.call(rbind, model_lca$probs) %>%
    as_tibble() %>%
    mutate(item = rep(names(model_lca$probs),
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])),
                        ncol(model_lca$y) )) %>%
    `colnames<-`(c( paste0("r",1:ncol(model_lca$probs[[1]]) ),
                    "item","Class"))


  summary_t_2 <- summary_t_1 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )

  summary_t_3 = do.call(rbind, model_lca$probs) %>%
    as_tibble() %>%
    mutate(item = rep(names(model_lca$probs),
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])),
                        ncol(model_lca$y) ))


  summary_t_4 <- summary_t_3 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )

  r_name = unique(model_lca$y[,1]) %>% as.character()

  # class type
  # wide data  summary
  wide_summary = summary_t_1 %>%
    # spread(key= Class, value = YES)
    pivot_wider(names_from = Class,
                values_from  = -c("item","Class"))
  # wide_summary <- wide_summary0 %>%
  # dplyr::select(1: )
  if(is.null(model_lca$coeff)){
    logit_reg=NULL
  }else{
    if(class_number==3){
      logit_reg = rbind(
        cbind(model_lca$coeff[1] ,model_lca$coeff.se[1]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("2/1 (intercept)")),
        cbind(model_lca$coeff[2] ,model_lca$coeff.se[2]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("2/1 corvariance")),
        cbind(model_lca$coeff[3] ,model_lca$coeff.se[3]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("3/1 (intercept)")),
        cbind(model_lca$coeff[4] ,model_lca$coeff.se[4]) %>%
          `colnames<-`(c("Coefficient","Std.error")) %>%
          `rownames<-`(c("3/1 corvariance"))
      ) %>%data.frame() %>%  rownames_to_column() %>%
        mutate(t = Coefficient/Std.error,
               p.value = 1-pt(abs(t), model_lca$resid.df)) %>%
        p_mark_sig()
    }
  }


  #visualization
  bar= apply(model_lca$posterior, 1, which.max) %>%
    table() %>%
    tibble() %>%
    `colnames<-`("freq") %>%
    mutate(freq = as.factor(freq),
           Class = str_c("Class", 1:ncol(model_lca$posterior) )) %>%
    ggplot()+
    geom_bar(aes(x=Class, y=freq), stat = "identity")+
    geom_text(aes(x=Class, y=freq, label = freq),
              vjust=-.5, size= 5)


  #bar graph
  g = summary_t_2 %>%
    ggplot()+
    geom_bar(aes(x = item, y = r1, fill= item),
             stat = 'identity',
             show.legend = FALSE)+
    labs(x= "estimateItem", y = "YES_Response")+
    coord_flip()+
    theme_bw()+theme(legend.position = "top")+
    theme(axis.text.y = element_text(size= size_y_text))+
    facet_wrap(~ Class_p)


  #line graph

  g2 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r1), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r1, color= item),
               size=3, alpha= 0.8)+
    labs(x= "estimateItem", y = "YES_Response",
         caption = r_name[1])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)

  g3 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r2), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r2, color= item),
               size=3, alpha= 0.8)+
    labs(x= "estimateItem", y = "YES_Response",
         caption = r_name[2])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)

  if(is.null(summary_t_2$r3)){
    g4 = "No variables"
    g4
  }else{
    g4 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r3), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r3, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "estimateItem", y = "YES_Response",
           caption = r_name[3])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }

  if(is.null(summary_t_2$r4)){
    g5 = "No variables"
    g5
  }else{
    g5 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r4), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r4, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "estimateItem", y = "YES_Response",
           caption = r_name[4])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }


  # library(gridExtra)

  res1=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,
    # source2= summary_t_2, # Should not be printed, for review only
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz2_Class_group = g
  )
  res2=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,

    # source2= summary_t_2,  # Should not be printed, for review only
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    Number_of_patterns = N,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz4_Class_group = g5,
    viz3_Class_group = g4,
    viz2_Class_group = g3,
    viz1_Class_main = g2
  )

  switch(type,
         res1 = res1,
         res2 = res2,
         class = wide_summary,
         model_fit = model_fit,
         df1 = summary_t_1,
         df2 = summary_t_2,
         df3 = summary_t_3,
         df4 = summary_t_4,
         bar = bar,
         llik = log_likelihood,
         logit_reg = logit_reg,
         pattern =  model_lca$predcell,
         pattern_data = pattern_data,
         pattern_g = g_patten,
         g = g,
         g1 = g2,
         g2 = g3,
         g3 = g4,
         g4 = g5
  )
  # res

}
>>>>>>> fd47d11 (error revise)
