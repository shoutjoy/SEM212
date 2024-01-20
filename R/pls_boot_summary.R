#' Summary of PLS-SEM bootstrap results
#'
#' @param pls_boot bootstrap data
#' @param res 'all','loadings','loadings0','loading_bar','paths','plot'.'htmt','browse_plot','varName'
#' @param title your tite
#' @param boot default FALSE
#' @param nboot default 100
#' @param diagram default FALSE
#' @param rename default FALSE
#' @param var_name default NULL
#' @param vjust default -0.5
#'
#' @return boot sig result
#' @export
#'
#' @examples
#' \dontrun{
#' boot_srlapp_pls <- bootstrap_model(srlapp_pls, nboot = 10000, cores = 4)
#' boot_srlapp_pls
#' boot_srlapp_pls %>%  pls_boot_summary()
#'
#' boot_srlapp_pls %>%  pls_boot_summary(rename = T, var_name = varName_boot)
#'
#' varName_boot = c(
#'              "S_Review","S_Add_learn","S_Feedback","S-Focus_on",
#'              "SE_place","SE_time",
#'              "On_joy","On_easy","On_satisfy","On_paticipate" ,
#'              "IntensionUse",
#'              "A_upgrade","A_satisfy")
#'
#' }
#'
#'
pls_boot_summary <- function(pls_boot,
                             res="all",
                             title="",
                             boot=FALSE,
                             nboot= 100,
                             diagram=FALSE,
                             rename=FALSE,
                             var_name=NULL,
                             vjust=-0.5){

  library(tidyverse)
  library(kableExtra)
  library(seminr)
  if(boot==TRUE){
    set.seed(2023)
    boot_data <- bootstrap_model(pls_boot, nboot = nboot, cores = 4)
    pls_boot_summary <- summary(boot_data)

  }else if(boot==FALSE){
    boot_data <- pls_boot
    #summary
    pls_boot_summary <- summary(boot_data)
  }

  # plot =   plot(boot_data, title = title)

  if(diagram==TRUE){
    browse_plot =  browse_plot(boot_data)
    plot =   plot(boot_data, title = title)

  }else{
    browse_plot =NULL
    plot =   plot(boot_data, title = title)
    # loadings <-   pls_boot_summary$bootstrapped_loadings
    #   paths <-   pls_boot_summary$bootstrapped_total_paths

  }
  # data summary
  varNames =rownames(pls_boot_summary$bootstrapped_loadings)

  loadings_boot0 <- pls_boot_summary$bootstrapped_loadings[,c(1,4,5,6)]%>%
    as.data.frame() %>%
    dplyr::rename( loadings = `Original Est.`) %>%
    mutate( communality = loadings^2,
            variable=rownames(pls_boot_summary$bootstrapped_loadings),
            check=  `2.5% CI`*`97.5% CI`,
            sig = ifelse(check>0,"*","ns")) %>%
    dplyr::select(variable, loadings, communality,`T Stat.`,
                  `2.5% CI`,  `97.5% CI`, sig ) %>% tibble()


  #
  if(rename==TRUE){
    loadings_boot <-  cbind.data.frame(var_name, loadings_boot0)

    loadings_boot <-  loadings_boot %>%
      dplyr::select(variable, loadings, communality,`T Stat.`,
                    `2.5% CI`,  `97.5% CI`, sig ) %>% tibble() %>%
      separate(variable,c("old","latent"), " -> ") %>%
      mutate(item = var_name) %>%
      dplyr::select(latent,item, loadings, communality,
                    `T Stat.`,`2.5% CI`, `97.5% CI`, sig  ) %>%
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))



    #loadinsg bar plot
    loading_bar <- loadings_boot %>%
      ggplot(aes(x=item, #변경된 것을 사용
                 y=loadings))+
      geom_bar(stat = "Identity", aes(fill= item),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1,
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))


  }else if(rename==FALSE){
    # loadings_boot <- loadings_boot0%>%
    #   dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
    #                               ifelse(loadings >0.5,"fair","")))

    loadings_boot <-  loadings_boot0 %>%
      dplyr::select(variable, loadings, communality,`T Stat.`,
                    `2.5% CI`,  `97.5% CI`, sig ) %>% tibble() %>%
      separate(variable,c("item","latent"), " -> ") %>%
      dplyr::select(latent,item, loadings, communality,
                    `T Stat.`,`2.5% CI`, `97.5% CI`, sig  ) %>%
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))


    loading_bar <- loadings_boot %>%
      ggplot(aes(x=item, y=loadings))+
      geom_bar(stat = "Identity", aes(fill= item),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1,
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13,
                                       angle=90))
  }

  loadings_boot <- loadings_boot %>% arrange(item)


  paths_boot <- pls_boot_summary$bootstrapped_total_paths[,c(1,4,5,6)] %>%
    as.data.frame() %>%
    mutate(check= `2.5% CI`*`97.5% CI`) %>%
    mutate(sig=ifelse(check > 0,"*","ns")) %>%
    dplyr::select(1,2,3,4,6)


  htmt_boot <-   pls_boot_summary$bootstrapped_HTMT[,c(1,4,5,6)] %>%
    as.data.frame() %>%
    dplyr::rename(htmt_est= `Original Est.`, t = `T Stat.`,
                  lowerCI=`2.5% CI`, upperCI=`97.5% CI`) %>%
    mutate(sig = ifelse(htmt_est <0.9, "*",
                        ifelse(htmt_est < 1, ".","ns")))


  result= list(
    loadings = loadings_boot,
    paths = paths_boot,
    loading_bar=loading_bar,
    plot = plot,
    htmt_boot = htmt_boot,
    browse_plot = browse_plot,
    varNames
  )


  switch(res,
         all=result,
         loadings = loadings_boot,
         loadings0 = loadings_boot0,
         loading_bar=loading_bar,
         paths = paths_boot,
         plot = plot,
         htmt = htmt_boot,
         browse_plot = browse_plot,
         varName = varNames
  )
}
