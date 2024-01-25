#' Group comparison and graph
#'
#' @param data seminr group result :  estimate_pls_mga
#' @param grp1 first group name
#' @param grp2 second group name
#' @param res 'all'.'plot','plot_data','res_data'
#'
#' @return graph and data
#' @export
#'
#' @examples
#' \dontrun{
#' library(seminr)
#' data(stat_onl)
#'
#' ### model
#' library(seminr)
#' lmodel1 <- constructs(
#'   composite("SRL_meta", single_item("S_Review"), weights = mode_A),
#'   composite("SRL_meta", single_item("S_Add_learn"), weights = mode_A),
#'   composite("SRL_meta", single_item("S_Feedback"), weights = mode_A),
#'   composite("SRL_meta", single_item("S_Focus_on"), weights = mode_A),
#'
#'   composite("SRL_env", single_item("SE_Place"), weights = mode_A),
#'   composite("SRL_env", single_item("SE_Time"), weights = mode_A),
#'
#'  composite("OnlineSAT", single_item("On_Joy"), weights = mode_A),
#'   composite("OnlineSAT", single_item("On_Easy"), weights = mode_A),
#'   composite("OnlineSAT", single_item("On_Satisfy"), weights = mode_A),
#'   composite("OnlineSAT", single_item("On_Engage" ), weights = mode_A),
#'
#'   composite("USEIntent", single_item("Intension_use"), weights = mode_A),
#'   composite("AppSatisfy", single_item("upgrade"), weights = mode_A),
#'   composite("AppSatisfy", single_item("satisfy"), weights = mode_A)
#' )

#' ## structure model ---------------
#' lstr1 = relationships(
#'   paths(from=c("AppSatisfy"), to=c("SRL_meta", "SRL_env" ,"OnlineSAT")),
#'   paths(from=c("SRL_meta", "SRL_env"),to=c( "OnlineSAT")),
#'   paths(from=c("SRL_meta", "SRL_env"),to=c( "USEIntent")),
#'   paths(from=c("OnlineSAT"),to=c( "USEIntent")))
#'
#' ## model estimate ----------------
#' srlapp_pls  <- estimate_pls(data = stat_onl,
#'                             measurement_model = lmodel1,
#'                             structural_model = lstr1)
#'

#' ##group comparison
#' srlapp_pls_gender <-  estimate_pls_mga(srlapp_pls,
#'                          stat_onl$gender == "male", cores=6 )
#' ###  Group Comparison between genders
#' srlapp_pls_gender %>% mga_comparison_plot("female","male")
#' srlapp_pls_gender %>%
#'   mga_comparison_plot("female","male","res_data") %>%
#'   markdown_table("Comparison of path coefficients between gender groups",
#'                  digits = 3 )
#'
#'
#' }
#'
#'
mga_comparison_plot <- function(data,
                                grp1="grp1_est",
                                grp2="grp2_est",
                                res="all"){

  plot_data <-   data %>% tibble::tibble() %>%
    dplyr::mutate(path = paste0(source," -> ",target),
                  hypothesis = paste0("H",1:8)) %>%
    dplyr::select(10,11,3,4,5) %>%
    dplyr::rename(male =group1_beta,
                  female = group2_beta,
                  total=estimate
    ) %>%
    tidyr::pivot_longer(names_to = "est",
                        values_to = "value",
                        cols = -c(path, hypothesis))


  g <-  plot_data %>% dplyr::filter(est != "total") %>%
    ggplot2::ggplot(aes(x=path, y= value))+
    geom_bar(stat="Identity",
             position = "dodge",  #" position_dodge()",
             aes(x=path, y= value, fill = est ),
             color = "black", show.legend = F  )+
   ggrepel::geom_text_repel(aes(y= value,
                        label=paste0(substr(est,1,1),":",round(value, 2)),
                        group=est),
                    vjust=-.6, hjust= 0.4, size= 4,
                    position = position_dodge(width = 0.9),
    )+
    ylim(-0.2, 1.1)+
    theme_bw()+
    theme(axis.text.x = element_text(size=12, angle=90))+
    scale_fill_grey(start = 0.1, end = 0.9)


  res_data= data%>% tibble::tibble() %>%
    dplyr::mutate(path= paste0(source," -> ",target),
           hypothesis = paste0("H",1:8),
           sig = ifelse( data[, ncol(data)] < 0.001, "***",
                         ifelse( data[, ncol(data)] < 0.01, "**",
                                 ifelse( data[, ncol(data)] < 0.05, "*",
                                         "")))) %>%
    dplyr::select(10,11,3,4,5,6,9,12)
    colnames(res_data)=c("Path(Exo -> Endo)","H","Total.est",
                       grp1,grp2,"diff","p","Accept")

  switch(res,
         all = list(g, res_data),
         plot = g,
         plot_data = plot_data,
         res_data = res_data)
}


