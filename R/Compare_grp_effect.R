
#' Comparative analysis of direct effects by group
#'
#' @param global lavaan overall group analysis results
#' @param groupsem Analysis results by lavaan group
#' @param digit 3 round
#' @param title title
#' @param g1 group name 1
#' @param g2 group name 2
#' @param namestart group name extraction
#'
#' @return compare table
#' @export
#'
Compare_grp_effect <- function(global,
                               groupsem,
                               digit = 3,
                               title = "",
                               g1 = "g1",
                               g2 = "g2",
                               namestart = 8   #group name extraction
){


  gen_global <- global %>% Med_effect(effect1 = "DE") %>% dplyr::select(est)

  #Selecting group a
  rowname <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(path=substring(lhs,namestart),grp=substring(parameter,3)) %>%
    filter(grp=="a") %>%
    dplyr::select(path)# %>% filter(grp=="a")


  grp1_para_0 <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(grp=substring(parameter,3)) %>% filter(grp=="a")
  #selecting grup b
  grp2_para_0 <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(grp=substring(parameter,3)) %>% filter(grp=="b")
  #Difference Verification Extraction
  grp_diff_0 <- groupsem %>% Med_effect(type="ci",effect1 = "diff_D")



  # total_data <- rbind.data.frame(grp1_para,grp2_para,grp_diff, make.row.names = F)



  a1 <- grp_diff_0 %>% dplyr::select(parameter)
  a2 <- gen_global

  a3 <- grp1_para_0 %>% dplyr::select(est,sig)
  a4 <- grp2_para_0 %>% dplyr::select(est,sig)
  a5 <- grp_diff_0 %>% dplyr::select(est,se,z,pvalue,sig)


  groupDiff <- cbind(rowname, a1, a2, a3, a4, a5)
  groupDiff$path<-  str_replace(groupDiff[,1],"_"," -> ")

  # g1=""
  # g2=""

  colnames(groupDiff)=c("path","para","global",
                        paste0(g1,".est"),paste0(g1,".sig"),
                        paste0(g2,".est"),paste0(g2,".sig"),
                        "d.est","d.se","z","p","sig")

  #Multigroup table
  groupDE <-  rbind(grp1_para_0 %>% dplyr::select(grp, lhs, parameter, est, se,
                                                  std, z, sig, pvalue, ci.lower,
                                                  ci.upper),
                    grp2_para_0%>% dplyr::select(grp, lhs, parameter, est, se,
                                                 std, z, sig, pvalue, ci.lower,
                                                 ci.upper))
  groupDE$lhs <- str_replace(substring(groupDE[,2],namestart),"_"," -> ")

  colnames(groupDE)=c("grp","path","H1",
                      "est","se",
                      "std","z",
                      "sig","p","ci.lower","ci.upper")
  # groupDiff%>% kable("markdown", 3,caption = "다집단 비교 ")
  res<- list(groupDiff%>% kable("markdown",digits =digit ,
                                caption = paste("Multigroup_Wald Test: ",
                                                title)),
             # group_a = grp1_para_0,
             # group_b = grp2_para_0,
             groupDE%>% kable("markdown",digits =digit ,
                              caption = paste("Multigroup Comparison: ",
                                              title)),
             grp_DE_different = grp_diff_0
  )
  res
}
