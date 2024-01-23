#' seminr pls-SEM CFA
#'
#' @param pls_data seminr result
#' @param res   'all','loadings','loadings_rename','CR_AVE','CR_AVE_sig','FL_criteria','HTMT','crossloadings','f2','reliability_plot','loading_bar','browse_Viewer','effect','mediation','indirect_effect','Indirect_effect','Total_effect','total_effect','Direct_effect','direct_effect','r2','str_cor','paths','loadings_boot','paths_boot','htmt_boot','vif','vifr','vifc','summarise'
#' @param diagram  deafult FALSE
#' @param reliability_plot deafult FALSE
#' @param digits deafult 3
#' @param boot deafult FALSE
#' @param nboot deafult 100
#' @param rename deafult FALSE
#' @param var_name deafult NULL
#' @param vjust deafult -0.5
#'
#' @return variout result
#' @export
#'
#' @examples
#' \dontrun{
#' ## Research model : Park (2023) constructs a model by selecting variables that fit the theoretical background.
#' ## JH, Park(2023) Exploring the Influence of  Feature Recognition and Usage Satisfaction in Online Learning Applications on Learning Satisfaction and Continued Learning Intentions: using Multivariate analysis and PLS-SEM
#' data(stat_onl)
#' str(stat_onl)
#'
#' library(seminr)
#'
#' ## Establishing a model for observed variables (items)
#'
#' srl_model="
#' SRL_meta =~ Review + Add_learn + Feedback + Focus_on
#' SRL_env =~ Place + Time
#' OnlineSAT =~ Joy + Easy + Satisgy + Engage
#' USEIntent =~ IntentUse
#' AppSatisfy =~ Upgrade + Appsatisfy
#'
#' ## Structural model establishes causal relationships
#' SRL_meta ~ AppSatisfy
#' SRL_env ~ AppSatisfy
#' OnlineSAT ~ AppSatisfy
#' OnlineSAT ~ SRL_meta + SRL_env
#' USEIntent ~ SRL_meta + SRL_env + OnlineSAT
#' "
#' # Output a plot of the model using the seminr function
#' browse_plot( csem2seminr(srl_model))
#' # output Viewer
#' plot( csem2seminr(srl_model))


#' ### model
#' library(seminr)
#' lmodel1 <- constructs(
#'   composite("SRL_meta", single_item("메타인지2"), weights = mode_A),
#'   composite("SRL_meta", single_item("메타인지3"), weights = mode_A),
#'   composite("SRL_meta", single_item("메타인지4"), weights = mode_A),
#'   composite("SRL_meta", single_item("노력지속2"), weights = mode_A),
#'   composite("SRL_env", single_item("환경관리1"), weights = mode_A),
#'   composite("SRL_env", single_item("환경관리2"), weights = mode_A),
#'   composite("OnlineSAT", single_item("learn_joy"), weights = mode_A),
#'   composite("OnlineSAT", single_item("learn_convenience"), weights = mode_A),
#'   composite("OnlineSAT", single_item("learn_satisfy"), weights = mode_A),
#'   composite("OnlineSAT", single_item("Participation" ), weights = mode_A),
#'   composite("USEIntent", single_item("Intension_use"), weights = mode_A),
#'  composite("AppSatisfy", single_item("upgrade"), weights = mode_A),
#'   composite("AppSatisfy", single_item("satisfy"), weights = mode_A)
#' )
#' ## learn_model.r
#' plot(lmodel1)  #plotting model
#'
#' # browse_plot(lmodel1)  #plotting model measurement model
#'
#' ## structure model ---------------
#' lstr1 = relationships(
#'   paths(from=c("AppSatisfy"), to=c("SRL_meta", "SRL_env" ,"OnlineSAT")),
#'   paths(from=c("SRL_meta", "SRL_env"),to=c( "OnlineSAT")),
#'   paths(from=c("SRL_meta", "SRL_env"),to=c( "USEIntent")),
#'   paths(from=c("OnlineSAT"),to=c( "USEIntent")))
#' ## view browse strutrue model
#' # browse_plot(lstr1)
#'  plot(lstr1)
#'
#' ## model estimate ----------------
#' srlapp_pls  <- estimate_pls(data = stat_onl,
#'                             measurement_model = lmodel1,
#'                             structural_model = lstr1)
#' #result summary
#' summary(srlapp_pls)
#'
#' ## change var name and total CFA result
#' varName = c( "A_upgrade","A_satisfy",
#'              "S_Review","S_Add_learn","S_Feedback","S-Focus_on",
#'             "SE_Place","SE_Time",
#'              "On_Joy","On_Easy","On_Satisfy","On_Engage" ,
#'              "IntenstUse")
#'
#' pls_cfa(srlapp_pls, rename = T, var_name = varName)
#'
#' ## item validigy : Below is a function that extracts each index. Includes structural model analysis and observed variable analysis corresponding to confirmatory factor analysis.
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName,
#'                       res="loadings_rename") %>% arrange(variable)
#' ## CR, convergent
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "CR_AVE_sig")
#'
#' ## Internal consistency reliability plot----------
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "reliablility_plot")
#'
#' ## crossloadings
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "crossloadings")
#'
#' ## fl ----------
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "FL_criteria")
#'
#' ## HTMT
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName,  res = "HTMT")
#'
#' #VIF
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "vif")
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "vifr")
#'
#' ## paths
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "paths")
#'
#' ## effect size
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "f2")
#'
#' ## Mediation  effect ----------------------
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "effect")
#'
#' ## indircet effect
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "indirect_effect")
#'
#' # totatl effect
#' srlapp_pls %>% pls_cfa(rename = T, var_name = varName, res = "Total_effect")
#'
#' # plotting
#' plot(srlapp_pls)
#' }
#'
pls_cfa = function(pls_data,
                   res="all",
                   diagram=FALSE,
                   reliability_plot=FALSE,
                   digits=3,
                   boot=FALSE,
                   nboot=100,
                   rename=FALSE,
                   var_name=NULL,
                   vjust=-0.5
){

  library(seminr)
  library(reshape)
  library(tidyverse)

  #overall data
  preview = pls_data
  # generate summary data
  pls_summary <- summary(pls_data)


  #simplt bootstrap
  if(boot==TRUE){
    set.seed(2023)
    boot_data <- bootstrap_model(pls_data, nboot = nboot, cores = 4)
    pls_boot_summary <- summary(boot_data)
    # boot plot
    Plot <-  plot(boot_data)

    # semPaths = browse_plot(boot_data)

    if(diagram ==TRUE){
      semPaths = browse_plot(boot_data)
    }else{
      semPaths =NULL
    }
    #bootstrap data list
    loadings_boot0 <-   pls_boot_summary$bootstrapped_loadings[,c(1,4,5,6)] %>%
      as.data.frame() %>%
      mutate(commulality = (`Original Est.`)^2) %>%
      dplyr::select(1,5,2,3,4)%>%
      mutate(check= `2.5% CI`*`97.5% CI`) %>%
      mutate(sig=ifelse(check > 0,"*","ns")) %>%
      arrange(desc(subfactor))%>%
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))

    paths_boot <- pls_boot_summary$bootstrapped_total_paths[,c(1,4,5,6)] %>%
      as.data.frame() %>%
      mutate(check= `2.5% CI`*`97.5% CI`) %>%
      mutate(sig=ifelse(check > 0,"*","ns")) %>%
      dplyr::select(1,2,3,4,6)

    htmt_boot <-   pls_boot_summary$bootstrapped_HTMT[,c(1,4,5,6)] %>%
      as.data.frame() %>%
      dplyr::rename(htmt_est= `Original Est.`, t= `T Stat.`,
                    lowerCI=`2.5% CI`, upperCI=`97.5% CI`) %>%
      mutate(sig = ifelse(htmt_est <0.9, "*",
                          ifelse(htmt_est < 1, ".","ns")))

  }else if(boot ==FALSE){
    pls_data <- pls_data
    Plot = plot(pls_data)
    semPaths_boot = NULL
    plot_boot = NULL
    paths_boot = NULL
    loadings_boot0 = NULL
    htmt_boot = NULL

    # semPaths = browse_plot(pls_data)
    if(diagram ==TRUE){
      semPaths = browse_plot(pls_data)

    }else{
      semPaths =NULL
    }
  }



  #loadings
  loadings0 = pls_data$outer_loadings %>%
    as.data.frame() %>%
    mutate(var= rownames(pls_data$outer_loadings)) %>% # 변수맞추기
    melt() %>%
    dplyr::rename(subfactor=var, loadings = value) %>%
    mutate( communality = loadings^2) %>%
    dplyr::select(variable, subfactor, loadings, communality) %>%
    filter( loadings !=0) %>%
    dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                ifelse(loadings >0.5,"fair","")))


  varname = loadings0 %>% dplyr::select(subfactor) %>% as.vector()



  # crossloadings = pls_summary$validity$cross_loadings

  crossloadings0 = pls_data$outer_loadings %>% as.data.frame() %>%
    mutate(var= rownames(pls_data$outer_loadings)) %>% # 변수맞추기
    melt() %>%
    dplyr::rename(item=var, loadings = value, latent= variable) %>%
    filter( loadings !=0) %>%
    dplyr::select(latent, item ) %>%
    left_join( #데이터합치기
      cbind.data.frame(
        item=pls_data$mmVariable,
        pls_summary$validity$cross_loadings)
    )



  if(rename==TRUE){
    loadings <- cbind.data.frame(loadings0, subFactor = var_name)
    loadings <- loadings[,c(1,2,6,3,4,5)]
    loadings_new <- loadings[,c(1,3,4,5,6)]
    # dplyr::select(variable, subfactor,subFactor, loadings, communality)
    # loadings_boot0
    # loadings_boot <- cbind.data.frame(loadings_boot0, subFactor = var_name)
    # loadings_boot <- loadings[,c(1,2,6,3,4,5)]
    #
    #
    #loadinsg bar plot
    loading_bar <- loadings %>%
      ggplot(aes(x=subFactor, #
                 y=loadings))+
      geom_bar(stat = "Identity", aes(fill= variable),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1,
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))
    # scale_fill_grey(start=0.4, end=0.6)

    #cross loadings + var name
    crossloadings <- cbind.data.frame(
      Latent = crossloadings0[,1],
      Item = var_name,
      crossloadings0[,-1])

    loadings <- loadings %>% arrange(subFactor)
    loadings_new <- loadings %>% arrange(subFactor)

  }else if(rename==FALSE){
    loadings <- loadings0 # raw
    #
    # if(boot==TRUE){
    #   loadings_boot <- loadings_boot0  # raw
    # }

    #loadinsg bar plot
    loading_bar <- loadings0 %>%
      ggplot(aes(x=subfactor, y=loadings))+
      geom_bar(stat = "Identity", aes(fill= variable),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1,
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))
    # scale_fill_grey(start=0.4, end=0.6)


    #crossloadings petsistance
    crossloadings <- crossloadings0

    loadings <- loadings %>% arrange(subfactor)
    loadings_new <- loadings %>% arrange(subfactor)

  }



  # #loadinsg bar plot
  # loading_bar <- loadings %>%
  #   ggplot(aes(x=subfactor, y=loadings))+
  #   geom_bar(stat = "Identity", aes(fill= variable), show.legend = FALSE)+
  #   theme_bw()+
  #   geom_hline(yintercept = 0.7, linetype=1, color="black", linewidth= 0.8)+
  #   geom_hline(yintercept = 0.5, linetype="dashed",
  #              color="gray20", linewidth=0.8)+
  #   theme(axis.text.x = element_text(size=13, angle=90))+
  #   scale_fill_grey(start=0.4, end=0.6)
  #



  # This Option is  for viewing reliabiilty plots
  if(reliability_plot==TRUE){
    reliability_plot = pls_summary$reliability %>% plot()
  }else{
    reliability_plot = NULL
  }

  # reliabiltiy index
  reliability = pls_summary$reliability

  # reliability acceptance criteria
  reliability_sig <- pls_summary$reliability %>%
    as.data.frame() %>%
    mutate(
      a_sig = ifelse(alpha> 0.7,"*","ns"),
      CR_sig = ifelse(rhoC > 0.7,"*","ns"),
      rhoA_sig = ifelse(rhoA > 0.7,"*","ns"),
      AVE_sig = ifelse(AVE > 0.5,"*","ns")
    ) %>%
    dplyr::select(alpha, a_sig,
                  rhoC, CR_sig,
                  AVE, AVE_sig,
                  rhoA,rhoA_sig)

  #discriminant valididty first
  fl_matrix = pls_summary$validity$fl_criteria
  # Discriminat validity second
  htmt = pls_summary$validity$htmt






  #f2 effect size
  f2_effect = pls_summary$fSquare %>%
    as.data.frame() %>%
    mutate(exogenous = rownames(pls_summary$fSquare )) %>%
    melt() %>%
    dplyr::rename(endogenous = variable) %>%
    filter(value != 0) %>%
    mutate(EffectSize = ifelse(value > 0.35,"large(>0.35)",
                               ifelse(value > 0.15,"medium(>0.15)",
                                      ifelse(value > 0.02,"small(>0.02)","")))) %>%
    dplyr::rename(f2=value)

  #paths coerficient
  pathcoeff= pls_summary$paths




  # pls_summary
  # Generation of mediation effect data--
  #Calculate direct effect by calculating direct effect, total indirect effect, total effect-indirect effect
  # mediation_effect = cbind.data.frame(
  #   pls_summary$total_effects,
  #   direct_total= (
  #     pls_summary$total_effects[,ncol(pls_summary$total_effects)] - pls_summary$total_indirect_effects[,ncol(pls_summary$total_indirect_effects)]  ),
  #
  #   Indirect_total = pls_summary$total_indirect_effects[,ncol(pls_summary$total_indirect_effects)]
  # ) %>%
  #   round(digits)

  mediation_effect=cbind.data.frame(
    effec= c(
      rep("Direct",nrow(pls_data$path_coef)),
      rep("Indirect",nrow(pls_summary$total_indirect_effects)),
      rep("Total",nrow(pls_summary$total_indirect_effects))),
    variable=c(
      rownames(pls_data$path_coef),
      rownames(pls_summary$total_effects),
      rownames(pls_summary$total_indirect_effects)),
    rbind.data.frame(
      pls_data$path_coef,
      pls_summary$total_indirect_effects ,
      pls_summary$total_effects
    )) %>% tibble()



  #indirect effect
  Indirect_effect= pls_summary$total_indirect_effects
  Total_effect= pls_summary$total_effects
  Direct_effect= pls_summary$total_effects-pls_summary$total_indirect_effects
  # stucture correlation coeff
  str_cor = pls_summary$descriptives$correlations$constructs
  descriptives = pls_summary$descriptives
  #설명력
  r2= pls_data$rSquared

  # VIF=pls_summary$vif_antecedents

  VIF= as.data.frame(do.call(cbind,
                             pls_summary$vif_antecedents))
  VIFr= as.data.frame(do.call(rbind,
                              pls_summary$vif_antecedents))
  VIFc= as.data.frame(do.call(rbind,
                              pls_summary$vif_antecedents))


  # overall result
  results = list(
    Loadings = loadings,
    loadings_boot = loadings_boot0,
    # cR_AVE = reliability,
    reliability_plot = reliability_plot,
    CRAVE_sig = reliability_sig,
    FL_matrix = fl_matrix,
    HTMT = htmt,
    htmt_boot = htmt_boot,
    crossloadings=crossloadings,
    f2_effectsize = f2_effect,

    loading_bar = loading_bar,
    browse_Viewer = semPaths,
    total_effect = mediation_effect,
    # Indirect_effect=Indirect_effect,
    # r2 = r2,
    # str_cor= str_cor,
    paths = pathcoeff,
    paths_boot = paths_boot,
    plot = Plot,
    varname=varname
  )
  # results


  switch(res,
         preview=preview,
         all = results,
         loadings = loadings,
         loadings_rename= loadings_new,
         CR_AVE = reliability,
         CR_AVE_sig = reliability_sig,
         FL_criteria = fl_matrix,
         HTMT = htmt,
         crossloadings=crossloadings,
         f2 = f2_effect,
         reliability_plot = reliability_plot,
         loading_bar= loading_bar,
         browse_Viewer= semPaths,

         effect = mediation_effect,
         mediation = mediation_effect,
         indirect_effect=Indirect_effect,
         Indirect_effect=Indirect_effect,
         Total_effect = Total_effect,
         total_effect = Total_effect,
         Direct_effect = Direct_effect,
         direct_effect = Direct_effect,

         r2 = r2,
         str_cor= str_cor,
         paths = pathcoeff,
         loadings_boot = loadings_boot0,
         paths_boot = paths_boot,
         htmt_boot = htmt_boot,
         vif=VIF,
         vifr=VIFr,
         vifc=VIFc,
         summarise=descriptives

  )

}
