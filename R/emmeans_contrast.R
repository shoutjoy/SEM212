#' emmeans_contrast is Two Way ANOVA and Post Hoc
#' @param aov_data aov data , example aov(len ~ supp*dose, data = ToothGrowth)
#' @param var contaast variable
#' @param ... contrast add
#' @param auto contrast auto
#' @param type overall result 'all','anova','emmeans','contrast', 'APA','confint','interaction'is plot
#' @param hjust text position horizontal
#' @param vjust text positin vertical
#'
#'
#' @examples
#'
#' \dontrun{
#' ## 2*3 ANOVA
#' aov(len ~ supp*dose, data = ToothGrowth) %>%
#'   emmeans_contrast()
#'
#' ToothGrowth %>% str()
#' ToothGrowth$dose<- as.factor(ToothGrowth$dose)
#' aov(len ~ factor(dose)*supp, data = ToothGrowth) %>% tidy()
#'
#' aov(len ~ dose*supp, data = ToothGrowth) %>%
#'   emmeans(specs = c("dose", "supp")) %>% data.frame()
#'
#'
#' aov(len ~ dose*supp, data = ToothGrowth) %>%
#'   emmeans_contrast(type = "emmeans")
#
#' aov(len ~ supp*dose, data = ToothGrowth) %>%
#'   emmeans_contrast()
#'
#' aov(AveragePleasantness ~ Gender*Condition, data = epl) %>%
#'   emmeans_contrast()
#'
#' aov(AveragePleasantness ~ Gender*Condition, data = epl) %>%
#'   emmeans_contrast(type="emmeans")
#'
#' aov(AveragePleasantness ~ Gender*Condition, data = epl) %>%
#'   emmeans_contrast(type="contrast")
#'
#' aov(AveragePleasantness ~ Gender*Condition, data = epl) %>%
#'   emmeans_contrast("Gender","Condition", auto = F)
#'
# #바꾼 경우
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>%
#   emmeans_contrast("Condition", "Gender", auto = F)
#'
#' }
#'
#'
#' @export

emmeans_contrast = function(aov_data, var="", ...,
                            auto= TRUE,
                            type="all",
                            hjust=0.1,
                            vjust=-2){
  library(emmeans)

  # Select and analyze automatic variables
  if(auto == TRUE){
    # result summary
    summary <- aov_data %>% rstatix::Anova(type="III")
    apa_report <- aov_data %>% report::report()
    # aov_formula <- aov_data %>% formula()
    aov_formula0 =  aov_data%>%formula()
    aov_formula = aov_formula0%>% as.character() %>%
      str_split("~") %>% unlist()
    DV_name = aov_formula[3]

    n_data <- aov_data %>% broom::tidy()
    var1 = n_data[1,1] %>% unlist()
    var2 = n_data[2,1]%>% unlist()

    EM = aov_data %>%
      emmeans::emmeans(specs = c(var1, var2)) %>%
      data.frame()

    EM <- EM%>%
      mutate(mu =c(paste0("m", 1:(nrow(EM)) )) ) %>%
      dplyr::select(8,1:7) %>% tibble()


    if(nrow(EM)==4){ #2*2 anova
      Contrast = aov_data %>%
        emmeans::emmeans(specs = c(var1, var2)) %>%
        contrast(list(
          "m1-m2: sme of Var1(1st)" = c(1,-1,0,0),
          "m3-m4: sme of Var1(2nd)" = c(0,0,1,-1),
          "m1-m3: sme of Var2(1st)" = c(1,0,-1,0),
          "m2-m4: sme of Var2(2nd)" = c(0,1,-1,0),
          "m1-m2-m3+m4:Interaction"=c(1,-1,-1,1)
        ))

    }else if(nrow(EM)==6){  # 2*3 anova
      Contrast = aov_data %>%
        emmeans::emmeans(specs = c(var1, var2)) %>%
        emmeans::contrast(list(
          "m1-m2-m3+m4: Interaction 1" = c(1,-1,-1,1,0,0),
          "m1-m2-m5+m6: Interaction 2" = c(1,-1,0,0,-1,1),
          "m3-m4-m5+m6: Interaction 3" = c(0,0, 1,-1,-1,1),
          "m1-m2: simple main effect 1" = c(1,-1,0,0,0,0),
          "m3-m4: simple main effect 2" = c(0,0, 1,-1,0,0),
          "m5-m6: simple main effect 3" = c(0,0,0,0,1,-1)
        ))
    }

    #significant
    ndata <- Contrast %>%
      as.data.frame() %>%
      mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "ns"))))
    # change variable as CHARACTER
    ndata$sig <- as.character(ndata$sig)
    ndata <- ndata %>% tibble()
    ndata_plot <- Contrast %>% plot()+
      geom_vline(xintercept = 0, linetype=2, col="Red")+
      labs(x ="Confidence Range",
           title = paste0("Planned Contrast DV: ", DV_name)
      )

    variable1 <- as.vector(var1)
    variable2 <- as.vector(var2)
    #
    EM0 <- EM
    colnames(EM0) <-c("mu","v1","v2","emmean",
                      "SE","df","lower.CL","upper.CL")

    #interaction plot
    library(ggrepel)
    interaction.plot <- EM0 %>%
      ggplot(aes(x = v2, y = emmean))+
      geom_point(size=3)+
      geom_point(aes(shape= v1,color=v1), #,
                 show.legend = T, size= 4)+
      geom_line(aes(group = v1, linetype= v1),
                linewidth=0.8, show.legend = F)+
      ggrepel::geom_text_repel(aes(label= round(emmean,2)),
                      vjust = vjust, hjust = hjust, size=5)+
      labs(x = variable2,
           color = variable1,
           shape = variable1,
           y = DV_name,
           # subtitle = aov_formula0
      )+
      theme_bw()



    #result
    res=list(
      ANOVA = summary,
      Estimate_marginal_mean = EM,
      Planned_contrast = ndata,
      APA = apa_report,
      confint_plot = ndata_plot,
      interaction_plot = interaction.plot
    )
    # Change variables manually
  }else if(auto == FALSE){
    #Result report type="III"
    summary <- aov_data %>% rstatix::Anova(type="III")
    apa_report <- aov_data %>% report::report()
    specs0 = c(var, ...)

    aov_formula0 =  aov_data%>%formula()
    aov_formula = aov_formula0%>% as.character() %>%
      str_split("~") %>% unlist()
    DV_name = aov_formula[3]


    EM = aov_data %>%
      emmeans::emmeans(specs = specs0) %>%
      data.frame()

    EM <- EM%>%
      dplyr::mutate(mu =c(paste0("m", 1:(nrow(EM)) ))) %>%
      dplyr::select(8,1:7) %>% tibble()




    if(nrow(EM)==4){
      Contrast = aov_data %>%
        emmeans(specs = specs0) %>%
        contrast(list(
          "m1-m2: sme of Var1(1st)" = c(1,-1,0,0),
          "m3-m4: sme of Var1(2nd)" = c(0,0,1,-1),
          "m1-m3: sme of Var2(1st)" = c(1,0,-1,0),
          "m2-m4: sme of Var2(2nd)" = c(0,1,-1,0),
          "m1-m2-m3+m4:Interaction"=c(1,-1,-1,1)
        ))
    }else if(nrow(EM)==6){
      Contrast = aov_data %>%
        emmeans(specs = specs0) %>%
        contrast(list(
          "m1-m2-m3+m4: Interaction 1" = c(1,-1,-1,1,0,0),
          "m1-m2-m5+m6: Interaction 2" = c(1,-1,0,0,-1,1),
          "m3-m4-m5+m6: Interaction 3" = c(0,0, 1,-1,-1,1),
          "m1-m2: simple main effect 1" = c(1,-1,0,0,0,0),
          "m3-m4: simple main effect 2" = c(0,0, 1,-1,0,0),
          "m5-m6: simple main effect 3" = c(0,0,0,0,1,-1)
        ))
    }
    ndata <- Contrast %>%
      as.data.frame() %>%
      mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "ns"))))
    #Change variable to CHARACTER
    ndata$sig <- as.character(ndata$sig)
    ndata <- ndata %>% tibble()
    ndata_plot <- Contrast %>% plot()+
      geom_vline(xintercept = 0, linetype=2, col="Red")+
      labs(x="Confidence Range",
           title = paste0("Planned Contrast DV: ", DV_name)
      )


    #interaction
    variable1 <- as.vector(specs0[1])
    variable2 <- as.vector(specs0[2])
    EM0 <- EM
    colnames(EM0) <-c("mu","v1","v2","emmean",
                      "SE","df","lower.CL","upper.CL")

    #interaction plot
    library(ggrepel)
    interaction.plot <- EM0 %>%
      ggplot(aes(x = v2, y = emmean))+
      geom_point(size=3)+
      geom_point(aes(shape= v1,color=v1), #,
                 show.legend = T, size= 4)+
      geom_line(aes(group = v1, linetype= v1),
                linewidth=0.8, show.legend = F)+
      geom_text_repel(aes(label= round(emmean,2)),
                      vjust = vjust, hjust = hjust, size=5)+
      labs(x = variable2,
           color = variable1,
           shape = variable1,
           y = DV_name,
           # subtitle = aov_formula0
      )+
      theme_bw()


    #result

    res=list(
      ANOVA =summary ,
      Estimate_marginal_mean = EM,
      Planned_contrast = ndata,
      APA = apa_report,
      confint_plot = ndata_plot,
      interaction_plot = interaction.plot
    )

  }
  #   dev.off()
  switch(type,
         all = res,
         anova = summary,
         emmeans = EM,
         contrast = ndata,
         APA = apa_report,
         confint = ndata_plot,
         interaction = interaction.plot
  )
}

