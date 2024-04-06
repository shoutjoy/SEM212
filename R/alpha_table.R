
#' Function to tabulate reliability
#'
#' @param alpha_data_res alpha result
#' @param title your title
#' @param show 'data', 'markdown'
#' @param format  markdown, html
#' @param digits  default 3
#' @param variable . output
#' @export
#'
#' @examples
#' \dontrun{
#'
#' stat_onl %>% select(S_Review, S_Add_learn, S_Feedback) %>%
#'    jjstat::alpha_table(show="data", variable = "A" )
#'
#'
#'bind_rows(
#'  stat_onl %>% select(S_Review, S_Add_learn, S_Feedback) %>%
#'    jjstat::alpha_table(show="data", variable = "A" ),
#'  stat_onl %>% select(SE_Place, SE_Time) %>%
#'    jjstat::alpha_table(show="data", variable = "B"),
#'  stat_onl %>% select(On_Satisfy, On_Joy, On_Easy, On_Engage) %>%
#'    jjstat::alpha_table(show="data", variable = "C"),
#'  stat_onl %>% select(upgrade,satisfy,fatigue_inverse) %>%
#'    jjstat::alpha_table(show="data", variable = "D")
#') |> markdown_table(digits=3, font_size = 12,
#'                    caption= "total Cronbach alpha")
#' }
#'
#'
#'
#'
alpha_table = function(alpha_data_res,
                       title="",
                       show="data",
                       format="markdown",
                       digits=3,
                       variable="."
){



  out <-  tryCatch(
    {  if(show =="alpha"){

      #If croncbah is calculated and entered
      alpha = paste0("alpha = ",
                     round(alpha_data_res$total[1],3),
                     ", 95%CI[",round(alpha_data_res$feldt[[1]],2),", ",
                     round(alpha_data_res$feldt[[3]],2), "]")

      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = alpha_data_res$keys[[1]],
        "cronbach alpha" = alpha_data_res$alpha.drop[,1]) %>%
        knitr::kable("markdown", digits,
              caption = paste0("Reliability for ", title, " (Cronbach's alpha)"))

      res


    }else if(show == "markdown"){
      #When calculating directly using data
      data_res <- alpha_data_res %>%psych::alpha(check.keys = TRUE)

      alpha = paste0("alpha = ",
                     round(data_res$total[1],3),
                     ", 95%CI[",round(data_res$feldt[[1]],2),", ",
                     round(data_res$feldt[[3]],2), "]")

      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = data_res$keys[[1]],
        "cronbach alpha"= data_res$alpha.drop[,1])%>%
        knitr::kable(format = format, digits,
              caption = paste0("Reliability for ", title, " (Cronbach's alpha)"))

      res

    }else if(show == "data"){
      #Required when combining multiple data to create another table
      data_res <- alpha_data_res %>%psych::alpha(check.keys = TRUE)

      alpha = paste0("alpha = ",
                     round(data_res$total[1],3),
                     ", 95%CI[",round(data_res$feldt[[1]],2),", ",
                     round(data_res$feldt[[3]],2), "]")

      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = data_res$keys[[1]],
        "cronbach alpha" = data_res$alpha.drop[,1])
      res
    }
    },error = function(e){
      stop("Check the option settings again. Enter alpha or data in show.
If alpha is the value, enter show = 'alpha',
When creating a table by analyzing only data, set show = 'data'
    ")
    }

  )
  return(out)
}



