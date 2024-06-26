% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patternGraph_obs_exp_ko.R
\name{patternGraph2}
\alias{patternGraph2}
\title{patternGraph2}
\usage{
patternGraph2(
  data,
  type = "g",
  raw = TRUE,
  Ncol = NULL,
  yadd = 0.45,
  strip_size = 16,
  axis_size = 16,
  text_size = 14,
  size_bartext = 5
)
}
\arguments{
\item{data}{data,frame}

\item{type}{g}

\item{raw}{raw data}

\item{Ncol}{grid}

\item{yadd}{up yadd}

\item{strip_size}{strip}

\item{axis_size}{axis}

\item{text_size}{text}

\item{size_bartext}{bartext}
}
\value{
result fill data
}
\description{
patternGraph2
}
\examples{
\dontrun{
aaa=kge_bind2 \%>\% filter(지역 =="부산" & N ==1) \%>\%
  auto_pattern("고유어1") \%>\%
  kge_chisq_table("a1","성조", "고유어1 부산")

#sAVE
data_long<- aaa$chi_table \%>\%
  rownames_to_column("syllabic") \%>\%
  pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
data_long

data_long\%>\% ggplot(aes(x = accent, y = ratio))+
  geom_bar(stat = "identity", aes( fill = accent),
           position = "dodge", show.legend = FALSE)+
  geom_hline(yintercept = 1, linetype=2, color="gray80")+
  geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
  ylim(0,max(data1$ratio)+0.07)+
  coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        axis.title = element_text(size= 12),
        strip.text = element_text(size= 14)
  )+
  scale_fill_grey(start = 0, end = 0.7) +
  facet_wrap(~ syllabic )

aaa\%>\% patternGraph_obs_exp()

}
}
