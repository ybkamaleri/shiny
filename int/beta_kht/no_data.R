no_data <- function(){
  data=data.frame(x=0,y=0)
  q <- ggplot(data=data)
  q <- q + theme_void()
  q <- q + annotate("text", label=glue::glue("Ikke noe data {fhi::nb$aa} vise\np{fhi::nb$aa} dette geografiske niv{fhi::nb$aa}et"), x=0, y=0, size=10)
  q
}

loading <- function(){
  data=data.frame(x=0,y=0)
  q <- ggplot(data=data)
  q <- q + theme_void()
  q <- q + annotate("text", label=glue::glue("Please wait. Loading data."), x=0, y=0, size=10)
  q
}
