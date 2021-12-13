#install.packages("DiagrammeR")
library(DiagrammeR)
library(htmltools)

Gantt<-DiagrammeR::mermaid("
gantt
dateFormat DD-MM-YYYY
title Diagrama de Gantt

section D1
Portada                 :done,   first_1,    25-09-2021, 24h
Definición de Proyecto  :done,   first_2,    25-09-2021, 24h
Diagrama de Gantt       :done,   first_3,    after first_2, 28-09-2021
Repartición Tareas      :done,   first_4,    after first_2, 28-09-2021
Plan de riesgos         :done,   first_5,    after first_2, 28-09-2021

section D2
Estructura de datos (Descirptiva)   :crit,done,    import_1,   after first_5,10-10-2021
Preprocessing                       :crit,done,    import_2,   after first_5,30-11-2021
Descriptiva Preprocessing           :crit,done,    import_3,   after import_2, 02-12-2021

section D3
Proceso Mineria de datos          :        extras_1,   15-11-2021, 08-12-2021
Análisis Comparativo              :        extras_2,   after extras_1, 13-12-2021
Conclusiones                      :        extras_3,   after extras_1, 16-12-2021
Revisión Trabajo                  :        extras_4,   after extras_1, 16-12-2021 
")


Gantt$x$config = list(ganttConfig = list(
  axisFormatter = list(list("%b %d, %Y",htmlwidgets::JS(
    'function(d){ return d.getDay() == 1 }'
  )
  ))
))

html_print(tagList(
  tags$h1("Default Behavior")
  ,Gantt
))
