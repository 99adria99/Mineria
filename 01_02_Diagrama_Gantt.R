#install.packages("DiagrammeR")
library(DiagrammeR)
library(htmltools)

Gantt<-DiagrammeR::mermaid("
gantt
dateFormat DD-MM-YYYY
title Diagrama de Gantt

section D1
Portada                 :done,   first_1,    25-09-2021, 24h
Definición de Proyecto  :done,   first_2,    after first_1, 28-09-2021

section D2
Diagrama de Gantt      :crit,done,    import_1,   after first_2,30-09-2021
Repartición Tareas     :crit,done,    import_2,   after import_1,02-10-2021
Plan de riesgos        :crit,done,    import_3,   after import_2, 05-10-2021

section D3
Estructura de datos(descriptiva) :        extras_1,   after import_3, 20-10-2021
Proceso Mineria de datos         :        extras_2,   after extras_1, 17-11-2021
Análisis Comparativo             :        extras_3,   after extras_2, 29-11-2021
Conclusiones                     :        extras_4,   after extras_3, 08-12-2021
Revisión Trabajo                 :        extras_5,   after extras_4, 16-12-2021 
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


