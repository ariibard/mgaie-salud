# Adaptacion base GCBA


if ("readxl" %in% installed.packages()[, "Package"]) {
  library(readxl)
} else {
  install.packages("readxl")
  library(readxl)
}
if ("openxlsx" %in% installed.packages()[, "Package"]) {
  library(openxlsx)
} else {
  install.packages("openxlsx")
  library(openxlsx)
}
if ("highcharter" %in% installed.packages()[, "Package"]) {
  library(highcharter)
} else {
  install.packages("highcharter")
  library(highcharter)
}






# descarga del archivo
url = "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2021/05/S_147.xlsx"

download.file(url, destfile = "data/cobertura_salud.xls", mode="wb")

sheets = readxl::excel_sheets("data/cobertura_salud.xls")
print(sheets)

# Eliminar el nombre de la primera hoja de la planilla en la variable (ya que no contiene información)
sheets = sheets[sheets!="GraphData"]
print(sheets)