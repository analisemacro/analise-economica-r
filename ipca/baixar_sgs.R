# Baixa uma série do SGS/BCB e devolve um data.frame com as colunas data e valor
library(jsonlite)

baixar_sgs <- function(codigo, inicio, fim = Sys.Date()) {
  url <- paste0(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.", codigo, "/dados",
    "?formato=json&dataInicial=", format(as.Date(inicio), "%d/%m/%Y"),
    "&dataFinal=", format(as.Date(fim), "%d/%m/%Y")
  )

  serie <- fromJSON(url)
  data.frame(
    data  = as.Date(serie$data, format = "%d/%m/%Y"),
    valor = as.numeric(serie$valor)
  )
}
