#' @title Generowanie tabeli z danymi z rozdziału D w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane i tworzy z nich sformatowaną tabelę
#'   `flextable` do wstawienia do raportu Word. Stosuje styl booktabs, dopasowuje
#'   szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez funkcje `dane_tab_D...`.
#' @param edycja Liczba całkowita reprezentująca rok edycji monitoringu.
#' @return Obiekt klasy `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs colformat_num
#' @importFrom flextable set_table_properties align add_footer_lines
#' @importFrom dplyr %>%
#' @export
gentab_tab_D <- function(dane_do_tabeli, edycja) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli)[1] %in% "Uwaga") {
    message("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób.")
  }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%
    theme_booktabs() %>%
    colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(j = -1, align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", edycja, " r.\n\n"), top = FALSE) #%>%

  return(tabela_flextable)
}
