#' @title Generowanie tabeli typów szkół w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   `przygotuj_dane_meta_typ_szkoly`. Oczekuje kolumn `Typ szkoły`, `liczba`, `procent`.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs set_table_properties
#' @importFrom flextable align_nottext_col add_footer_lines
#' @importFrom rlang .data
#' @export
gentab_typsz_flext_og <- function(dane_do_tabeli) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || nrow(dane_do_tabeli) == 0) {
    message("Brak danych do wygenerowania tabeli. Zwracam pusty obiekt flextable.")
    return(flextable())
  }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = c("liczba", "procent"), align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) %>%
    # Możesz dodać dalsze formatowanie, np. nazwy kolumn
    set_header_labels(
      `Typ szkoły` = "Typy szkół",
      liczba = paste0("Liczba\n", glowna_edycja - 2), # Przykładowo, jeśli rok pochodzi z glowna_edycja - 2
      procent = paste0("Procent\n", glowna_edycja - 2)
    )

  return(tabela_flextable)
}

#' @title Generowanie tabeli typów szkół w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   `przygotuj_dane_meta_typ_szkoly`. Oczekuje kolumn `Typ szkoły`, `liczba`, `procent`.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs set_table_properties
#' @importFrom flextable align_nottext_col add_footer_lines
#' @importFrom rlang .data
#' @export
gentab_typsz_flext_plec <- function(dane_do_tabeli) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || nrow(dane_do_tabeli) == 0) {
    message("Brak danych do wygenerowania tabeli. Zwracam pusty obiekt flextable.")
    return(flextable())
  }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = 2:5, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) #%>%
    # # Możesz dodać dalsze formatowanie, np. nazwy kolumn
    # set_header_labels(
    #   `Typ szkoły` = "Typy szkół",
    #   liczba = paste0("Liczba\n", glowna_edycja - 2), # Przykładowo, jeśli rok pochodzi z glowna_edycja - 2
    #   procent = paste0("Procent\n", glowna_edycja - 2)
    # )

  return(tabela_flextable)
}


#' @title Generowanie tabeli typów szkół w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   `przygotuj_dane_meta_typ_szkoly`. Oczekuje kolumn `Typ szkoły`, `liczba`, `procent`.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs set_table_properties
#' @importFrom flextable align_nottext_col add_footer_lines
#' @importFrom rlang .data
#' @export
gentab_typsz_flext_zaw <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "meta_zaw",
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)
  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_wejsciowe) || nrow(dane_wejsciowe) == 0) {
    message("Brak danych do wygenerowania tabeli. Zwracam pusty obiekt flextable.")
    return(flextable())
  }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_wejsciowe %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = 2:ncol(dane_wejsciowe), align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE)

  return(tabela_flextable)
}
