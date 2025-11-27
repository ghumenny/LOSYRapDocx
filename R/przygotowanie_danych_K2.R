#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami  `plec`, `kontynuacja` oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresKontynuacjeDziedzinyPlec z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_wyk_K2dzi_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dziedziny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)


  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  dane_wejsciowe <- dane_wejsciowe |> filter(liczba >= 10)
  if (is.null(dane_wejsciowe) ) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Mniej niż 10 absolwentó ogółem."
    ))
  }

  dane_wyjsciowe <- dane_wejsciowe  %>%
    select(-liczba, -procent) %>%
    pivot_longer(
      cols = matches("^(liczba|procent)_"),
      names_to = c(".value", "dziedzina"),
      names_pattern = "^(liczba|procent)_(.*)$"
    ) |> group_by(dziedzina) %>%
    # filter(
    #   any(sexf == "OGÓŁEM" & liczba >= 10, na.rm = TRUE)
    # ) %>%
    ungroup() %>%
    select(sexf, dziedzina, pct = procent) |>
    mutate(
      pct = pct / 100,
      dziedzina = factor(dziedzina, levels = c(
        "nauk teologicznych",
        "nauk weterynaryjnych",
        "sztuki",
        "nauk rolniczych",
        "nauk ścisłych i przyrodniczych",
        "nauk humanistycznych",
        "nauk inżynieryjno-technicznych",
        "nauk medycznych i nauk o zdrowiu",
        "nauk społecznych"
      )),
      plec = if_else(sexf == "Mężczyzna", "Mężczyźni",
                     if_else(sexf == "Kobieta", "Kobiety", "Ogółem")),
      plec = factor(plec, levels = c(
        "Ogółem",
        "Mężczyźni",
        "Kobiety"
      ))) %>%
    select(plec, dziedzina, pct)

  if (nrow(dane_wyjsciowe)  == 0) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_tab_K2dzi_plec <- function(pelna_finalna_ramka_wskaznikow,
                                typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dziedziny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  dane_wejsciowe <- dane_wejsciowe |> filter(liczba >= 10)
  if (is.null(dane_wejsciowe) ) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Mniej niż 10 absolwentó ogółem."
    ))
  }

  dane_wyjsciowe <- dane_wejsciowe  %>%
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                   if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","Dziedzina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      Dziedzina = if_else(!is.na(Dziedzina), Dziedzina, "ogółem")) |>
    arrange(desc(Ogółem_liczba))

  if (nrow(dane_wyjsciowe)  == 0) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów
#' do filtrowania.
#' @param tylko_tabele parametr TRUE/FALSE przekazywany z głównej funkcji
#'   generującej raport - jeśli FALSE to przycina tabele z zawodami do 10
#'   najliczniejszych zawodów, jeśli TRUE to raport generuje tabele zawodów bez
#'   przycinania
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_tab_K2dzi_zaw <- function(pelna_finalna_ramka_wskaznikow,
                                typ_szk, rok_absolwentow, tylko_tabele) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "nazwa_zaw",
      .data$parametr_K2 == "dziedziny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  if(tylko_tabele == FALSE) {
    dane_wyjsciowe <- dane_wejsciowe  %>%
      arrange(desc(liczba)) %>%
      slice(2:11)  %>%
      filter(liczba > 10) %>%
      select(nazwa_zaw, liczba, starts_with("procent_"))%>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw) %>%
      rename_with(~ str_replace(., "^procent_", "Procent w dziedzinie_"), matches("^procent_"))
  } else {
    dane_wyjsciowe <- dane_wejsciowe  %>%
      arrange(desc(liczba)) %>%
      filter(liczba > 10) %>%
      select(nazwa_zaw, liczba, starts_with("procent_"))%>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw) %>%
      rename_with(~ str_replace(., "^procent_", "Procent w dziedzinie_"), matches("^procent_"))
  }

  if (nrow(dane_wyjsciowe)  == 0) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_tab_K2dys_plec <- function(pelna_finalna_ramka_wskaznikow,
                                typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dyscypliny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  dane_wejsciowe <- dane_wejsciowe |> filter(liczba >= 10)
  if (is.null(dane_wejsciowe) ) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Mniej niż 10 absolwentó ogółem."
    ))
  }

  dane_wyjsciowe <- dane_wejsciowe  %>%
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                          if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","Dyscyplina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      Dyscyplina = if_else(!is.na(Dyscyplina), Dyscyplina, "ogółem")) %>%
    arrange(desc(Ogółem_liczba)) %>%
    slice(2:11)

  if (nrow(dane_wyjsciowe)  == 0) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do tabeli dyscyplin kontynuacji nauki (w zawodach)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych w formacie
#'   będącym wejściem do funkcji gentab_K2dys_zaw z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate arrange desc slice rename
#' @importFrom dplyr across where
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom tibble tibble as_tibble
#' @export
dane_tab_K2dys_zaw <- function(pelna_finalna_ramka_wskaznikow,
                               typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "nazwa_zaw",
      .data$parametr_K2 == "dyscypliny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1) %>%
    arrange(desc(liczba))

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  dane_wejsciowe_posortowane <- dane_wejsciowe %>%
    as_tibble() %>%
    arrange(desc(liczba))

  if (nrow(dane_wejsciowe_posortowane) < 2) {
    message("Brak danych o zawodach (jest tylko 'Ogółem' lub brak danych). Zwracam pustą ramkę.")
    return(tibble(
      Uwaga = "Brak danych o zawodach do przetworzenia."
    ))
  }
  liczba_wierszy_spelniajacych_warunek <- sum(dane_wejsciowe_posortowane$liczba >= 10, na.rm = TRUE)

  max_n <- min(nrow(dane_wejsciowe_posortowane),
               liczba_wierszy_spelniajacych_warunek,
               6)

  if (max_n < 2) {
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  lista_wynikow <- list()

  for (n in 2:max_n) {

    nazwa_zawodu_n <- dane_wejsciowe_posortowane[["nazwa_zaw"]][n]
    liczba_zawodu_n <- dane_wejsciowe_posortowane[["liczba"]][n]

    separator_name <- paste0(nazwa_zawodu_n, ' (liczba absolwentów ', format(liczba_zawodu_n, big.mark = " "), ')_Dyscyplina')
    final_col_name <- paste0(nazwa_zawodu_n, ' (liczba absolwentów ', format(liczba_zawodu_n, big.mark = " "), ')_procent')

    temp <- dane_wejsciowe_posortowane %>%
      slice(n) %>%
      rename(liczba_Ogółem = liczba,
             procent_Ogółem = procent) %>%
      tidyr::pivot_longer(liczba_Ogółem : ncol(.), # `ncol(.)` odnosi się do 1-wierszowej ramki
                          names_to = "typ2",
                          values_to = "value") %>%
      tidyr::separate(typ2, c('forma', separator_name), sep = '_') %>%
      tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
                         values_from = c("value"),
                         names_sep = "_",
                         names_repair = "unique") %>%
      arrange(desc(.[[2]])) %>% # Sortuj wg liczby (kolumna 2)
      slice(2:6) %>%           # Weź top 5 dyscyplin
      select(1, 3)             # Wybierz dyscyplinę (kol 1) i procent (kol 3)

    colnames(temp)[2] <- final_col_name

    lista_wynikow[[n - 1]] <- temp # Dodajemy ramkę do listy
  }

  dane_wyjsciowe <- dplyr::bind_cols(lista_wynikow) %>%
    mutate(
      across(where(is.numeric), ~  round(., digits = 2))
    )


  if (nrow(dane_wyjsciowe)  == 0) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak zawodów o minimum 10 absolwentach"
    ))
  }

  # dane_wyjsciowe <-   dane_wejsciowe %>% #
  #   as_tibble() %>%
  #   slice(2) %>%
  #   rename(liczba_Ogółem = liczba,
  #          procent_Ogółem = procent) %>%
  #   tidyr::pivot_longer(liczba_Ogółem :ncol(dane_wejsciowe),
  #                       names_to = "typ2",
  #                       values_to = "value") %>%
  #   tidyr::separate(typ2, c('forma', paste0(dane_wejsciowe[["nazwa_zaw"]][2],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][2],big.mark = " "), ')_Dyscyplina')), sep = '_') %>%
  #   tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
  #                      values_from = c("value"),
  #                      names_sep = "_",
  #                      names_repair = "unique") %>%
  #   arrange(desc(.[[2]])) %>%
  #   slice(2:6) %>%
  #   select(1,3)
  # colnames(dane_wyjsciowe)[2] <- paste0(dane_wejsciowe[["nazwa_zaw"]][2],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][2],big.mark = " "), ')_procent')
  #
  # for (n in 3:6) {
  #   temp <- dane_wejsciowe %>%
  #     as_tibble() %>%
  #     slice(n) %>%
  #     rename(liczba_Ogółem = liczba,
  #            procent_Ogółem = procent) %>%
  #     tidyr::pivot_longer(liczba_Ogółem :ncol(dane_wejsciowe),
  #                         names_to = "typ2",
  #                         values_to = "value") %>%
  #     tidyr::separate(typ2, c('forma', paste0(dane_wejsciowe[["nazwa_zaw"]][n],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][n],big.mark = " "), ')_Dyscyplina')), sep = '_') %>%
  #     tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
  #                        values_from = c("value"),
  #                        names_sep = "_",
  #                        names_repair = "unique") %>%
  #     arrange(desc(.[[2]])) %>%
  #     slice(2:6) %>%
  #     select(1,3)
  #   colnames(temp)[2] <- paste0(dane_wejsciowe[["nazwa_zaw"]][n],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][n],big.mark = " "), ')_procent')
  #
  #
  #
  #   dane_wyjsciowe = cbind(
  #     dane_wyjsciowe,
  #     temp) %>%
  #     mutate(
  #       across(where(is.numeric), ~  round(.,digits = 2)))
  # }
  #


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else arrange
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_wyk_K2dys_plec <- function(pelna_finalna_ramka_wskaznikow,
                                typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dyscypliny",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  dane_wejsciowe <- dane_wejsciowe |> filter(liczba >= 10)
  if (is.null(dane_wejsciowe) ) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Mniej niż 10 absolwentó ogółem."
    ))
  }

  poprawna_kolejnosc <- dane_wejsciowe %>%
    filter(sexf == "OGÓŁEM") %>%
    select(starts_with("liczba_")) %>%
    pivot_longer(cols = starts_with("liczba_"),
      names_to = c(".value", "dyscyplina"),
      names_sep = "_") |>
    arrange(liczba) %>%
    pull(dyscyplina)

  # dane_wyjsciowe <- dane_wejsciowe  %>%
  #   select(-liczba, -procent) %>%
  #   pivot_longer(
  #     cols = matches("^(liczba|procent)_"),
  #     names_to = c(".value", "dyscyplina"),
  #     names_pattern = "^(liczba|procent)_(.*)$"
  #   ) |> group_by(dyscyplina) %>%
  #   # filter(
  #   #   any(sexf == "OGÓŁEM" & liczba >= 10, na.rm = TRUE)
  #   # ) %>%
  #   ungroup() %>%
  #   select(sexf, dyscyplina, pct = procent) |>
  #   mutate(
  #     dyscyplina = factor(dyscyplina, levels = poprawna_kolejnosc),
  #     pct = pct / 100,
  #     plec = if_else(sexf == "Mężczyzna", "Mężczyźni",
  #                    if_else(sexf == "Kobieta", "Kobiety", "Ogółem")),
  #     plec = factor(plec, levels = c(
  #       "Ogółem",
  #       "Mężczyźni",
  #       "Kobiety"
  #     ))) %>%
  #   select(plec, dyscyplina, pct) |>
  #   slice(1:30)




  dane_wyjsciowe <- dane_wejsciowe  %>%
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                          if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","dyscyplina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    arrange(desc(Ogółem_liczba)) %>%
    select(dyscyplina, ends_with("_procent")) %>%
    slice(2:11)  %>%
    mutate(
      dyscyplina = factor(dyscyplina, levels = poprawna_kolejnosc)
    ) %>%
    pivot_longer(2:4,
                 names_to = c("plec"),
                 values_to = "pct") %>%
    mutate(
      plec = if_else(plec == "Kobiety_procent", "Kobiety",
                     if_else(plec == "Mężczyźni_procent", "Mężczyźni", "Ogółem")),
      plec = factor(.data$plec, levels = c(
        "Ogółem",
        "Mężczyźni",
        "Kobiety"
      )),
      pct = pct/100)


  return(dane_wyjsciowe)
}