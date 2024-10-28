library(tibble)


# Tabelas -----------------------------------------------------------------

tabela_bruta <- mtcars |> 
  tibble()


# addorn_totals -----------------------------------------------------------

# Serve para incluir um total que é uma soma de valores ou contagem

tabela_bruta |> 
  dplyr::count(cyl) |> 
  janitor::adorn_totals(where = "row", name = "Todas as cilindradas")


tabela_bruta |> 
  dplyr::count(gear,cyl) |> 
  tidyr::pivot_wider(names_from =  cyl, values_from = n) |> 
  janitor::adorn_totals(
    where = c("row", "col"),
    name = "Total",
    "-",
    TRUE)




# add_row -----------------------------------------------------------------

tabela_bruta |> 
  dplyr::add_row(
    mpg = 10
  ) |> View()

tabela_bruta |> 
  dplyr::add_row(
    mpg = c(9,10)
  ) |> View()



# Complete ----------------------------------------------------------------

tidyr::complete()

tabela_bruta |> 
  dplyr::count(cyl,carb) |> 
  tidyr::complete(
    cyl, carb, fill = list(n =0)
  )
  

tabela_bruta |> 
  dplyr::mutate(carb = factor(carb, 1:10)) |> 
  dplyr::count(carb) |> 
  tidyr::complete(carb, fill = list(n=0))


# fill --------------------------------------------------------------------


dados::clima |> 
  tidyr::fill(velocidade_rajada, .direction = "downup")



# accross -----------------------------------------------------------------

tabela_bruta |> 
  dplyr::summarise(
    mpg = mean(mpg),
    wt = mean(wt),
    drat = mean(drat)
  )

tabela_bruta |> 
  dplyr::summarise(
    dplyr::across(
      c(mpg, wt, drat),
      mean
    )

  )


tabela_bruta |> 
  dplyr::summarise(
    dplyr::across(
      c(mpg, wt, drat),
      .fns = list(media = mean, desv_pad =sd)
      
    )
    
  )


  










