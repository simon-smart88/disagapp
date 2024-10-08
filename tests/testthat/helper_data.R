is_ci <- Sys.getenv("GITHUB_ACTIONS") == "true"

covdf <- data.frame(datapath = list.files(system.file("extdata", "covariates", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "covariates", package = "disagapp")))

shpdf <- data.frame(datapath = list.files(system.file("extdata", "shapes", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "shapes", package = "disagapp")))

aggdf <- data.frame(datapath = list.files(system.file("extdata", "aggregation", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "aggregation", package = "disagapp")))


df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

df_path <- system.file("extdata", "test_data", "lie.csv", package = "disagapp")

ch_df <- data.frame("area" =  c('Aargau', 'Appenzell Ausserrhoden', 'Appenzell Innerrhoden',
           'Basel-Landschaft', 'Basel-Stadt', 'Bern', 'Fribourg', 'Genève',
           'Glarus', 'Graubünden', 'Jura', 'Luzern', 'Neuchâtel', 'Nidwalden',
           'Obwalden', 'Schaffhausen', 'Schwyz', 'Solothurn', 'St. Gallen',
           'Thurgau', 'Ticino', 'Uri', 'Valais', 'Vaud', 'Zug', 'Zürich'),
           "response" = 1:26)

mdf <- rbind(df, ch_df)

shape <- sf::st_read(list.files(system.file("extdata", "test_data", package="disagapp")
                                , pattern = ".shp", full.names = TRUE), quiet = TRUE)

shpdf_small <- data.frame(datapath = list.files(system.file("extdata", "test_data", package = "disagapp"), pattern = "mdg", full.names = TRUE),
                    name = list.files(system.file("extdata", "test_data", package = "disagapp"), pattern = "mdg"))

country_code <- c("LIE", "CHE")
area_column <- "area"
resp_column <- "response"
admin_level <- "ADM1"

if (is_ci){
  save_path <- tempfile(fileext = ".rds")
} else {
  save_path <- "~/temprds/saved_file.rds"
}
