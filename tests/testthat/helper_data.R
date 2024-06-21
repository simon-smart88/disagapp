covdf <- data.frame(datapath = list.files(system.file("extdata/covariates", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/covariates", package="disagapp")))

shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/shapes", package="disagapp")))

aggdf <- data.frame(datapath = list.files(system.file("extdata/aggregation", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/aggregation", package="disagapp")))


df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

ch_df <- data.frame("area" =  c('Aargau', 'Appenzell Ausserrhoden', 'Appenzell Innerrhoden',
           'Basel-Landschaft', 'Basel-Stadt', 'Bern', 'Fribourg', 'Genève',
           'Glarus', 'Graubünden', 'Jura', 'Luzern', 'Neuchâtel', 'Nidwalden',
           'Obwalden', 'Schaffhausen', 'Schwyz', 'Solothurn', 'St. Gallen',
           'Thurgau', 'Ticino', 'Uri', 'Valais', 'Vaud', 'Zug', 'Zürich'),
           "response" = 1:26)

mdf <- rbind(df, ch_df)

save_path <- "~/temprds/saved_file.rds"

shp <- list.files(system.file("extdata/shapes", package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

temp_shape <- tempfile(fileext = ".shp")
sf::st_write(shape, temp_shape, quiet = TRUE)
shpdf_small <- data.frame(datapath = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape)), full.names = TRUE),
                    name = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape))))

country_code <- c("LIE", "CHE")
area_column <- "area"
resp_column <- "response"
admin_level <- "ADM1"
