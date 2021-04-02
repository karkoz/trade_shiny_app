# https://karkoz.shinyapps.io/trade_data/

# shiny app for trade data 
#shiny::runGitHub(repo = "trade_shiny_app",username = "karkoz",ref="main")


# run gist runGist("c6873769798a690b4c9bba63f9320239")
library(remotes)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggthemes)
library(forcats)
library(tidyr)
library(scales)
library(stringr)
library(countrycode)
library(classInt)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(rgeos)
library(rvest)
library(readr)
library(zoo)
library(ShinyEditor)
library(rdrop2)

#Sys.setlocale("LC_TIME", "English")

# wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# wd <- sub('\\/app.R$', "", wd)
# setwd(wd)

# corvus corporate colors
corvus_colors <- c(
  `turquoise70`= "#7fe6d9ff",
  `turquoise60`= "#55ddcdff",
  `turquoise50`= "#2bd4bfff",
  `turquoise40`= "#28a496ff",
  `turquoise35`= "#1e9586ff",
  `turquoise30`= "#1a7f72ff",
  `turquoise20`= "#11554cff",
  `turquoise12`= "#0a312cff",
  `turquoise10`= "#0a2926ff",
  `turquoise2` = "#020808ff",
  `gold70`     = "#ffe866ff",
  `gold60`     = "#ffe133ff",
  `gold50`     = "#ffda00ff",
  `gold40`     = "#ccb600ff",
  `gold35`     = "#b29800ff",
  `gold30`     = "#998700ff",
  `gold20`     = "#665a00ff",
  `gold12`     = "#3d3600ff",
  `gold10`     = "#332d00ff",
  `gold2`      = "#0a0800ff",
  `grey70`     = "#b8b3adff",
  `grey50`     = "#898076ff",
  `grey35`     = "#605952ff",
  `grey20`     = "#3a332cff",
  `green50`    = "#2ad565ff",
  `green35`    = "#1e9547ff",
  `green20`    = "#145229ff",
  `orange50`   = "#ff9900ff",
  `orange35`   = "#b26b00ff",
  `orange20`   = "#663d00ff")

#' Function to extract corvus colors as hex codes
#'
#' @param ... Character names of corvus_colors 
#'
corvus_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (corvus_colors)
  
  corvus_colors[cols]
}

# corvus_cols()
# 
# corvus_cols("gold50")

corvus_palettes <- list(
  `dv3`  = corvus_cols("turquoise35", "grey50", "gold50"),
  
  `dv5`  = corvus_cols("turquoise20", "turquoise35", "grey50", "gold50", "gold20"),
  
  `dv9`  = corvus_cols("turquoise12", "turquoise20", "turquoise35", "turquoise50", "grey70", "gold50", "gold35", "gold20", "gold12"),
  
  `sqGold`   = corvus_cols("gold70", "gold60", "gold50", "gold40", "gold30", "gold20", "gold10", "gold2"),
  
  `sqBlue` = corvus_cols("turquoise70", "turquoise60", "turquoise50", "turquoise40", "turquoise30", "turquoise20", "turquoise10", "turquoise2"),
  
  `mixed` = corvus_cols("turquoise50", "gold50", "grey50",
                        "turquoise35", "gold35", "grey35",
                        "turquoise20", "gold20", "grey20",
                        "green50", "orange50", "green35",
                        "orange35", "green20", "orange20"),
  `mixed2` = corvus_cols("turquoise50", "gold50", "grey50",
                         "green50", "orange50", "turquoise35",
                         "gold35", "grey35", "green35",
                         "orange35", "turquoise20", "gold20",
                         "grey20", "green20", "orange20")
  
)

#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
corvus_pal <- function(palette = "mixed", reverse = FALSE, ...) {
  pal <- corvus_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


'%ni%' <- Negate('%in%')

# token <- drop_auth()
# saveRDS(token, file = "token.rds")
token <- readRDS("token.rds")
drop_acc(dtoken = token)

# read data from dropbox
comtrade_data_2016_2020 <- drop_read_csv("shiny_app/comtrade_data_2016_2020.csv")
#comtrade_data_2016_2020 <- read.csv("comtrade_data_2016_2020.csv")
x <- comtrade_data_2016_2020 %>% 
  filter(partner %ni% "World") %>%
  mutate(Quantity = round(netweight_kg / 1000, 0)) %>%
  separate(period, into = c("Year", "Month"), sep = 4)
# 
x$Exporters <- toupper(enc2utf8(x$partner))
x$Importer <- toupper(enc2utf8(x$reporter))
# 
x$Region <- countrycode(sourcevar = x$Exporters, origin = "country.name", destination = "region")
x$Region <- enc2utf8(x$Region)
x <- x %>% mutate(Date = as.Date(paste(Year, Month, 1, sep = "-")),
                  Month = lubridate::month(Date, label = T))

x <- x %>% mutate_at("Region", ~replace(., is.na(.), "World"))

x <- x %>% select(Exporters, Date, Quantity, Region, Importer, Year, Month, commodity_code, HS_code_name, HS_code_fullname)

x$Exporters <- x$Exporters %>% str_trim()
#
x$Exporters[x$Exporters == "LAO PEOPLE'S DEMOCRATIC REPUBLIC"] <- "LAO PDR"
x$Exporters[x$Exporters == "LAO PEOPLE'S DEM REP"] <- "LAO PDR"
x$Exporters[x$Exporters == "VIET NAM"] <- "VIETNAM"
x$Exporters[x$Exporters == "HONG KONG, CHINA"] <- "HONG KONG"
x$Exporters[x$Exporters == "UNITED STATES OF AMERICA"] <- "UNITED STATES"
x$Exporters[x$Exporters == "DOMINICAN REPUBLIC"] <- "DOMINICAN REP."
x$Exporters[x$Exporters == "TAIPEI, CHINESE"] <- "TAIWAN"
x$Exporters[x$Exporters == "TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
x$Exporters[x$Exporters == "CZECH REPUBLIC"] <- "CZECH REP."
x$Exporters[x$Exporters == "VENEZUELA, BOLIVARIAN REPUBLIC OF"] <- "VENEZUELA"
x$Exporters[x$Exporters == "SAINT VINCENT AND THE GRENADINES"] <- "ST. VIN. AND GREN."
x$Exporters[x$Exporters == "ESWATINI"] <- "SWAZILAND"
x$Exporters[x$Exporters == "SOUTH SUDAN"] <- "S. SUDAN"
x$Exporters[x$Exporters == "IRAN, ISLAMIC REPUBLIC OF"] <- "IRAN"
x$Exporters[x$Exporters == "SAINT KITTS AND NEVIS"] <- "ST. KITTS AND NEVIS"
x$Exporters[x$Exporters == "RUSSIAN FEDERATION"] <- "RUSSIA"
x$Exporters[x$Exporters == "BOLIVIA, PLURINATIONAL STATE OF"] <- "BOLIVIA"
x$Exporters[x$Exporters == "WESTERN SAHARA"] <- "W. SAHARA"
x$Exporters[x$Exporters == "MACEDONIA, NORTH"] <- "MACEDONIA"
x$Exporters[x$Exporters == "SAO TOME AND PRINCIPE"] <- "SAO TOMĂ‰ AND PRINCIPE"
x$Exporters[x$Exporters == "KOREA, REPUBLIC OF"] <- "KOREA"
x$Exporters[x$Exporters == "MACAO, CHINA"] <- "MACAO"
x$Exporters[x$Exporters == "CHINA, MACAO SAR"] <- "MACAO"
x$Exporters[x$Exporters == "BOSNIA AND HERZEGOVINA"] <- "BOSNIA AND HERZ."
x$Exporters[x$Exporters == "ANTIGUA AND BARBUDA"] <- "ANTIGUA AND BARB."
x$Exporters[x$Exporters == "BRUNEI DARUSSALAM"] <- "BRUNEI"
x$Exporters[x$Exporters == "PALESTINE, STATE OF"] <- "PALESTINE"
x$Exporters[x$Exporters == "FRENCH POLYNESIA"] <- "FR. POLYNESIA"
x$Exporters[x$Exporters == "FRENCH SOUTHERN AND ANTARCTIC TERRITORIES"] <- "FR. S. ANTARCTIC LANDS"
x$Exporters[x$Exporters == "EQUATORIAL GUINEA"] <- "EQ. GUINEA"
x$Exporters[x$Exporters == "CONGO, DEMOCRATIC REPUBLIC OF THE"] <- "DEM. REP. CONGO"
x$Exporters[x$Exporters == "CAYMAN ISLANDS"] <- "CAYMAN IS."
x$Exporters[x$Exporters == "BRITISH INDIAN OCEAN TERRITORY"] <- "BR. INDIAN OCEAN TER."
x$Exporters[x$Exporters == "SOLOMON ISLANDS"] <- "SOLOMON IS."
x$Exporters[x$Exporters == "MARSHALL ISLANDS"] <- "MARSHALL IS."
x$Exporters[x$Exporters == "PITCAIRN"] <- "PITCAIRN IS."
x$Exporters[x$Exporters == "TURKS AND CAICOS ISLANDS"] <- "TURKS AND CAICOS IS."
x$Exporters[x$Exporters == "SYRIAN ARAB REPUBLIC"] <- "SYRIA"
x$Exporters[x$Exporters == "UNITED REP. OF TANZANIA"] <- "TANZANIA"
x$Exporters[x$Exporters == "LAO PEOPLE'S DEM. REP."] <- "LAO PDR"
x$Exporters[x$Exporters == "BOSNIA HERZEGOVINA"] <- "BOSNIA AND HERZ."
x$Exporters[x$Exporters == "REP. OF MOLDOVA"] <- "MOLDOVA"
x$Exporters[x$Exporters == "TFYR OF MACEDONIA"] <- "MACEDONIA"

x$Exporters <- enc2utf8(x$Exporters)
x$Exporters[x$Exporters == "CĂ”TE D'IVOIRE"] <- "COTE D'IVOIRE"

# remove Austria and Malta due to lack of data avaiability
x <- x %>% filter(Importer %ni% c("AUSTRIA", "MALTA") & Year %ni% c(2015,2020) & Region != "World")

x <- x %>% filter(Date > "2015-12-01" & Date < "2020-01-01")

x$commodity_code <- paste("0" ,x$commodity_code, sep = "")

# ordered list of hs codes with names

# list(`080450 (fresh or dried guavas, mangoes and mangosteens)` = "080450",
#      `081090 (other edible fruits)` = "081090",
#      `070960 (fresh or chilled fruits of the genus Capsicum or Pimenta)` = "070960",
#      `0805 (fresh or dried citrus fruits)` = "0805",
#      `070930 (fresh or chilled aubergines 'eggplants')` = "070930")

codes <- x %>%
  group_by(commodity_code, HS_code_name) %>%
  dplyr::summarise(freq = n()) %>% 
  mutate(name = paste(commodity_code, " (", HS_code_name, ")", sep = "")) %>%
  arrange(commodity_code)

codes_table <- x %>% 
  group_by(commodity_code, HS_code_fullname) %>% 
  dplyr::summarise(freq = n()) %>%
  arrange(commodity_code) %>%
  select(commodity_code, HS_code_fullname)

codes_list <- vector(mode = "list", length = 32)

for (i in 1:nrow(codes)) {
  name <- codes$name[i]
  code <- codes$commodity_code[i]
  names(codes_list)[[i]] <- name
  codes_list[[i]] <- code
}

world <- ne_countries(scale = "medium", continent = NULL, returnclass = "sf")
class(world)
world <- world %>% select(4,18,19,25)
world$name <- toupper(world$name)
world$name <- enc2utf8(world$name)
world$name[world$name == "CĂ”TE D'IVOIRE"] <- "COTE D'IVOIRE"

eurostat_austria_malta_2016_2019 <- drop_read_csv("shiny_app/eurostat_austria_malta_2016_2019.csv")
#eurostat_austria_malta_2016_2019 <- read.csv("eurostat_austria_malta_2016_2019.csv")
y <- eurostat_austria_malta_2016_2019
y$commodity_code <- paste("0" ,y$commodity_code, sep = "")
y <- y %>% mutate(Date = as.Date(Date),
                  Month = lubridate::month(Date, label = T),
                  Year = as.character(Year),
                  Quantity = round(Quantity))
y <- y %>% select(Exporters, Date, Quantity, Region, Importer, Year, Month, commodity_code)

x <- dplyr::bind_rows(x, y)

comtrade_list <- toupper(c("Belgium", "Bulgaria", "Croatia", "Cyprus", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Slovenia", "Spain"))
eurostat_list <- toupper(c("Austria", "Malta"))

js <- "
function openFullscreen(elem) {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}"

css <- "
#ggplot:-webkit-full-screen {
  height: 100%;
  margin: 0;
}
#ggplot:-ms-fullscreen {
  height: 100%;
}
#ggplot:fullscreen {
  height: 100%;
}"
my_choices2 <- stringr::str_wrap(names(codes_list), width = 30)
my_choices2 <- stringr::str_replace_all(my_choices2, "\\n", "<br>")

# download text for text editor, working only when run locally

download_text <- function(x) {
  temp <- tempfile()
  name <- paste("shiny_app/overview", x, ".txt", sep = "")
  drop_download(name, dtoken = token, overwrite = T, local_path = temp)
  t <- read_file(temp)
  return(t)
}
# 
use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv")


## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader(title = a(href = 'https://corvus-geostat.pl/en/', target="_blank",
                            img(src = 'corvus_geostat_logo.png',
                                title = "Company website", height = "30px")),
                  tags$li(HTML(paste0(
                    "<table style='margin-left:auto; margin-right:auto;'>",
                    "<tr>",
                    "<td style='padding: 10px;padding-top: 18px;'><a href='https://twitter.com/aszyniszewska?lang=en' target='_blank'><div style='font-size: 13px;'><i class='fab fa-twitter fa-lg'></i></div></a></td>",
                    "<td style='padding: 10px;padding-top: 18px;'><a href='https://www.linkedin.com/in/anna-szyniszewska-04a1011/' target='_blank'><div style='font-size: 13px;'><i class='fab fa-linkedin-in fa-lg'></i></div></a></td>",
                    "<td style='padding: 10px;padding-top: 18px;'><a href='https://github.com/aniaszy' target='_blank'><div style='font-size: 13px;'><i class='fab fa-github fa-lg'></i></div></a></td>",
                    "</tr>",
                    "</table>")),
                    class = "dropdown"))



## 2. sidebar 
sidebar <- dashboardSidebar(
  sidebarMenu(
    pickerInput(
      inputId = "codes",
      label = "HS code:", 
      choices = codes_list,
      selected = "080450",
      options = list(
        `live-search` = TRUE),
      choicesOpt = list(
        content = my_choices2
      ),
      
    ),
    radioGroupButtons(
      inputId = "source",
      label = "Source:",
      choices = c("UN Comtrade", 
                  "Eurostat"),
      justified = TRUE
    ),
    pickerInput(
      inputId = "importers",
      label = "Importer:", 
      choices = comtrade_list,
      selected = comtrade_list,
      options = list(
        `liveSearch` = TRUE,
        `actions-box` = TRUE),
      multiple = TRUE
      
    ),
    
    
    
    menuItem("HS codes fullnames", icon = icon("table"), tabName = "table", badgeLabel = "i", badgeColor = "blue"),
    menuItem("Top importers", tabName = "importers", icon = icon("far fa-chart-bar"), selected = TRUE),
    menuItem("Import by months and years", tabName = "plot1", icon = icon("far fa-chart-bar")),
    menuItem("Top 10 exporters", icon = icon("far fa-chart-bar"), tabName = "plot2"),
    menuItem("Exporters map", icon = icon("far fa-map"), tabName = "plot3"),
    menuItem("Seasonality", icon = icon("fas fa-chart-line"), tabName = "plot4_1"),
    menuItem("Seasonality by country", icon = icon("fas fa-chart-line"), tabName = "plot4"),
    menuItem("Import by regions", icon = icon("fas fa-chart-line"), tabName = "plot5"),
    menuItem("Import by regions, seasonality", icon = icon("far fa-chart-line"), tabName = "plot6"),
    menuItem(p(HTML("Quarterly export<br>by the top 5 exporters")), icon = icon("fas fa-chart-line"), tabName = "plot7"),
    menuItem("Heatplot 1", icon = icon("fas fa-border-all"), tabName = "plot8"),
    menuItem("Heatplot 2", icon = icon("fas fa-border-all"), tabName = "plot9"),
    menuItem("Heatplot 3", icon = icon("fas fa-border-all"), tabName = "plot10"),
    menuItem("Export by product groups", icon = icon("fas fa-border-all"), tabName = "plot11")
    
  )
)

body <- dashboardBody(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Trade data </span>\');
      })
     ')),
  
  tabItems(
    tabItem(tabName = "table",
            dataTableOutput('table')
    ),
    tabItem(tabName = "importers",
            fluidRow(
              box(title = "Plot", status = "primary", solidHeader = TRUE,
                  plotOutput("top_importers"), width = 12)
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent1', text = ""),
                  br(),
                  actionButton(
                    "save1",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            # fluidRow(  # Setup
            #   box(title = "Overview", status = "success", solidHeader = TRUE,
            #       p(textOutput("text")), width = 9),
            #   
            #   box(status = "info", solidHeader = TRUE,
            #       HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
            #                   "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
            #                   "<p style = 'font-size: 1.375rem;
            #                              font-weight: 400;
            #                              color: #606060;
            #                              font-family: Ubuntu,sans-serif;'>
            #                   This project has received funding from the
            #                   European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
            #                 </p>")), 
            #       width = 3))
    ),
    tabItem(tabName = "plot1",
            fluidRow(
              box(title = "Plot", status = "primary", solidHeader = TRUE,
                  plotly::plotlyOutput("correlation_plot"), width = 12)
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent2', text = ""),
                  br(),
                  actionButton(
                    "save2",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
    ),
    
    tabItem(tabName = "plot2",
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         checkboxInput("with_or_without4", "Incuding European countries", FALSE),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot2', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("top_10exporters_plot"), width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent3', text = ""),
                  br(),
                  actionButton(
                    "save3",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            
    ),
    
    tabItem(tabName = "plot3",
            fluidRow(
              box(title = "Plot", status = "primary", solidHeader = TRUE,
                  leafletOutput("export_map_2016_2019"), width = 12
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent4', text = ""),
                  br(),
                  actionButton(
                    "save4",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
    ),
    
    tabItem(tabName = "plot4_1",
            fluidRow(
              box(title = p("Plot", downloadButton('downloadPlot4', 'Download plot')), status = "primary", solidHeader = TRUE,
                  plotOutput("seasonality"), width = 12
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent5', text = ""),
                  br(),
                  actionButton(
                    "save5",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
    ),
    
    tabItem(tabName = "plot4",
            
            fluidRow(
              
              column(6,
                     box(title = p("Plot", downloadButton('downloadPlot4_1', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("seasonality_by_exporters"), width = "100%"
                     )       
              ),
              column(6,
                     box(title = p("Plot", downloadButton('downloadPlot4_2', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("seasonality_by_exporters2"), width = "100%"
                     )  
              )
            ),
            fluidRow(
              box(
                title = "Input 1", status = "warning", solidHeader = TRUE,
                selectInput("exporters", "Choose a exporter:", ""), width = 6
              ),
              box(
                title = "Input 2", status = "warning", solidHeader = TRUE,
                selectInput("exporters2", "Choose a exporter:", ""), width = 6
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent6', text = ""),
                  br(),
                  actionButton(
                    "save6",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            
    ),
    tabItem(tabName = "plot5",
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         pickerInput(
                           inputId = "multipleRegions",
                           label = "Select regions:", 
                           choices = c("Sub-Saharan Africa", "Europe & Central Asia",
                                       "Middle East & North Africa", "South Asia",
                                       "Latin America & Caribbean", "North America",
                                       "East Asia & Pacific"),
                           multiple = TRUE,
                           selected = c("Sub-Saharan Africa", "Europe & Central Asia",
                                        "Middle East & North Africa", "South Asia",
                                        "Latin America & Caribbean", "North America",
                                        "East Asia & Pacific"),
                           options = list(
                             `actions-box` = TRUE)
                         ),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot5', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("import_by_regions"), width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent7', text = ""),
                  br(),
                  actionButton(
                    "save7",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
    ),
    tabItem(tabName = "plot6",
            
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         selectInput("regions", "Choose a region:", c("All", "Sub-Saharan Africa", "Europe & Central Asia",
                                                                      "Middle East & North Africa", "South Asia",
                                                                      "Latin America & Caribbean", "North America",
                                                                      "East Asia & Pacific")),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot6', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("import_by_reg_seasonality"), width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent8', text = ""),
                  br(),
                  actionButton(
                    "save8",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            
    ),
    tabItem(tabName = "plot7",
            
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         checkboxInput("with_or_without1", "Incuding European countries", FALSE),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot7', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("top5exp"), width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent9', text = ""),
                  br(),
                  actionButton(
                    "save9",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
    ),
    tabItem(tabName = "plot8",
            
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         checkboxInput("with_or_without2", "Incuding European countries", FALSE),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot8', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("heatplot1"), 
                         width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent10', text = ""),
                  br(),
                  actionButton(
                    "save10",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            
    ),
    tabItem(tabName = "plot9",
            
            fluidRow(
              
              column(3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE,
                         selectInput("top10exporters", "Choose from Top10 esporters:", ""),
                         width = "100%")       
              ),
              
              column(9,
                     box(title = p("Plot", downloadButton('downloadPlot9', 'Download plot')), status = "primary", solidHeader = TRUE,
                         plotOutput("heatplot2"), 
                         width = "100%")
              )
            ),
            fluidRow(  # Setup
              use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
              
              # Text Input 1
              fluidRow(
                column(
                  width = 9,
                  editor(id = 'textcontent11', text = ""),
                  br(),
                  actionButton(
                    "save11",
                    "Save",
                    icon = icon("edit")
                  )),
                column(
                  width = 3,
                  box(status = "info", solidHeader = TRUE,
                      HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                  "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                  "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                      width = "100%")
                )
              ))
            
    ),
    tabItem(tabName = "plot10",
            fluidPage(
              tags$head(
                tags$script(HTML(js)),
                tags$style(HTML(css))
              ),
              br(),
              fluidRow(
                
                column(3,
                       box(title = "Inputs", status = "warning", solidHeader = TRUE,
                           checkboxInput("with_or_without3", "Incuding European countries", FALSE),
                           width = "100%")       
                ),
                
                column(9,
                       box(title = p("Plot", downloadButton('downloadPlot10', 'Download plot'), actionButton(
                         "fs", "", icon = icon("fas fa-expand-arrows-alt", "fas-2x"), 
                         onclick = "openFullscreen(document.getElementById('top_10exporters_plot'));"
                       )), status = "primary", solidHeader = TRUE,
                       plotOutput("heatplot3"), 
                       width = "100%")
                )
              ),
              fluidRow(  # Setup
                use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
                
                # Text Input 1
                fluidRow(
                  column(
                    width = 9,
                    editor(id = 'textcontent12', text = ""),
                    br(),
                    actionButton(
                      "save12",
                      "Save",
                      icon = icon("edit")
                    )),
                  column(
                    width = 3,
                    box(status = "info", solidHeader = TRUE,
                        HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                    "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                    "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                        width = "100%")
                  )
                )) 
            )
            
    ),
    tabItem(tabName = "plot11",
            fluidRow(
              column(12,
                     tabsetPanel(position = "below",
                                 tabPanel("Tab 1", 
                                          fluidRow(
                                            box(title = "Plot", status = "primary", solidHeader = TRUE,
                                                plotly::plotlyOutput("compare_product"), width = 12
                                            )
                                          ),
                                          fluidRow(  # Setup
                                            use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
                                            
                                            # Text Input 1
                                            fluidRow(
                                              column(
                                                width = 9,
                                                editor(id = 'textcontent13', text = ""),
                                                br(),
                                                actionButton(
                                                  "save13",
                                                  "Save",
                                                  icon = icon("edit")
                                                )),
                                              column(
                                                width = 3,
                                                box(status = "info", solidHeader = TRUE,
                                                    HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                                                "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                                                "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                                                    width = "100%")
                                              )
                                            ))
                                 ), 
                                 tabPanel("Tab 2", 
                                          fluidRow(
                                            box(title = "Plot", status = "primary", solidHeader = TRUE,
                                                plotly::plotlyOutput("compare_product_by_region"), width = 12
                                            )
                                          ),
                                          fluidRow(  # Setup
                                            use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
                                            
                                            # Text Input 1
                                            fluidRow(
                                              column(
                                                width = 9,
                                                editor(id = 'textcontent14', text = ""),
                                                br(),
                                                actionButton(
                                                  "save14",
                                                  "Save",
                                                  icon = icon("edit")
                                                )),
                                              column(
                                                width = 3,
                                                box(status = "info", solidHeader = TRUE,
                                                    HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                                                "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                                                "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                                                    width = "100%")
                                              )
                                            ))
                                 ), 
                                 tabPanel("Tab 3", 
                                          fluidRow(
                                            
                                            column(3,
                                                   box(title = "Inputs", status = "warning", solidHeader = TRUE,
                                                       
                                                       pickerInput(
                                                         inputId = "multipleCodes",
                                                         label = "Select product codes:", 
                                                         choices = codes_list,
                                                         multiple = TRUE,
                                                         choicesOpt = list(
                                                           content = my_choices2
                                                         )
                                                       ),
                                                       width = "100%")       
                                            ),
                                            
                                            column(9,
                                                   box(title = p("Plot", downloadButton('downloadPlot11', 'Download plot')), status = "primary", solidHeader = TRUE,
                                                       plotOutput("import_by_product_codes"), 
                                                       width = "100%")
                                            )
                                          ),
                                          fluidRow(  # Setup
                                            use_editor("n2dsgmto6sikj8fwkxsua54ix0j18zcdpifat2lh2q4be3nv"),
                                            
                                            # Text Input 1
                                            fluidRow(
                                              column(
                                                width = 9,
                                                editor(id = 'textcontent15', text = ""),
                                                br(),
                                                actionButton(
                                                  "save15",
                                                  "Save",
                                                  icon = icon("edit")
                                                )),
                                              column(
                                                width = 3,
                                                box(status = "info", solidHeader = TRUE,
                                                    HTML(paste0("<a href='https://fruitflies-ipm.eu/' target='_blank'><img title = 'Project website' width='200' height='168' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png' class='image wp-image-138  attachment-medium size-medium' alt='' loading='lazy' style='max-width: 100%; height: auto;' srcset='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-300x168.png 300w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-260x146.png 260w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-50x28.png 50w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541-134x75.png 134w, https://fruitflies-ipm.eu/wp-content/uploads/2019/10/FFIPM_logo-neg-05-e1572003186541.png 657w' sizes='(max-width: 300px) 100vw, 300px'></a>",
                                                                "<a href='https://ec.europa.eu/programmes/horizon2020/' target='_blank'><img title = 'Horizon 2020 program website' width='150' height='102' src='https://fruitflies-ipm.eu/wp-content/uploads/2019/10/EU-flag-border-e1572002837360.png' class='image wp-image-165  attachment-full size-full' alt='' loading='lazy' style='max-width: 100%; height: auto;'></a>",
                                                                "<p style = 'font-size: 1.375rem;
                                           font-weight: 400;
                                           color: #606060;
                                           font-family: Ubuntu,sans-serif;'>
                                This project has received funding from the
                                European Union's Horizon 2020 research and innovation programme under grant agreement No 818184.
                              </p>")), 
                                                    width = "100%")
                                              )
                                            ))
                                 )
                     ))
            )
    )
    
  )
  
)


## put UI together --------------------
ui <- dashboardPage(title = "Trade data", header, sidebar, body )



server <- function(input, output, session) {
  options(scipen=999)
  
  
  output$table <- renderDataTable(codes_table)
  
  #output$table <- renderTable(codes_table)
  
  text <- function() {
    if(identical(which(codes_list %in% input$codes), integer(0)) == TRUE) {
      txt = "missing title" 
    } else {
      txt = names(codes_list)[which(codes_list %in% input$codes)]  
    }
    return(txt)
  }
  
  output$text <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text1 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text2 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text3 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text4_1 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text4 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text5 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text6 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text7 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text8 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text9 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text10 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text11 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text12 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  output$text13 <- renderText({ paste("HS code:", input$codes, "-", text(), "-", "short info about plot", sep = " ") })  
  
  exporters <- reactive({
    the_biggest_exporters <- x %>% 
      filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
      group_by(Exporters) %>% 
      dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
      arrange(desc(Sum_export))
    exporters <- the_biggest_exporters %>% head(n = 50) %>% select(Exporters) %>% as.vector()
    exporters <- dplyr::pull(exporters, "Exporters")
    return(exporters)
  })
  
  
  observe({
    updateSelectInput(session, "exporters",
                      choices = exporters()
    )})
  
  observe({
    updateSelectInput(session, "exporters2",
                      choices = exporters()
    )})
  
  # text editor update 
  observe({
    
    UpdateEditor(session,
                 id = "textcontent1",
                 text = download_text("1"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent2",
                 text = download_text("2"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent3",
                 text = download_text("3"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent4",
                 text = download_text("4"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent5",
                 text = download_text("5"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent6",
                 text = download_text("6"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent7",
                 text = download_text("7"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent8",
                 text = download_text("8"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent9",
                 text = download_text("9"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent10",
                 text = download_text("10"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent11",
                 text = download_text("11"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent12",
                 text = download_text("12"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent13",
                 text = download_text("13"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent14",
                 text = download_text("14"))
    
  })
  
  observe({
    
    UpdateEditor(session,
                 id = "textcontent15",
                 text = download_text("15"))
    
  })
  # 
  top10exporters <- reactive({
    the_biggest_exporters <- x %>% 
      filter(commodity_code %in% input$codes & Region != "Europe & Central Asia" & Importer %in% input$importers) %>%
      group_by(Exporters) %>% 
      dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
      arrange(desc(Sum_export)) 
    exporters <- the_biggest_exporters %>% head(n = 10) %>% select(Exporters) %>% as.vector()
    exporters <- dplyr::pull(exporters, "Exporters")
    return(exporters)
  })
  
  observe({
    updateSelectInput(session, "top10exporters",
                      choices = top10exporters()
    )})
  
  
  plot1 <- function(){
    levels(x$Year) <- c(2016, 2017, 2018, 2019)
    txt <- text()
    title <- paste("Import of", txt, "\nby the seleted European countries* in 2016-2019.", sep = " ")
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    
    p <- x %>% 
      filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
      group_by(Month, Year) %>%
      dplyr::summarise(Quantity = sum(Quantity, na.rm = T)) %>%
      ggplot(data = ., aes(x = Month, y = Quantity, fill = Year)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      labs(y = "Quantity [tons]", x = "Month",
           caption = captions,
           title = title) +
      scale_fill_manual(values = corvus_pal("mixed")(15))
    
    ggplotly(p) %>% 
      layout(margin = list(l = 50, r = 50, t = 60, b = 110),
             annotations = list(text = captions,
                                font =  list(size = 11),
                                showarrow = FALSE,
                                xref = 'paper', x = 1,
                                yref = 'paper', y = -0.45,
                                align = 'right')) 
  }
  
  output$correlation_plot <- plotly::renderPlotly({
    plot1()
  }) 
  
  # without europe
  #x_2 <- x %>% filter(Region != "Europe & Central Asia" & Region != "World")
  
  top_importers <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    
    title <- paste("Quantity of", txt, "import \nby the seleted European countries* in 2016-2019.", sep = " ")
    
    x %>%
      filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
      group_by(Importer) %>%
      dplyr::summarise(Sum_import = sum(Quantity, na.rm = T)) %>%
      arrange(desc(Sum_import)) %>%
      head(n = 10) %>%
      ggplot(data = ., mapping = aes(x = reorder(Importer, -Sum_import), y= Sum_import)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            plot.title = element_text(size=17),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]", x = "Importers",
           caption=captions,
           title = title) +
      geom_text(aes(label=Sum_import), position=position_dodge(width=0.9), vjust=-0.25, size = 4)
    
    # ggplotly(p) %>% 
    #   layout(margin = list(l = 50, r = 50, t = 60, b = 110),
    #          annotations = list(text = 'Data source: UN Comtrade\n* Belgium, Bulgaria, Croatia, Cyprus, France, Germany, Greece, Italy, Netherlands, Portugal, Slovenia, Spain.',
    #                             font =  list(size = 11),
    #                             showarrow = FALSE,
    #                             xref = 'paper', x = 1,
    #                             yref = 'paper', y = -0.45,
    #                             align = 'right'))
    
    
  }
  
  output$top_importers <- renderPlot({
    top_importers()
  })
  
  
  top_10exporters_plot <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    if (input$with_or_without4 == FALSE) {
      title <- paste("Top 10 exporters (excluding European countries) of", txt, "imported\nby the seleted European countries* in 2016-2019.", sep = " ")
      
      x %>%
        filter(commodity_code %in% input$codes & Region != "Europe & Central Asia" & Importer %in% input$importers) %>%
        group_by(Exporters) %>%
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        head(n = 10) %>%
        ggplot(data = ., mapping = aes(x = reorder(Exporters, -Sum_export), y= Sum_export)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10)) +
        labs(y = "Quantity [tons]", x = "Exporters",
             caption=captions,
             title = title) +
        geom_text(aes(label=Sum_export), position=position_dodge(width=0.9), vjust=-0.25, size = 4)
      
      # ggplotly(p) %>% 
      #   layout(margin = list(l = 50, r = 50, t = 60, b = 110),
      #          annotations = list(text = 'Data source: UN Comtrade\n* Belgium, Bulgaria, Croatia, Cyprus, France, Germany, Greece, Italy, Netherlands, Portugal, Slovenia, Spain.',
      #                             font =  list(size = 11),
      #                             showarrow = FALSE,
      #                             xref = 'paper', x = 1,
      #                             yref = 'paper', y = -0.45,
      #                             align = 'right'))
    } else {
      title <- paste("Top 10 exporters of", txt, "imported\nby the seleted European countries* in 2016-2019.", sep = " ")
      
      x %>%
        filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
        group_by(Exporters) %>%
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        head(n = 10) %>%
        ggplot(data = ., mapping = aes(x = reorder(Exporters, -Sum_export), y= Sum_export)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10)) +
        labs(y = "Quantity [tons]", x = "Exporters",
             caption=captions,
             title = title) +
        geom_text(aes(label=Sum_export), position=position_dodge(width=0.9), vjust=-0.25, size = 4)
    }
    
  }
  
  output$top_10exporters_plot <- renderPlot({
    top_10exporters_plot()
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = "top_10exporters.png",
    content = function(file) {
      ggsave(file,top_10exporters_plot(), width = 12, height = 7)
    }
  )
  
  output$export_map_2016_2019 <- renderLeaflet({
    txt <- text()
    
    exporters_2016_2019 <- x %>%
      filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
      group_by(Exporters) %>%
      dplyr::summarise(Quantity = mean(Quantity, na.rm = T))
    
    #anti_join_ <- exporters_2016_2019 %>% anti_join(world, by = c("Exporters" = "name"))
    
    exporters_2016_2019_sf <- exporters_2016_2019 %>% right_join(world, by = c("Exporters" = "name"))
    exporters_2016_2019_sf <- st_as_sf(exporters_2016_2019_sf)
    sapply(exporters_2016_2019_sf, class) 
    
    map_title <- paste("Annual average export for", txt, "\nto selected European countries* in 2016-2019.")
    #Breaks <- classIntervals(exporters_2016_2019_sf$Quantity, n = 6, style = "pretty")
    
    tm <- tm_shape(exporters_2016_2019_sf, projection="+proj=robin") +
      tm_fill(col = "Quantity",
              palette = corvus_pal("sqGold")(5), 
              title = "Exported quantity [tons]", 
              textNA = "No data", 
              colorNA = "#bdbdbd",
              style = "jenks") +
      tm_borders(col = "#3d3c38", lwd = 0.6, alpha = 0.5) +
      tm_layout(title = map_title,
                title.position = "left",
                title.size = 0.8, fontface = 3,
                legend.title.size=0.8,
                legend.text.size = 0.6,
                earth.boundary = TRUE,
                bg.color = "#9ecae1",
                space.color="white",
                attr.outside = TRUE) + 
      tm_view(set.view = c(-8, 20, 1), view.legend.position = c("left", "bottom"))
    
    tmap_leaflet(tm)
  })
  
  seasonality <- function(){
    txt <- text()
    Title <- paste("Monthly quantity of", txt, "\nimported by the selected European countries* in 2015-2019.")
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    p <- x %>% 
      filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
      group_by(Date) %>%
      dplyr::summarise(sum = sum(Quantity, na.rm = T)) %>%
      mutate(Year = as.factor(year(Date)),
             Month = month(Date, label = T)) %>%
      ggplot(data = ., aes(x = Month, y = sum, colour = Year)) +
      geom_line(aes(group = Year), size = 1, linetype="dotdash") +
      geom_point(aes(color=Year), size = 3.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 50, hjust=1),
            plot.title = element_text(size=17),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]", 
           caption = captions,
           title = Title) +
      scale_color_manual(values = corvus_pal("mixed")(15))
    p
    
  }
  
  output$seasonality <- renderPlot({
    seasonality()
  })
  
  output$downloadPlot4 <- downloadHandler(
    filename = "seasonality.png",
    content = function(file) {
      ggsave(file,seasonality(), width = 12, height = 7)
    }
  )
  
  seasonality_by_exporters <- function(){
    txt <- text()
    Title <- paste("Monthly quantity of", txt, "\nimported by the selected European countries* in 2015-2019 from", input$exporters)
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    p <- x %>% 
      filter(commodity_code %in% input$codes & Exporters %in% input$exporters & Importer %in% input$importers) %>%
      group_by(Date) %>%
      dplyr::summarise(sum = sum(Quantity, na.rm = T)) %>%
      mutate(Year = as.factor(year(Date)),
             Month = month(Date, label = T)) %>%
      ggplot(data = ., aes(x = Month, y = sum, colour = Year)) +
      geom_line(aes(group = Year), size = 1, linetype="dotdash") +
      geom_point(aes(color=Year), size = 2.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 50, hjust=1),
            plot.title = element_text(size=13),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]", title = Title, caption = captions) +
      scale_color_manual(values = corvus_pal("mixed")(15))
    
    p  
  }
  
  output$seasonality_by_exporters <- renderPlot({
    seasonality_by_exporters()
  })
  
  output$downloadPlot4_1 <- downloadHandler(
    filename = "seasonality_by_exporters.png",
    content = function(file) {
      ggsave(file,seasonality_by_exporters(), width = 12, height = 7)
    }
  )
  
  seasonality_by_exporters2 <- function(){
    txt <- text()
    Title <- paste("Monthly quantity of", txt, "\nimported by the selected European countries* in 2015-2019 from", input$exporters2)
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    p <- x %>% 
      filter(commodity_code %in% input$codes & Exporters %in% input$exporters2 & Importer %in% input$importers) %>%
      group_by(Date) %>%
      dplyr::summarise(sum = sum(Quantity, na.rm = T)) %>%
      mutate(Year = as.factor(year(Date)),
             Month = month(Date, label = T)) %>%
      ggplot(data = ., aes(x = Month, y = sum, colour = Year)) +
      geom_line(aes(group = Year), size = 1, linetype="dotdash") +
      geom_point(aes(color=Year), size = 2.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 50, hjust=1),
            plot.title = element_text(size=13),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]", title = Title, caption = captions) +
      scale_color_manual(values = corvus_pal("mixed")(15))
    
    p  
  }
  
  output$seasonality_by_exporters2 <- renderPlot({
    seasonality_by_exporters2()
  })
  
  output$downloadPlot4_2 <- downloadHandler(
    filename = "seasonality_by_exporters2.png",
    content = function(file) {
      ggsave(file,seasonality_by_exporters2(), width = 12, height = 7)
    }
  )
  
  import_by_regions <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    x %>%
      filter(commodity_code %in% input$codes & Region %in% input$multipleRegions & Importer %in% input$importers) %>%
      group_by(Region, Date) %>%
      dplyr::summarise(sum_region = sum(Quantity, na.rm = T)) %>%
      ggplot(data = ., aes(x = Date, y = sum_region, colour = Region))+
      geom_line(size = 1, linetype="dotdash") + 
      geom_point(aes(color=Region), size = 2.5) +
      xlab("") + 
      scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2019-12-01"), by="6 months"), date_labels = "%Y %b") +
      theme_minimal() +
      theme(plot.title = element_text(size=17),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]",
           caption = captions,
           title = paste("Monthly quantity of", txt, "imported\nby the selected European countries* in 2016-2019 broken down by Regions.", sep = " ")) +
      scale_colour_manual(values = c("East Asia & Pacific"="#2bd4bfff", "Europe & Central Asia"="#ffda00ff", 
                                     "Latin America & Caribbean"="#898076ff", "Middle East & North Africa"="#2ad565ff",
                                     "North America"="#ff9900ff" , "South Asia"="#1e9586ff", "Sub-Saharan Africa"="#b29800ff"))
    
  }
  
  
  output$import_by_regions <- renderPlot({
    import_by_regions()
  })
  
  output$downloadPlot5 <- downloadHandler(
    filename = "import_by_regions.png",
    content = function(file) {
      ggsave(file,import_by_regions(), width = 12, height = 7)
    }
  )
  
  
  import_by_reg_seasonality <- function(){
    
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    if (input$regions == "All") {
      
      x %>% 
        filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
        group_by(Region, Month) %>%
        dplyr::summarise(sum = sum(Quantity, na.rm = T)) %>%
        ggplot(data = ., aes(x = Month, y = sum, colour = Region)) +
        geom_line(aes(group = Region), size = 1, linetype="dotdash") +
        geom_point(aes(color=Region), size = 2.5) +
        theme_minimal() +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10)) +
        labs(y = "Quantity [tons]",
             caption = captions,
             title = paste("Monthly quantity of", txt, "\nimported by the selected European countries* in 2016-2019 broken down by regions.", sep = " "))  +
        scale_color_manual(values = corvus_pal("mixed")(15))
    } else {
      
      region <- input$regions
      
      x %>% 
        filter(commodity_code %in% input$codes & Region %in% region & Importer %in% input$importers) %>%
        group_by(Date) %>%
        dplyr::summarise(sum = sum(Quantity, na.rm = T)) %>%
        mutate(Year = as.factor(year(Date)),
               Month = month(Date, label = T)) %>%
        ggplot(data = ., aes(x = Month, y = sum, colour = Year)) +
        geom_line(aes(group = Year), size = 1, linetype="dotdash") +
        geom_point(aes(color=Year), size = 2.5) +
        theme_minimal() +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10)) +
        labs(y = "Quantity [tons]",
             caption = captions,
             title = paste("Monthly quantity of", txt, "\nimported by the selected European countries* in 2016-2019 from", region, sep = " ")) +
        scale_color_manual(values = corvus_pal("mixed")(15))
      
    }
    
  }
  
  output$import_by_reg_seasonality <- renderPlot({
    import_by_reg_seasonality()
  })
  
  output$downloadPlot6 <- downloadHandler(
    filename = "import_by_reg_seasonality.png",
    content = function(file) {
      ggsave(file,import_by_reg_seasonality(), width = 12, height = 7)
    }
  )
  
  top5exp <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    if (input$with_or_without1 == FALSE) {
      
      ####
      x_2 <- x %>% 
        filter(commodity_code %in% input$codes & Region != "Europe & Central Asia" & Importer %in% input$importers)
      
      top5Exporters_lat4yrs <- x_2 %>%
        group_by(Exporters) %>%
        dplyr::summarise(Quantity = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Quantity)) %>%
        head(n = 5)
      
      imports_last4yrs <- x_2 %>%
        filter(Exporters %in% top5Exporters_lat4yrs$Exporters) %>%
        mutate(Quarter = as.yearqtr(Date, format = "%Y-%m-%d")) %>%
        group_by(Exporters, Quarter) %>%
        dplyr::summarise(Quantity = sum(Quantity, na.rm = T))
      
      ggplot(data = imports_last4yrs, aes(x = Quarter, y = Quantity, colour = Exporters)) +
        geom_line(size = 1, linetype="dotdash") + 
        geom_point(aes(color=Exporters), size = 2.5) +
        xlab("") + 
        theme_minimal() +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10),
              axis.text.x=element_text(angle=60, hjust=1)) +
        labs(y = "Quantity [tons]",
             caption = captions,
             title = paste("Annual export of", txt, "for the 5 largest exporters\n(excluding European countries) to selected European countries* in 2016-2019.", sep = " ")) +
        scale_x_yearqtr(format = "%Y-%q", n = 16) + 
        scale_color_manual(values = corvus_pal("mixed")(15)) 
      
    } else {
      
      top5Exporters_lat4yrs <- x %>%
        filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
        group_by(Exporters) %>%
        dplyr::summarise(Quantity = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Quantity)) %>%
        head(n = 5)
      
      imports_last4yrs <- x %>%
        filter(Exporters %in% top5Exporters_lat4yrs$Exporters) %>%
        mutate(Quarter = as.yearqtr(Date, format = "%Y-%m-%d")) %>%
        group_by(Exporters, Quarter) %>%
        dplyr::summarise(Quantity = sum(Quantity, na.rm = T))
      
      ggplot(data = imports_last4yrs, aes(x = Quarter, y = Quantity, colour = Exporters)) +
        geom_line(size = 1, linetype="dotdash") + 
        geom_point(aes(color=Exporters), size = 2.5) +
        theme_minimal() +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10),
              axis.text.x=element_text(angle=60, hjust=1)) +
        labs(y = "Quantity [tons]",
             caption = captions,
             title = paste("Quarterly export of", txt, "for the 5 largest exporters\n(including European countries) to selected European countries* in 2016-2019.", sep = " ")) + 
        scale_x_yearqtr(format = "%Y-%q", n = 16) + 
        scale_color_manual(values = corvus_pal("mixed")(15)) 
      
    }
    
  }
  
  output$top5exp <- renderPlot({
    top5exp()
  })
  
  output$downloadPlot7 <- downloadHandler(
    filename = "top5exp.png",
    content = function(file) {
      ggsave(file,top5exp(), width = 12, height = 7)
    }
  )
  
  # heatplot 1
  
  heatplot1 <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    '%ni%' <- Negate('%in%')
    
    if (input$with_or_without2 == FALSE) {
      x_2 <- x %>% 
        filter(commodity_code %in% input$codes & Region != "Europe & Central Asia" & Importer %in% input$importers)
      # Annual exports of by the 15 largest exporters. (excluding european countries)
      top_15_exporters <- x_2 %>%
        group_by(Exporters) %>%
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        head(n = 15)
      
      library(BAMMtools)
      
      the_biggest_exporters_by_year <- x_2 %>% 
        group_by(Exporters, Year) %>% 
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        filter(Exporters %in% top_15_exporters$Exporters) 
      
      ##################
      
      attach(the_biggest_exporters_by_year)
      
      col1 = "#d8e1cf"
      col2 = "#2a7878"
      # palette5 <- viridis_pal(option = "D")(11) 
      
      Breaks <- classIntervals(the_biggest_exporters_by_year$Sum_export, n = 6, style = "pretty")
      
      ggplot(the_biggest_exporters_by_year, aes(Year, Exporters, fill = Sum_export)) + 
        geom_tile(colour="grey95") + 
        scale_fill_gradientn(name = "Quantity [tons]", 
                             colours = corvus_pal("sqGold")(8), 
                             values = c(0,0.001,0.1, 0.2, 0.4, 0.7,1),
                             breaks = Breaks$brks)+
        theme_minimal() + theme_minimal() +
        labs(title = paste("Annual exports of", txt, "to selected\nEuropean countries* in 2016-2019 by the 15 largest exporters (excluding European countries).", sep = " "),
             x = "Year", y = "Exporters",
             caption = captions) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10),
              legend.key.height = unit(2, "cm")) +
        scale_y_discrete(limits=(rev(top_15_exporters$Exporters)))
      
    } else {
      x <- x %>% 
        filter(commodity_code %in% input$codes & Importer %in% input$importers)
      # Annual exports of by the 15 largest exporters. (excluding european countries)
      top_15_exporters <- x %>%
        group_by(Exporters) %>%
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        head(n = 15)
      
      library(BAMMtools)
      
      the_biggest_exporters_by_year <- x %>% 
        group_by(Exporters, Year) %>% 
        dplyr::summarise(Sum_export = sum(Quantity, na.rm = T)) %>%
        arrange(desc(Sum_export)) %>%
        filter(Exporters %in% top_15_exporters$Exporters) 
      
      ##################
      
      attach(the_biggest_exporters_by_year)
      
      col1 = "#d8e1cf"
      col2 = "#2a7878"
      # palette5 <- viridis_pal(option = "D")(11) 
      
      Breaks <- classIntervals(the_biggest_exporters_by_year$Sum_export, n = 6, style = "pretty")
      
      ggplot(the_biggest_exporters_by_year, aes(Year, Exporters, fill = Sum_export)) + 
        geom_tile(colour="grey95") + 
        scale_fill_gradientn(name = "Quantity [tons]", 
                             colours = corvus_pal("sqGold")(8), 
                             values = c(0,0.001,0.1, 0.2, 0.4, 0.7,1),
                             breaks = Breaks$brks)+
        theme_minimal() + theme_minimal() +
        labs(title = paste("Annual exports of", txt, "to selected\nEuropean countries* in 2016-2019 by the 15 largest exporters (including European countries).", sep = " "),
             x = "Year", y = "Exporters",
             caption = captions) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10),
              axis.text.x=element_text(angle=60, hjust=1),
              legend.key.height = unit(2, "cm")) +
        scale_y_discrete(limits=(rev(top_15_exporters$Exporters)))
    }
  }
  
  output$heatplot1 <- renderPlot({
    heatplot1()
  })
  
  output$downloadPlot8 <- downloadHandler(
    filename = "heatplot1.png",
    content = function(file) {
      ggsave(file,heatplot1(), width = 12, height = 7)
    }
  )
  
  heatplot2 <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    exporter <- input$top10exporters
    top_exporter_export <- x %>%
      filter(commodity_code %in% input$codes & Exporters %in% exporter & Importer %in% input$importers) %>%
      group_by(Importer, Year) %>%
      dplyr::summarise(Annual_export = sum(Quantity, na.rm = T))
    
    top_exporter_total <- top_exporter_export %>%
      group_by(Importer) %>%
      dplyr::summarise(Total_export = sum(Annual_export, na.rm = T)) %>%
      arrange(desc(Total_export))
    
    Breaks <- classIntervals(top_exporter_export$Annual_export, n = 6, style = "pretty")
    
    ggplot(top_exporter_export, aes(Year, Importer, fill = Annual_export)) +
      geom_tile(colour="grey90") +
      scale_fill_gradientn(name = "Quantity [tons]",
                           colours = corvus_pal("sqGold")(8),
                           values = c(0,0.001,0.1, 0.2, 0.4, 0.7,1),
                           breaks = Breaks$brks,
                           na.value = "grey50") +
      theme_minimal() + theme_minimal() +
      labs(title = paste("Annual import of", txt, "\nfrom", exporter, "to selected European countries* in 2016-2019.", sep = " "),
           caption = captions,
           x = "Year", y = "Importers") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(size=17),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            strip.text = element_text(size=9, face = "bold"),
            legend.key.height = unit(2, "cm"),
            plot.caption = element_text(size = 10)) +
      scale_y_discrete(limits=(rev(top_exporter_total$Importer)))
    
  }
  
  output$heatplot2 <- renderPlot({
    heatplot2()
  })
  
  output$downloadPlot9 <- downloadHandler(
    filename = "heatplot2.png",
    content = function(file) {
      ggsave(file,heatplot2(), width = 12, height = 7)
    }
  )
  
  # heatplot 3 
  heatplot3 <- function(){
    txt <- text()
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    if (input$with_or_without3 == FALSE) {
      #excluding europe
      x_2 <- x %>% 
        filter(commodity_code %in% input$codes & Region != "Europe & Central Asia" & Importer %in% input$importers)
      
      sum_by_exporters_importers <- x_2 %>%
        group_by(Exporters, Importer) %>%
        dplyr::summarise(Total_quantity = sum(Quantity, na.rm = T)) #%>%
      #spread(Importer, Total_quantity)
      
      top_30_exp <- sum_by_exporters_importers %>%
        group_by(Exporters) %>%
        dplyr::summarise(Total_export = sum(Total_quantity, na.rm = T)) %>%
        arrange(desc(Total_export)) %>%
        head(n = 25)
      
      top_imp <- sum_by_exporters_importers %>%
        filter(Exporters %in% top_30_exp$Exporters) %>%
        group_by(Importer) %>%
        dplyr::summarise(Total_import = sum(Total_quantity, na.rm = T)) %>%
        arrange(desc(Total_import)) 
      
      sum_by_exporters_importers <- sum_by_exporters_importers %>%
        filter(Exporters %in% top_30_exp$Exporters)
      
      Breaks <- classIntervals(sum_by_exporters_importers$Total_quantity, n = 6, style = "pretty")
      
      ggplot(sum_by_exporters_importers, aes(Importer, Exporters, fill = Total_quantity)) + 
        geom_tile(colour="grey90") + 
        scale_fill_gradientn(name = "Quantity [tons]", 
                             colours = corvus_pal("sqGold")(8), 
                             values = c(0,0.001,0.1, 0.2, 0.4, 0.7,1),
                             breaks = Breaks$brks, 
                             na.value = "grey50")+
        theme_minimal() + theme_minimal() +
        labs(title = paste("Trade of", txt, "in 2016-2019 between\nselected European countries* and the 25 largest exporters (excluding European countries).", sep = " "),
             caption=captions,
             x = "Importers", y = "Exporters") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.text.x=element_text(angle=40, hjust=1),
              plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              strip.text = element_text(size=9, face = "bold"),
              legend.key.height = unit(2, "cm"),
              plot.caption = element_text(size = 10)) +
        scale_y_discrete(limits=(rev(top_30_exp$Exporters))) +
        scale_x_discrete(limits=(top_imp$Importer))
      
    } else {
      #including europe
      
      sum_by_exporters_importers <- x %>%
        filter(commodity_code %in% input$codes & Importer %in% input$importers) %>%
        group_by(Exporters, Importer) %>%
        dplyr::summarise(Total_quantity = sum(Quantity, na.rm = T)) #%>%
      #spread(Importer, Total_quantity)
      
      top_30_exp <- sum_by_exporters_importers %>%
        group_by(Exporters) %>%
        dplyr::summarise(Total_export = sum(Total_quantity, na.rm = T)) %>%
        arrange(desc(Total_export)) %>%
        head(n = 25)
      
      top_imp <- sum_by_exporters_importers %>%
        filter(Exporters %in% top_30_exp$Exporters) %>%
        group_by(Importer) %>%
        dplyr::summarise(Total_import = sum(Total_quantity, na.rm = T)) %>%
        arrange(desc(Total_import)) 
      
      sum_by_exporters_importers <- sum_by_exporters_importers %>%
        filter(Exporters %in% top_30_exp$Exporters)
      
      Breaks <- classIntervals(sum_by_exporters_importers$Total_quantity, n = 6, style = "pretty")
      
      ggplot(sum_by_exporters_importers, aes(Importer, Exporters, fill = Total_quantity)) + 
        geom_tile(colour="grey90") + 
        scale_fill_gradientn(name = "Quantity [tons]", 
                             colours = corvus_pal("sqGold")(8), 
                             values = c(0,0.001,0.1, 0.2, 0.4, 0.7,1),
                             breaks = Breaks$brks, 
                             na.value = "grey50")+
        theme_minimal() + theme_minimal() +
        labs(title = paste("Trade of", txt, "in 2016-2019 between\nselected European countries* and the 25 largest exporters (excluding European countries).", sep = " "),
             caption = captions,
             x = "Importers", y = "Exporters") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.text.x=element_text(angle=40, hjust=1),
              plot.title = element_text(size=17),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              strip.text = element_text(size=9, face = "bold"),
              legend.key.height = unit(2, "cm"),
              plot.caption = element_text(size = 10)) +
        scale_y_discrete(limits=(rev(top_30_exp$Exporters))) +
        scale_x_discrete(limits=(top_imp$Importer))
      
    }
  }
  
  output$heatplot3 <- renderPlot({
    heatplot3()
  })
  
  output$downloadPlot10 <- downloadHandler(
    filename = "heatplot3.png",
    content = function(file) {
      ggsave(file,heatplot3(), width = 12, height = 7)
    }
  )
  
  output$compare_product <- plotly::renderPlotly({
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    
    plot <- x %>%
      filter(Importer %in% input$importers) %>%
      group_by(commodity_code, HS_code_name) %>%
      dplyr::summarise(Quantity = sum(Quantity, na.rm = T)) %>%
      ggplot(data = ., mapping = aes(x = reorder(commodity_code, -Quantity), y= Quantity)) +
      geom_bar(aes(
        group = HS_code_name, 
        text = paste("Quantity:", Quantity)
      ),stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            plot.title = element_text(size=14),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10),
            axis.text=element_text(size=10),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            plot.caption = element_text(size = 8)) +
      labs(y = "Quantity [tons]", x = "Product code",
           caption = captions,
           title = "Import of selected groups of goods by the seleted European countries* in 2016-2019.")
    
    ggplotly(plot, tooltip = c("commodity_code", "HS_code_name", "text")) %>% 
      layout(margin = list(l = 50, r = 50, t = 60, b = 110),
             annotations = list(text = captions,
                                font =  list(size = 10),
                                showarrow = FALSE,
                                xref = 'paper', x = 1,
                                yref = 'paper', y = -0.45,
                                align = 'right'))
    
  })
  
  output$compare_product_by_region <- plotly::renderPlotly({
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    
    sum_all <- x %>% 
      filter(Importer %in% input$importers) %>%
      group_by(commodity_code) %>%
      dplyr::summarise(Sum_all = sum(Quantity, na.rm = T))
    
    gr_by_region <- x %>%
      filter(Importer %in% input$importers) %>%
      group_by(commodity_code, HS_code_name, Region) %>%
      dplyr::summarise(Sum = sum(Quantity, na.rm = T))
    
    x_3 <- sum_all %>% 
      right_join(gr_by_region, by = "commodity_code") %>%
      mutate(Quantity_share = (Sum/Sum_all) * 100)
    
    order <- x_3 %>%
      filter(Region == "Europe & Central Asia") %>%
      arrange(desc(Quantity_share))
    
    levels_region <- x %>%
      filter(Importer %in% input$importers) %>%
      group_by(Region) %>%
      dplyr::summarise(Sum = sum(Quantity, na.rm = T)) %>%
      arrange(desc(Sum)) 
    
    p <- x_3 %>%
      ggplot(data = ., mapping = aes(x = commodity_code, y= Quantity_share, fill = factor(Region, levels = levels_region$Region))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            plot.title = element_text(size=14),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10),
            axis.text=element_text(size=10),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            plot.caption = element_text(size = 8)) +
      labs(y = "Quantity [tons]", x = "Product code", fill = "Region",
           title = "Import of selected groups of goods by the seleted European countries* in 2016-2019 broken down by regions.")  +
      scale_x_discrete(limits=(order$commodity_code)) + 
      scale_fill_manual(values = corvus_pal("mixed2")(15)) + 
      geom_col(position = position_stack(reverse = T))
    
    ggplotly(p, tooltip = c("HS_code_name", "Quantity_share", "Region")) %>% 
      layout(margin = list(l = 50, r = 50, t = 60, b = 110),
             annotations = list(text = captions,
                                font =  list(size = 10),
                                showarrow = FALSE,
                                xref = 'paper', x = 1,
                                yref = 'paper', y = -0.45,
                                align = 'right'))
    
    
  })
  
  import_by_product_codes <- function(){
    importers <- paste(input$importers, collapse = ", ")
    captions <- paste("Data source: UN Comtrade\n *", importers, sep = "")
    
    txt <- text()
    x %>%
      filter(commodity_code %in% input$multipleCodes & Importer %in% input$importers) %>%
      group_by(commodity_code, Date) %>%
      dplyr::summarise(sum_code = sum(Quantity, na.rm = T)) %>%
      ggplot(data = ., aes(x = Date, y = sum_code, colour = commodity_code))+
      geom_line(size = 1, linetype="dotdash") + 
      geom_point(aes(color=commodity_code), size = 2.5) +
      xlab("") + 
      scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2019-12-01"), by="6 months"), date_labels = "%Y %b") +
      theme_minimal() +
      theme(plot.title = element_text(size=17),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10)) +
      labs(y = "Quantity [tons]", colour = "HS code",
           caption = captions,
           title = paste("Monthly quantity of", txt, "imported\nby the selected European countries* in 2016-2019 broken down by product codes.", sep = " ")) +
      scale_color_manual(values = corvus_pal("mixed2")(15)) 
    
    
  }
  
  output$import_by_product_codes <- renderPlot({
    import_by_product_codes()
  })
  
  output$downloadPlot11 <- downloadHandler(
    filename = "import_by_product_codes.png",
    content = function(file) {
      ggsave(file,import_by_product_codes(), width = 12, height = 7)
    }
  )
  
  observeEvent(input$source,{
    if(input$source %in% 'UN Comtrade') {
      
      updatePickerInput(session, "importers",
                        label = paste("Importer:"),
                        choices = comtrade_list,
                        selected = comtrade_list
      )
    } else if (input$source %in% 'Eurostat') {
      
      updatePickerInput(session, "importers",
                        label = paste("Importer:"),
                        choices = eurostat_list,
                        selected = eurostat_list
      )
    } else {
      updatePickerInput(session, "importers",
                        label = paste("select source"),
                        choices = "",
                        selected = ""
      )
    }
    
  })
  
  # Write text input to dropbox
  write_text_content <- function(arg) {
    button_id <- paste("save", arg, sep = "")
    observeEvent(input[[button_id]], {
      
      editorText(session, editorid = paste("textcontent", arg, sep = ""), outputid = 'mytext')
      
      generateText <- function(){
        req(input$mytext)
        enc2utf8(input$mytext)
      }
      
      file_name <- paste("overview", arg, ".txt", sep = "")
      write(generateText(), file_name)
      drop_upload(file_name, path = "shiny_app", dtoken = token, mode = "overwrite")
      
    })
  }
  
  write_text_content("1")
  write_text_content("2")
  write_text_content("3")
  write_text_content("4")
  write_text_content("5")
  write_text_content("6")
  write_text_content("7")
  write_text_content("8")
  write_text_content("9")
  write_text_content("10")
  write_text_content("11")
  write_text_content("12")
  write_text_content("13")
  write_text_content("14")
  write_text_content("15")
}

shinyApp(ui, server)

