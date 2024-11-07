
###############################################################################
# data-manipulation functions
###############################################################################
get_numerator_data <- function(dt, selectedService, 
                               selectedVar4num, 
                               group,
                               selectedYears){
  dt |> 
    dplyr::filter(Service == selectedService | Service =="census")   |>
    dplyr::filter(Variable==selectedVar4num)   |>
    dplyr::filter(Municipality %in% group) |> 
    dplyr::arrange(Municipality, Year) |>
    dplyr::spread(key=Year, value=Value)       |>
    dplyr::select(tidyselect::all_of(selectedYears)) |> 
    as.matrix()
  
  
}

get_bd_matrix_data <- function(dt, selectedService, 
                               selectedVar4num, 
                               group,
                               selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4num)   |>
    filter(Municipality %in% group) |> 
    arrange(Municipality, Year) |>
    spread(key=Year, value=Value)       |>
    select(tidyselect::all_of(selectedYears)) |> 
    as.matrix()
  
  
}


# -----------------------------------------------------------------------------
# Graph-rendering-related settings 
# -----------------------------------------------------------------------------
# 
# custom palettes
# This is a static approach, i.e., colors are pre-assigned to all participating 
# cities and these city-color pairs are fixed 
# For more realistic settings, the palettes must be dynamically subset
#pairedPalette <- RColorBrewer::brewer.pal(n=length(citylabel), name="Paired")
#print("---------------------------------------------------pairedPalette=")
# print(pairedPalette)
# lvlcl <- levels(factor(citylabel, ordered = T))
# print("levelsCityLabel=")
# print(lvlcl)
# names(pairedPalette) <- lvlcl
# print("pairedPalette=")
# print(pairedPalette)
# print(str(pairedPalette))


# shapeNoList <- seq(1:length(citylabel))
# names(shapeNoList)  <- lvlcl
# print("shapeNoList=")
# print(shapeNoList)


# for bar/column plot
#fixed_f_scale <- ggplot2::scale_fill_manual(name="Legend", values=pairedPalette)
# for line and point
#fixed_c_scale <- ggplot2::scale_color_manual(name="Legend", values = pairedPalette)
# for shapes
#fixed_s_scale <- ggplot2::scale_shape_manual(name="Legend", values = shapeNoList)


shape_no_list <- function(target_name, peer_names, average=FALSE, systemAvg=FALSE){
  
  pp <- c()
  base <-c(19)
  mpty_palette <- c()
  all_names <-c()
  if (average){
    pp <- c(17)
    mpty_palette <- c(base, pp)
    all_names <- if (systemAvg) c(target_name, "System Average") else c(target_name, "Average")
    names(mpty_palette) <- all_names
  } else if (is.null(peer_names)) {
    # target only: 19 only
    mpty_palette <- base
    all_names <- c(target_name)
    names(mpty_palette) <- all_names
    
    
    # } else if (length(peer_names) < ) {
    #   # 
    #   pp <- RColorBrewer::brewer.pal(n=3, name="Paired")
    #   pp <- pp[1:length(peer_names)]
    #   mpty_palette <- c(base, pp)
    #   all_names <- c(target_name, peer_names)
    #   names(mpty_palette) <- all_names
  } else {
    # maximum : 1 + 5 = 6 shapes
    # target: c(19)
    # peers(5): c(15[sq], 17[triangle], 18[diamond], 25[triangle], 4 [x])
    pp <- c(15, 17, 18, 25, 6)
    mpty_palette <- c(base, pp)
    all_names <- c(target_name, peer_names)
    names(mpty_palette) <- all_names
  }
  
  mpty_palette  
}


color_palette_mpty_indv <- function(target_name, peer_names, average=FALSE, systemAvg=FALSE){
  pp <- c()
  base <-c("#000000")
  mpty_palette <- c()
  all_names <-c()
  
  if (average){
    mpty_palette <-c("#000000", "#7BAFD4")
    all_names <- if (systemAvg) c(target_name, "System Average") else  c(target_name, "Average")
    
    
    
    names(mpty_palette) <- all_names
    
  } else if (is.null(peer_names)) {
    
    
    
    mpty_palette <- base
    all_names <- c(target_name)
    
    
    names(mpty_palette) <- all_names
    
  } else if (length(peer_names) < 3) {
    # if (average){
    #   print("average-1 case")
    #   mpty_palette <-c("#000000", "#7BAFD4")
    #   all_names <- c(target_name, "Average")
    # } else {
    pp <- RColorBrewer::brewer.pal(n=3, name="Paired")
    pp <- pp[1:length(peer_names)]
    mpty_palette <- c(base, pp)
    all_names <- c(target_name, peer_names)
    #}
    
    
    names(mpty_palette) <- all_names
  } else {
    
    # if (average){
    #   print("average-2 case")
    #   mpty_palette <-c("#000000", "#7BAFD4")
    #   all_names <- c(target_name, "Average")
    # } else {
    pp <- RColorBrewer::brewer.pal(n=length(peer_names), name="Paired")
    mpty_palette <- c(base, pp)
    all_names <- c(target_name, peer_names)
    # }
    
    names(mpty_palette) <- all_names
  }
  
  mpty_palette
}
# color_palette_mpty_avg <- function(){
#   pp <- RColorBrewer::brewer.pal(n=length(mpty_names), name="Paired")
#   base <-c("#000000", "#7BAFD4")
#   mpty_palette <- c(base, pp)
#   
# }

# default page layout
paperWidth <- 11
paerHeight <- 8.5



# ------------------------------------------------------------------------------

createTextualTable <- function(dt, denominator=FALSE){
  if (denominator) {
    # denominator available
    
    ggpubr::ggtexttable(dt, rows = NULL,
                        
                        theme = ttheme(base_style="blank", 
                                       tbody.style = tbody_style(size=9, hjust=0, x=0.01, fill = NA),
                                       colnames.style = colnames_style(size=9, hjust=0, x=0.01, fill = NA))) |>
      ggpubr::tab_add_hline(at.row = c(1, 2), 
                            row.side = "top", linewidth = 2) |>
      ggpubr::tab_add_hline(at.row = c(3), 
                            row.side = "bottom", linewidth = 2)
    
    
  } else {
    # no denominator
    ggpubr::ggtexttable(dt, rows = NULL,
                        theme = ttheme(base_style="blank", 
                                       tbody.style = tbody_style(size=9, hjust=0, x=0.01, fill = NA),
                                       colnames.style = colnames_style(siz=9, hjust=0, x=0.01, fill = NA))) |>
      ggpubr::tab_add_hline(at.row = c(1, 2), 
                            row.side = "top", linewidth = 2) |>
      ggpubr::tab_add_hline(at.row = c(3), 
                            row.side = "top", linewidth = 2)
    
  }
  
  
  
}

# -----------------------------------------------------------------------------
# Theme for gtable
# -----------------------------------------------------------------------------
tt1 <- gridExtra::ttheme_default(
  base_family = "barlow",
  core = list(fg_params = list(
    hjust = 0,
    x = 0.01,
    fontsize = 10
  )),
  colhead = list(fg_params = list(
    hjust = 0,
    x = 0.01,
    fontface = "bold"
  ))
)

# -----------------------------------------------------------------------------
# custom Google-font setting
# -----------------------------------------------------------------------------
# The following setting is expected to download font files from a remote site
# sysfonts::font_add_google(name = "Barlow Semi Condensed",family =  "barlow")

# This setting loads locally saved ttf files in www/fonts 
sysfonts::font_add(family = "barlow", 
                   regular = "www/fonts/barlow-semi-condensed-v14-latin-regular.ttf", 
                   italic = "www/fonts/barlow-semi-condensed-v14-latin-italic.ttf")

# the above setting was supposed to be replaced with a new css setting with 
# gfonts::includeCSS() in app.R; see the manual of an R package, gfonts, esp.,
# setup_font() function; however, this approach did not work with ggplot2 
# for some # unknown reason (naming?) and was abandoned 
