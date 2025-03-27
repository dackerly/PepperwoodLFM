
#custom ggplot: 
ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank(),# Hide minor gridlines
          strip.background = element_rect(fill = "white"))  # Make background white
}

theme_minimal <- theme_minimal() + 
  theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
        panel.grid.major = element_blank(),  # Hide major gridlines
        panel.grid.minor = element_blank(),# Hide minor gridlines
        strip.background = element_rect(fill = "white")) 


# #Change overall colors and plotting information so we don't have to do it every time :)
# cal_palettes <- list(
#   ### release 1: June 2020
#   sierra1 = c("#BD973D", "#5F5C29", "#3B7D6E", "#5792CC", "#4D5B75", "#262E43"),
#   sierra2 = c("#FDD989", "#8BAD57", "#516238", "#4CA2B0", "#5A8B92", "#395B5F"),
#   chaparral1 = c("#DCC27A", "#B0B9BE", "#63605F", "#985E5C", "#AEBFA8", "#F19B34"),
#   chaparral2 = c("#D98A63", "#D9E4DC", "#C5D2D2", "#79B38F", "#9A9B5F", "#A7C2CD"),
#   chaparral3 = c("#D3E3CA", "#BED6B3", "#92A587", "#4A5438", "#2F3525"),
#   conifer = c("#CC7540", "#765043", "#A69260", "#979A6B", "#39692F"),
#   desert = c("#F6EECF", "#ECD6AB", "#B09175", "#632D1F", "#291611"),
#   wetland = c("#DED4C8", "#AD6F4F", "#AEC96F", "#2B3851", "#3F320D"),
#   oak = c("#EFC68E", "#B58755", "#7C9867", "#4F5730","#7A5028"),
#   kelp1 = c("#C70000", "#FFBF00", "#BE8333", "#54662C", "#009BB0", "#114C54"),
#   kelp2 = c("#0FB2D3", "#026779", "#368000", "#3D6334", "#6D5A18"),
#   coastaldune1 = c("#DCC8BA", "#DCD6C5", "#B4AA98", "#D7DCE4", "#444239"),
#   coastaldune2 = c("#E2D78A", "#E4B3E2", "#90816E", "#523833", "#372E21"),
#   superbloom1 = c("#B9C7E2", "#ECAB99", "#F1C100",  "#5B6530", "#9484B1"),
#   superbloom2 = c("#DE7424", "#F5CA37", "#AD8D26", "#496849", "#654783"),
#   superbloom3 = c("#E69512", "#D3105C", "#3B4F8E", "#3A5D3D", "#4C4976", "#6C91BD"),
#   sbchannel = c("#A1CAF6", "#6592D6", "#4C6FA1", "#375377", "#1E2F46"),
#   lake = c("#CECEB9", "#7AC9B7", "#6CA184", "#3793EC", "#2A3927"),
#   fire = c("#B77B7B", "#FEEC44", "#F66C09", "#E60505", "#2C1B21"),
#   agriculture = c("#A45C44", "#5A7F3C", "#CACA91", "#2C3B26", "#88B063"),
#   bigsur = c("#E4DECE", "#ECBD95", "#9BB1BB", "#79ACBD", "#346575", "#0B4221"),
#   figmtn = c("#E29244", "#FFAA00", "#D46F10", "#4CA49E", "#69B9FA", "#59A3F8", "#4B8FF7", "#5A7ECB", "#6B6D9F"),
#   
#   ### release 2: Sep 2020
#   caqu = c("#E6DECC", "#F3E3C2", "#8F96A6", "#625D55", "#501F16"), # california quail
#   eschscholzia = c("#F2B705", "#F29F05", "#F28705", "#D95204", "#A62F03"), # california poppy
#   arbutus = c("#DFE3CE", "#B5C861", "#8AA789", "#CB8573", "#976153"), # pacific madrone
#   calochortus = c("#CAC8CF", "#C9B3B5", "#8F706E", "#AF6E78", "#5C3327"), # catalinae
#   grassdry = c("#E1BC8D", "#845B3E", "#5B4E23", "#35301C", "#4C5454"),
#   grasswet = c("#4C4E32","#908E6C","#5D8FBC","#97C2E2","#17252A","#B4A480"),
#   sage = c("#607860", "#304830", "#C0D8F0", "#909078", "#181818"),
#   tidepool = c("#84A6A2","#4A5352","#151E2F","#D7C8C6","#BE5A47","#604A76"),
#   seagrass = c("#5A870A", "#BDD0A2", "#555B53", "#6A4D3B", "#BEAB91", "#8F9BAB"),
#   bigsur2 = c("#20618D", "#91AAC4", "#6B6C58", "#464724", "#83932D", "#CAB89F"),
#   bixby = c("#286A81", "#045CB4", "#7F6F43", "#748B75", "#B8B196"),
#   redwood1 = c("#303018", "#604830", "#609048", "#90A860", "#786048"),
#   redwood2 = c("#304818", "#906030", "#486030", "#784830", "#181800"),
#   halfdome = c("#A2A098", "#5E6B7B", "#233D3F", "#85ADCC", "#426714"),
#   creek = c("#EBDAC9", "#CEAD96", "#CECFD4", "#686F60", "#455D44", "#23341E"),
#   vermillion = c("#c39ca4","#e05959","#ac181d","#713d3f","#381f21"), # vermillion rockfish
#   canary = c("#FFDBA5", "#FAB455", "#F28023", "#A5683C", "#B4450E"), # canary rockfish
#   casj = c("#336887", "#8197A4", "#A9B4BC", "#B7AA9F", "#706A6B"),
#   lupinus = c("#6C568C", "#9386A6", "#BFCDD9", "#7F8C72", "#607345"),
#   dudleya = c("#7E8C69", "#E7A655", "#E59D7F", "#E38377", "#6D4847"),
#   gayophytum = c("#AA767C", "#B7AF57", "#797014", "#C2607F", "#A65644"),
#   collinsia = c("#9E8ABC", "#A99CD9", "#808C91", "#A7907B", "#A5BA92"),
#   buow = c("#DED4CB", "#DBE38E", "#7E7576", "#A79787", "#3A2C21") # burrowing owl
# )


#color_fill <- scale_fill_manual(values = cal_palette("sierra1"))
color_fill <- scale_fill_manual(values = c("#B9C7E2", "#ECAB99",  "#5B6530", "#9484B1",  "#F1C100"))

color_fill_all <- scale_fill_manual(values = c("#B9C7E2", "#ECAB99",  "#5B6530", "#9484B1",  "#F1C100"))

color_fill_site <- scale_fill_manual(values = c("#B9C7E2", "#ECAB99",  "#5B6530", "#9484B1",  "#F1C100"))


#color <- scale_color_manual(values = cal_palette("sierra1"))
color <- scale_color_manual(values = c( "#ECAB99",  "#5B6530", "#9484B1",  "#F1C100"))

color_line_size <- scale_color_manual(values = c("#004f63", "#62929a"
))

color_line_site <- scale_color_manual(values = c("#c969a1", "#ce4441", "#ee8577", "#eb7926", "#ffbb44", "#859b6c", "#62929a", "#004f63", "#122456"))

th <- theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
            panel.grid.major = element_blank(),  # Hide major gridlines
            panel.grid.minor = element_blank())


#Hexcode for Cross color palette
Cross = list(c("#c969a1", "#ce4441", "#ee8577", "#eb7926", "#ffbb44", "#859b6c", "#62929a", "#004f63", "#122456"))


#Other color palette codes here: https://github.com/BlakeRMills/MetBrewer/blob/main/R/PaletteCode.R 