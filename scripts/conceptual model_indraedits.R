## conceptual model
source('scripts/lfm_functions.R')
spres <- read.csv('results/species-results.csv')
head(spres)
slp <- -0.198

#MWPvec <- seq(-6,0,length.out=25)
ilfm.sat.vec <- seq(0.25,1.25,length.out=25)
MWPcrit <- c()
for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/0.7)
lfm.sat.vec <- 1/ilfm.sat.vec
plot(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),type='l',lwd=2,xlab='Minimum MWP (MPa)',ylab='LFM @ MWP=0')

for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/0.5)
lfm.sat.vec <- 1/ilfm.sat.vec
lines(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),lwd=1)

for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/1)
lfm.sat.vec <- 1/ilfm.sat.vec
lines(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),lwd=1)

#points(I(1/model1.int)~minWP,data=spres[which(spres$study=='Coastal'),],pch=19)
text(I(1/model1.int)~minWP,data=spres[which(spres$study=='Coastal'),],labels=letters)

# conceptual figure, in inverted space - need to flip to LFM# conceptual figulettersre, in inverted space - need to flip to LFM
lfm.sat <- c(0.9,0.5)
slp <- -0.2
min.MWP <- c(-6,-3)

xy <- inversePlotData(c(min.MWP[1],0),c(lfm.sat[1],slp))
head(xy)
plot(xy$MWP,xy$LFM)

(MWPcrit <- solveForX(c(lfm.sat[2],slp),1/0.7))
#------
(max.iLFM <- lfm.sat + min.MWP*slp)
(min.LFM <- 1/max.iLFM)

op=par(mfrow=c(1,2))
plot(c(-7,0),c(0,3),type='n',xlab='MWP',ylab='1/LFM')
x <- list()
y <- list()
for (i in 1:2) {
  x[[i]] <- c(0,min.MWP[i])
  y[[i]] <- c(lfm.sat[i],max.iLFM[i])
  lines(x[[i]],y[[i]],type='b',cex=2,pch=c(1,19))
}

xy1 <- inversePlotData(c(0,min.MWP[1]),c(lfm.sat[1],slp))
xy2 <- inversePlotData(c(0,min.MWP[2]),c(lfm.sat[2],slp))
plot(c(-7,0),c(0,3),type='n',xlab='MWP',ylab='LFM')
lines(xy1$MWP,xy1$LFM,type='l')
head(xy1)
points(xy1[c(1,100),c('MWP','LFM')],cex=2,pch=c(1,19))
lines(xy2$MWP,xy2$LFM,type='l')
points(xy2[c(1,100),c('MWP','LFM')],cex=2,pch=c(1,19))
par(op)

#-----
# Choose distinct colors for each line
cols <- c("steelblue3", "tomato3")  # i=1 -> cols[1], i=2 -> cols[2]

xy <- inversePlotData(c(min.MWP[1], 0), c(lfm.sat[1], slp))
plot(xy$MWP, xy$LFM)

(MWPcrit <- solveForX(c(lfm.sat[2], slp), 1/0.7))

(max.iLFM <- lfm.sat + min.MWP * slp)
(min.LFM  <- 1 / max.iLFM)

op <- par(mfrow = c(1, 2))

## Panel 1: 1/LFM vs MWP
plot(c(-7, 0), c(0, 3), type = "n", xlab = "MWP", ylab = "1/LFM")
mtext("A", side = 3, adj = 0, line = 1.2, font = 2, cex = 1.2)  # Panel label
x <- vector("list", 2)
y <- vector("list", 2)
for (i in 1:2) {
  x[[i]] <- c(0, min.MWP[i])
  y[[i]] <- c(lfm.sat[i], max.iLFM[i])
  # open circle for first point, filled circle for second
  lines(x[[i]], y[[i]], col = cols[i], lwd = 2)
  points(x[[i]][1], y[[i]][1], pch = 21, bg = "white", col = cols[i], cex = 2, lwd = 2)
  points(x[[i]][2], y[[i]][2], pch = 19, col = cols[i], cex = 2)
}

## Panel 2: LFM vs MWP
xy1 <- inversePlotData(c(0, min.MWP[1]), c(lfm.sat[1], slp))
xy2 <- inversePlotData(c(0, min.MWP[2]), c(lfm.sat[2], slp))
plot(c(-7, 0), c(0, 3), type = "n", xlab = "MWP", ylab = "LFM")
mtext("B", side = 3, adj = 0, line = 1.2, font = 2, cex = 1.2)  # Panel label

lines(xy1$MWP, xy1$LFM, col = cols[1], lwd = 2)
points(xy1[1, c("MWP", "LFM")], pch = 21, bg = "white", col = cols[1], cex = 2, lwd = 2)
points(xy1[100, c("MWP", "LFM")], pch = 19, col = cols[1], cex = 2)

lines(xy2$MWP, xy2$LFM, col = cols[2], lwd = 2)
points(xy2[1, c("MWP", "LFM")], pch = 21, bg = "white", col = cols[2], cex = 2, lwd = 2)
points(xy2[100, c("MWP", "LFM")], pch = 19, col = cols[2], cex = 2)

par(op)

#----
library(dplyr)
library(ggplot2)
library(patchwork)

# Colors mapped consistently across both panels
cols <- c("steelblue3", "tomato3")
names(cols) <- c("1","2")  # map to id "1" and "2"

# --- Data prep ---------------------------------------------------------------

# Panel A (1/LFM vs MWP): two straight line segments with endpoints
max.iLFM <- lfm.sat + min.MWP * slp

dfA_lines <- tibble(
  id = factor(c(1,1, 2,2)),
  MWP = c(0, min.MWP[1], 0, min.MWP[2]),
  invLFM = c(lfm.sat[1], max.iLFM[1], lfm.sat[2], max.iLFM[2])
)

dfA_open <- dfA_lines %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup()

dfA_filled <- dfA_lines %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Panel B (LFM vs MWP): two curves from inversePlotData
xy1 <- inversePlotData(c(0, min.MWP[1]), c(lfm.sat[1], slp)) %>% mutate(id = factor(1))
xy2 <- inversePlotData(c(0, min.MWP[2]), c(lfm.sat[2], slp)) %>% mutate(id = factor(2))
dfB_curve <- bind_rows(xy1, xy2)

dfB_open <- dfB_curve %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup()

dfB_filled <- dfB_curve %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup()

# --- Plots -------------------------------------------------------------------

# Panel A: 1/LFM vs MWP
pA <- ggplot() +
  geom_line(data = dfA_lines,
            aes(x = MWP, y = invLFM, group = id, color = id), linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_point(data = dfA_open,
             aes(x = MWP, y = invLFM, color = id),
             shape = 21, fill = "white", size = 3.5, stroke = 1.2) +
  geom_point(data = dfA_filled,
             aes(x = MWP, y = invLFM, color = id),
             shape = 19, size = 3.5) +
  annotate("text", x = -7, y = 3, label = "A", fontface = "bold",
           hjust = 0, vjust = 1, size = 5) +
  coord_cartesian(xlim = c(-7, 0), ylim = c(0, 3)) +
  scale_color_manual(values = cols, guide = "none") +
  labs(x = "Midday Water Potential (MPa)", y = "1/LFM (%)")

# Panel B: LFM vs MWP
pB <- ggplot(dfB_curve, aes(x = MWP, y = LFM, color = id)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_point(data = dfB_open,
             aes(x = MWP, y = LFM),
             shape = 21, fill = "white", size = 3.5, stroke = 1.2) +
  geom_point(data = dfB_filled,
             aes(x = MWP, y = LFM),
             shape = 19, size = 3.5) +
  annotate("text", x = -7, y = 3, label = "B", fontface = "bold",
           hjust = 0, vjust = 1, size = 5) +
  coord_cartesian(xlim = c(-7, 0), ylim = c(0, 3)) +
  scale_color_manual(values = cols, guide = "none") +
  labs(x = "Midday Water Potential (MPa)", y = "LFM (%)")

# Side-by-side layout (no legend)
pA + pB + plot_layout(ncol = 2)

# Create the combined plot
final_plot <- pA + pB + plot_layout(ncol = 2, 
                                    axis_titles = "collect")
final_plot

# Save it
ggsave(
  filename = "MWP_LFM_panels.png",  # or .pdf, .jpg, etc.
  plot     = final_plot,
  width    = 8,     # inches
  height   = 4,     # inches
  dpi      = 300    # high resolution
)
#----

