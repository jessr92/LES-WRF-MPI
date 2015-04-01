data <- read.table("MPICH313-OpenMPI184-expanding-area.input", header=T, sep="\t")

y_range = range(50, data$mpich, data$openmpi)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="MPICH313-OpenMPI184-expanding-area.png", height=400, width=500, bg="white")

plot(data$processes, data$mpich, log="xy",type="o", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$openmpi, type="o", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(50, 100, 150, 200), col="grey10", lty="dotted")

legend("bottomright", legend = c("MPICH 3.1.3", "OpenMPI 1.8.4"), fill=plot_colours)

title(main="MPICH 3.1.3 versus OpenMPI 1.8.4 Expanding Area")
title(xlab="Process count")
title(ylab="Runtime (s)")

box()
