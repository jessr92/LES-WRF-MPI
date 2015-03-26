data <- read.table("GMCF-MPI-fixed-area.input", header=T, sep="\t")

y_range = range(20, data$mpi, data$gmcf)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="GMCF-MPI-fixed-area.png", height=400, width=500, bg="white")

plot(data$processes, data$mpi, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$gmcf, type="l", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(20, 50, 100, 200), col="grey10", lty="dotted")

legend("topright", legend = c("MPI", "GMCF"), fill=plot_colours)

title(main="MPI versus GMCF Fixed Area")
title(xlab="Instance count")
title(ylab="Runtime (s)")

box()
