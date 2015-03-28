data <- read.table("GMCF-MPI-expanding-area.input", header=T, sep="\t")

y_range = range(50, data$mpi, data$gmcf, na.rm=TRUE)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="GMCF-MPI-expanding-area.png", height=400, width=500, bg="white")

plot(data$processes, data$mpi, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$gmcf, type="l", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(50, 100, 150, 200), col="grey10", lty="dotted")

legend("bottomright", legend = c("MPI", "GMCF"), fill=plot_colours)

title(main="MPI versus GMCF Expanding Area")
title(xlab="Instance count")
title(ylab="Runtime (s)")

box()
