data <- read.table("MPICH313-fixed-area.input", header=T, sep="\t")

par(ps=22)

y_range = range(20, data$default, data$bycore)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="MPICH313-fixed-area.png", height=350, width=500, bg="white")

plot(data$processes, data$default, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$bycore, type="l", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(20, 50, 100, 200), col="grey10", lty="dotted")

legend("topright", legend = c("Default mapping", "By core mapping"), fill=plot_colours)

title(main="MPICH 3.1.3 Fixed Area")
title(xlab="Process count")
title(ylab="Runtime (s)")

box()
