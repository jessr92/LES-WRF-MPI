data <- read.table("MPICH313-expanding-area.input", header=T, sep="\t")

y_range = range(100, data$default, data$bycore)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="MPICH313-expanding-area.png", height=400, width=500, bg="white")

plot(data$processes, data$default, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$bycore, type="l", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(100, 150, 200), col="grey10", lty="dotted")

legend("bottomright", legend = c("Default mapping", "By core mapping"), fill=plot_colours)

title(main="MPICH 3.1.3 Expanding Area")
title(xlab="Process count")
title(ylab="Runtime (s)")

box()
