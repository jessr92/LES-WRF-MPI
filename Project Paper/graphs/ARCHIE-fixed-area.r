data <- read.table("ARCHIE-fixed-area.input", header=T, sep="\t")

y_range = range(20, data$runtime)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="ARCHIE-fixed-area.png", height=350, width=500, bg="white")

plot(data$processes, data$runtime, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50, 100), col="grey10", lty="dotted")
abline(h=c(10, 20, 50, 100, 200), col="grey10", lty="dotted")

title(main="ARCHIE Fixed Area")
title(xlab="Process count")
title(ylab="Runtime (s)")

box()
