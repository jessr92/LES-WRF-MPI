data <- read.table("GMCF-before-after-fixed-area.input", header=T, sep="\t")

y_range = range(50, data$before, data$after)
x_range = range(data$processes)

plot_colours <- c("blue", "red")

png(filename="GMCF-before-after-fixed-area.png", height=400, width=500, bg="white")

plot(data$processes, data$before, log="xy",type="l", col=plot_colours[1], ylim=y_range, xlim=x_range, axes=TRUE, ann=FALSE)

lines(data$processes, data$after, type="l", col=plot_colours[2], ylim=y_range, xlim=x_range, ann=FALSE)

abline(v=c(1, 2, 5, 10, 20, 50), col="grey10", lty="dotted")
abline(h=c(50, 100, 150, 200, 250, 300, 350), col="grey10", lty="dotted")

legend("right", legend = c("GMCF Original", "GMCF Modified"), fill=plot_colours)

title(main="GMCF Performance Enhancements")
title(xlab="Instance count")
title(ylab="Runtime (s)")

box()
