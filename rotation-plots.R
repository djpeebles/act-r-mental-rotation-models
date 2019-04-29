packages <- c("here","Hmisc")

for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos="https://www.stats.bris.ac.uk/R/")
        library(package, character.only=TRUE)
    }
}

dat <- read.csv(here("rotation-output.csv"), header=TRUE)

pdf(here("rotation-data-humans.pdf"), width=8.0, height=8.0, onefile=FALSE, paper="special")
plot(dat$Xvariable, dat$Humans, type = "b", xlim = c(0,180), ylim = c(0.5,8.0), pch=16, cex=1.0, col="black", xaxt="n", yaxt="n", xlab = "Angular Difference in Orientation (degrees)", ylab = "Response Time (s)", cex.axis=1.3, cex.lab=1.3)
axis(1, at = seq(0, 180, by = 20))
axis(2, at = seq(0.0, 8.0, by = 1.0))
minor.tick(ny=4, nx=0, tick.ratio=0.5)
lm_mod <- lm(dat$Humans ~ dat$Xvariable)
abline(lm_mod,col="red")
lm_coef <- round(coef(lm_mod), 3) # extract coefficients
text(162, 0.5, bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), cex=1.2)
dev.off()

pdf(here("rotation-data-model.pdf"), width=8.0, height=8.0, onefile=FALSE, paper="special")
plot(dat$Xvariable, dat$Model, type = "b", xlim = c(0,180), ylim = c(0.5,8.0), pch=16, cex=1.0, col="black", xaxt="n", yaxt="n", xlab = "Angular Difference in Orientation (degrees)", ylab = "Response Time (s)", cex.axis=1.3, cex.lab=1.3)
axis(1, at = seq(0, 180, by = 20))
axis(2, at = seq(0.0, 8.0, by = 1.0))
minor.tick(ny=4, nx=0, tick.ratio=0.5)
lm_mod <- lm(dat$Model ~ dat$Xvariable)
abline(lm_mod,col="red")
lm_coef <- round(coef(lm_mod), 3) # extract coefficients
text(162, 0.5, bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), cex=1.2)
dev.off()
