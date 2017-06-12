# ---------------------------------------------------------------------
# Program: DifferentialEQs1.R
#  Author: Steve Boker
#    Date: Sun Feb 1 12:21:17 EST 2015
#
#
# ---------------------------------------------------------------------
# Revision History
#   Steve Boker -- Sun Feb 1 12:21:21 EST 2015
#      Create DifferentialEQs1.R.
#
# ---------------------------------------------------------------------


# ----------------------------------
# Read libraries and set options.

options(width=80)

library(deSolve)

# ----------------------------------
# Set constants.

totalSamples <- 30
totalInterval <- 30
deltaT <- totalInterval / totalSamples

theTimes  <- seq(0, totalInterval, length=totalSamples)  # the measurement occasions

# ----------------------------------
# Define the damped linear oscillator model.
#   Note that y(t) = dx(t)/dt as in the Mathematica model
#   Thus d^2x(t)/dt = eta * x(t) + zeta * dx(t)/dt

DLOmodel <- function(t, prevState, parms) {
    x <- prevState[1] # x[t]
    y <- prevState[2] # dx[t]

    with(as.list(parms), {
            dx <- y
            dy <- parms[1]*x + parms[2]*y
            res<-c(dx,dy)
            list(res)
            }
        )
}

# ----------------------------------
# Simulate and plot one oscillator.

eta <- -0.3
zeta <- -0.05
xstart <- c(x = 1, y = 0)

out1 <- as.data.frame(lsoda(xstart, theTimes, DLOmodel, parms=c(eta, zeta)))

pdf("DLOExample0.pdf", height=5, width=6)
plot(c(min(theTimes), max(theTimes)), c(-1, 1),
     xlab="Time",
     ylab="X",
     type='n')
lines(out1$time, out1$x, type='p', lwd=2, col=2)
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', lty=2, col=1)
dev.off()

pdf("DLOExample0-x-dx.pdf", height=5, width=5)
plot(c(-1, 1), c(-1, 1),
     xlab="X",
     ylab="dX/dt",
     type='n')
lines(out1$x, out1$y, type='l', lwd=2, col=2)
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', lty=2, col=1)
dev.off()



# ----------------------------------
# Write a univariate data vector.

write(t(cbind(out1$x, out1$y)), file="DLOExample0.dat", ncolumns=2, append=FALSE)


# ----------------------------------
# Quit here.
#
# q()

