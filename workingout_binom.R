# Binomial messing

num <- c(12, 30, 26, 50, 42, 71, 4, 29, 48, 36)
denom <- c(82, 98, 100, 125, 162, 151, 12, 100, 82, 99)
p <- num / denom
pbar<- sum(num)/sum(denom)


x<- seq(175) # Arbitrary 175 points for drawing line
# Upper and lower normal approximation using std. error of proportion
uci <- pbar + 1.959964*sqrt((pbar*(1-pbar)) / x)
lci <- pbar - 1.959964*sqrt((pbar*(1-pbar)) / x)


# Work out the overdispersion element 
# Use arcsin sqrt as per CQC and Spieglehalter

Target_transformed<- asin(sqrt(pbar))
n <- length(p)
Y <- asin(sqrt(p))
s  <- 1 / (2 * sqrt(denom))

# Unadjusted z-score
z <- (Y-Target_transformed)/s

# No winsorization this time, straigh to phi as life is too short.
phi <- (1 / n) * sum(z^2)

# Calculate tau2
Tau2 <- max(0, ((sum(n) * sum(phi)) - (sum(n) - 1)) /
              (sum(1/(s^2)) - (sum((1/(s^2))^2) / sum(1/(s^2)))))


# If I naively use the transformed Tau2 on original scale
uci2 <- pbar + 1.959964*sqrt(((pbar*(1-pbar)) / x)+Tau2)
lci2 <- pbar - 1.959964*sqrt(((pbar*(1-pbar)) / x)+Tau2)


# Now is I try calculate on scale, it works for larger numbers, but small go odd.
ci_up <- sin(Target_transformed + 1.959964 * sqrt(((1/(2 * sqrt(x)))^2 +Tau2)))^2
ci_low <- sin(Target_transformed - 1.959964 * sqrt(((1/(2 * sqrt(x)))^2 +Tau2)))^2
ci_up2 <- sin(Target_transformed + 3.090232 * sqrt(((1/(2 * sqrt(x)))^2 +Tau2)))^2
ci_low2 <- sin(Target_transformed - 3.090232 * sqrt(((1/(2 * sqrt(x)))^2 +Tau2)))^2


# Put in data.frame as it's easier for ggplot
lims2 <- data.frame(pbar, uci, lci, x, uci2, lci2, ci_up, ci_low, ci_up2, ci_low2)

# Draw plot
library(ggplot2)

ggplot(lims, aes(x=x))+
  geom_line(aes(y=uci))+  # Normal approx using binom std. error in black
  geom_line(aes(y=lci))+  # Normal approx using binom std. error in black
  geom_line(aes(y=pbar), linetype="dashed")+  # Centre/target line
  geom_line(aes(y=uci2), col="red")+ # Normal approx + unadjusted phi
  geom_line(aes(y=lci2), col="red")+ # Normal approx + unadjusted phi
  geom_line(aes(y=ci_up), col="blue")+ # Adjusted scaled back transformed 95%
  geom_line(aes(y=ci_low), col="blue")+ # Adjusted scaled back transformed 95%
  geom_line(aes(y=ci_up2), col="green3")+ # Adjusted scaled back transformed 95%
  geom_line(aes(y=ci_low2), col="green3")+ # Adjusted scaled back transformed 99.8%
  geom_point(aes(y=p,x=denom), data=data.frame(p, denom, num))+
  theme_minimal()

