# Rainhard Findling
# u'smile Lab - University of Applied Sciences Upper Austria
# 2014/06
# 
# Demo of using residuum to derotate two acceleration timeseries before comparing them. 
# Besides derotation this script gives detail insight to magnitude comparison as well
# as comparision of derotated, individual axes of both timeseries, including multiple
# figures and statistics of multiple well known similarity measures.

library('dtw')
library('stats')

source('residuum.R')

# results folder
dir.create('results')
bg <- 'white' # white, transparent
width <- 5
height <- 4

# load multiple datasets
d <- lapply(c('../../demo_data/X1.csv', '../../demo_data/Y1.csv'), FUN=function(f)read.csv(f, header=F, sep=' ')[1:200,])

# normalize
d <- lapply(d, scale, scale=F)
# magnitude
magnitudes <- lapply(d, function(x){sqrt(x[,1]**2+x[,2]**2+x[,3]**2)})

# compare magnitude
svg('results/magnitudes.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(magnitudes), type='l', ylab='Acceleration magnitude [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend)
dev.off()
# compare axes
svg('results/axis1_no_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,1])), type='l', ylab='Acceleration axis 1 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_no_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,2])), type='l', ylab='Acceleration axis 2 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_no_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,3])), type='l', ylab='Acceleration axis 3 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()

# derotate axes
derotated <- residuum(d[[1]],d[[2]])

# compare axes with derotated axes
svg('results/axis1_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,1])), type='l', ylab='Acceleration axis 1 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,2])), type='l', ylab='Acceleration axis 2 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_quat.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,3])), type='l', ylab='Acceleration axis 3 [m/s²]', xlab='Time [ms]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()

# DEMONSTRATION OF IMPROVEMENTS

# do fft 
original_fft_power <- lapply(d, function(di) apply(di, MARGIN = 2, FUN = function(x)Mod(fft(x))[1:length(x)/2]))
original_fft_phase <- lapply(d, function(di) apply(di, MARGIN = 2, FUN = function(x)Arg(fft(x))[1:length(x)/2]))
derotated_fft_power <- apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)Mod(fft(x))[1:length(x)/2])
derotated_fft_phase <- apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)Arg(fft(x))[1:length(x)/2])
magnitudes_fft_power <- lapply(magnitudes, function(x)abs(fft(x))[1:length(x)/2])
magnitudes_fft_phase <- lapply(magnitudes, function(x)Arg(fft(x))[1:length(x)/2])

# FFT POWER: compare freq of axes with axes and derotated axes
svg('results/magnitudes_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(magnitudes_fft_power), type='l', ylab='Magnitudes acceleration power spectrum', xlab='Frequency [Hz]')
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis1_no_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],original_fft_power[[1]]), function(x)x[,1])), type='l', ylab='Axis 1 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_no_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],original_fft_power[[1]]), function(x)x[,2])), type='l', ylab='Axis 2 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_no_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],original_fft_power[[1]]), function(x)x[,3])), type='l', ylab='Axis 3 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis1_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],derotated_fft_power), function(x)x[,1])), type='l', ylab='Axis 1 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],derotated_fft_power), function(x)x[,2])), type='l', ylab='Axis 2 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_quat_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_power[[2]],derotated_fft_power), function(x)x[,3])), type='l', ylab='Axis 3 acceleration power spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()

# FFT PHASE: compare freq of axes with axes and derotated axes
svg('results/magnitudes_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(magnitudes_fft_phase), type='l', ylab='Magnitudes acceleration phase spectrum', xlab='Frequency [Hz]')
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis1_no_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],original_fft_phase[[1]]), function(x)x[,1])), type='l', ylab='Axis 1 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_no_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],original_fft_phase[[1]]), function(x)x[,2])), type='l', ylab='Axis 2 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_no_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],original_fft_phase[[1]]), function(x)x[,3])), type='l', ylab='Axis 3 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis1_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],derotated_fft_phase), function(x)x[,1])), type='l', ylab='Axis 1 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis2_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],derotated_fft_phase), function(x)x[,2])), type='l', ylab='Axis 2 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()
svg('results/axis3_quat_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(lapply(list(original_fft_phase[[2]],derotated_fft_phase), function(x)x[,3])), type='l', ylab='Axis 3 acceleration phase spectrum', xlab='Frequency [Hz]', col=1:2)
  legend(bg='white',"topright", legend = c('Device 1', 'Device 2'), col=1:2, lty=1:2, cex=0.75) # optional legend
dev.off()

# AUTOCORRELATION TEST
# auto covarinace in R
# plot(ccf(sin(1:200), sin(1:200), type='covariance', lag.max = 200), type='l')
# auto covariance in octave 
# plot(xcov(sin(1:200), sin(1:200), 'biased'))
# autocorrelation in R
# plot(ccf(sin(1:200), sin(1:200), type='correlation', lag.max = 200), type='l')
# autocorrelation in octave
# plot(xcov(sin(1:200), sin(1:200), 'coeff'))

# AUTOCORRELATION
# we go for covariance here as it is the same as correlation - just without normalization to [-1,1]
original_ac <- lapply(d, function(di) data.frame(apply(di, MARGIN = 2, FUN = function(x)ccf(x, x, type='correlation', lag.max = 200)$acf)))
derotated_ac <- data.frame(apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)ccf(x, x, type='correlation', lag.max = 200)$acf))
magnitudes_ac <- data.frame(lapply(magnitudes, function(x)ccf(x, x, type='correlation', lag.max = 200)$acf))

# statistics
o_rmse <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_rmse <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_rmse <- sqrt(1/length(magnitudes[[1]])*sum((magnitudes[[1]]-magnitudes[[2]])**2))

o_mae <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_mae <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_mae <- mean(abs(magnitudes[[1]]-magnitudes[[2]]))

o_median <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_median <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_median <- median(abs(magnitudes[[1]]-magnitudes[[2]]))

o_sd <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=sd)
q_sd <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=sd)
m_sd <- sd(magnitudes[[1]]-magnitudes[[2]])

o_mad <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_mad <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_mad <- mad(magnitudes[[1]]-magnitudes[[2]])

o_ccf <- lapply(1:3,function(i)max(ccf(d[[2]][,i],d[[1]][,i])$acf))
q_ccf <- lapply(1:3,function(i)max(ccf(d[[2]][,i],derotated$derotated[,i])$acf))
m_ccf <- max(ccf(magnitudes[[1]],magnitudes[[2]])$acf)

o_pearson <- lapply(1:3,function(i)cor(d[[2]][,i],d[[1]][,i], method = 'pearson'))
q_pearson <- lapply(1:3,function(i)cor(d[[2]][,i],derotated$derotated[,i], method = 'pearson'))
m_pearson <- cor(magnitudes[[1]],magnitudes[[2]], method = 'pearson')

o_kendall <- lapply(1:3,function(i)cor(d[[2]][,i],d[[1]][,i], method = 'kendall'))
q_kendall <- lapply(1:3,function(i)cor(d[[2]][,i],derotated$derotated[,i], method = 'kendall'))
m_kendall <- cor(magnitudes[[1]],magnitudes[[2]], method = 'kendall')

o_spearman <- lapply(1:3,function(i)cor(d[[2]][,i],d[[1]][,i], method = 'spearman'))
q_spearman <- lapply(1:3,function(i)cor(d[[2]][,i],derotated$derotated[,i], method = 'spearman'))
m_spearman <- cor(magnitudes[[1]],magnitudes[[2]], method = 'spearman')

o_dtw <- lapply(1:3, function(i)dtw(d[[2]][,i], d[[1]][,i])$normalizedDistance)
q_dtw <- lapply(1:3, function(i)dtw(d[[2]][,i], derotated$derotated[,i])$normalizedDistance)
m_dtw <- dtw(magnitudes[[1]], magnitudes[[2]])$normalizedDistance

o_fft_power_rmse <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_fft_power_rmse <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_fft_power_rmse <- sqrt(1/length(magnitudes_fft_power[[1]])*sum((magnitudes_fft_power[[1]]-magnitudes_fft_power[[2]])**2))

o_fft_power_mae <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_fft_power_mae <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_fft_power_mae <- mean(abs(magnitudes_fft_power[[1]]-magnitudes_fft_power[[2]]))

o_fft_power_median <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_fft_power_median <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_fft_power_median <- median(abs(magnitudes_fft_power[[1]]-magnitudes_fft_power[[2]]))

o_fft_power_sd <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=sd)
q_fft_power_sd <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=sd)
m_fft_power_sd <- sd((magnitudes_fft_power[[1]]-magnitudes_fft_power[[2]]))

o_fft_power_mad <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_fft_power_mad <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_fft_power_mad <- mad((magnitudes_fft_power[[1]]-magnitudes_fft_power[[2]]))

o_fft_phase_rmse <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_fft_phase_rmse <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_fft_phase_rmse <- sqrt(1/length(magnitudes_fft_phase[[1]])*sum((magnitudes_fft_phase[[1]]-magnitudes_fft_phase[[2]])**2))

o_fft_phase_mae <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_fft_phase_mae <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_fft_phase_mae <- mean(abs(magnitudes_fft_phase[[1]]-magnitudes_fft_phase[[2]]))

o_fft_phase_median <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_fft_phase_median <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_fft_phase_median <- median(abs(magnitudes_fft_phase[[1]]-magnitudes_fft_phase[[2]]))

o_fft_phase_sd <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=sd)
q_fft_phase_sd <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=sd)
m_fft_phase_sd <- sd((magnitudes_fft_phase[[1]]-magnitudes_fft_phase[[2]]))

o_fft_phase_mad <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_fft_phase_mad <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_fft_phase_mad <- mad((magnitudes_fft_phase[[1]]-magnitudes_fft_phase[[2]]))

lagmax <- 10 # 0.1s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_10_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_10_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_10_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_10_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_10_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_10_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_10_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_10_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_10_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_10_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_10_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_10_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_10_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_10_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_10_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_10_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_10_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_10_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 20 # 0.2s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_20_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_20_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_20_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_20_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_20_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_20_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_20_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_20_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_20_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_20_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_20_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_20_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_20_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_20_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_20_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_20_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_20_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_20_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 30 # 0.3s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_30_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_30_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_30_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_30_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_30_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_30_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_30_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_30_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_30_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_30_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_30_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_30_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_30_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_30_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_30_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_30_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_30_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_30_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 50 # 0.5s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_50_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_50_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_50_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_50_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_50_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_50_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_50_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_50_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_50_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_50_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_50_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_50_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_50_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_50_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_50_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_50_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_50_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_50_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 70 # 0.7s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_70_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_70_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_70_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_70_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_70_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_70_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_70_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_70_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_70_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_70_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_70_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_70_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_70_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_70_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_70_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_70_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_70_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_70_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 100 # 1.0s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_100_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_100_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_100_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_100_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_100_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_100_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_100_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_100_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_100_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_100_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_100_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_100_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_100_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_100_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_100_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_100_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_100_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_100_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

lagmax <- 150 # 1.5s
center <- ceiling(length(original_ac[[2]][[1]])/2)
ac_indexes <- (center - lagmax):(center + lagmax)
o_ac_150_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], original_ac[[1]][[i]][ac_indexes], method = 'pearson'))
q_ac_150_pearson <- lapply(1:3,function(i)cor(original_ac[[2]][[i]][ac_indexes], derotated_ac[[i]][ac_indexes], method = 'pearson'))
m_ac_150_pearson <- cor(magnitudes_ac[[1]][ac_indexes], magnitudes_ac[[2]][ac_indexes], method = 'pearson')

o_ac_150_rmse <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_ac_150_rmse <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_ac_150_rmse <- sqrt(1/length(magnitudes_ac[[1]][ac_indexes])*sum((magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])**2))

o_ac_150_mae <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_ac_150_mae <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_ac_150_mae <- mean(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_150_median <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
q_ac_150_median <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)median(abs(col)))
m_ac_150_median <- median(abs(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes]))

o_ac_150_sd <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
q_ac_150_sd <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=sd)
m_ac_150_sd <- sd(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])

o_ac_150_mad <- apply(X = original_ac[[1]][ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
q_ac_150_mad <- apply(X = derotated_ac[ac_indexes,]-original_ac[[2]][ac_indexes,], MARGIN = 2, FUN=function(col)mad(col, constant=1))
m_ac_150_mad <- mad(magnitudes_ac[[1]][ac_indexes]-magnitudes_ac[[2]][ac_indexes])



# AC: 10, 20, 30, 50, 70, 100, 150

# results in df and file
results <- rbind(t(c(m_rmse, o_rmse, q_rmse)),
                 t(c(m_mae, o_mae, q_mae)),
                 t(c(m_median, o_median, q_median)),
                 t(c(m_sd, o_sd, q_sd)),
                 t(c(m_mad, o_mad, q_mad)),
                 t(c(m_ccf, o_ccf, q_ccf)),
                 t(c(m_pearson, o_pearson, q_pearson)),
                 t(c(m_kendall, o_kendall, q_kendall)),
                 t(c(m_spearman, o_spearman, q_spearman)),
                 t(c(m_dtw, o_dtw, q_dtw)),
                 t(c(m_fft_power_rmse, o_fft_power_rmse, q_fft_power_rmse)),
                 t(c(m_fft_power_mae, o_fft_power_mae, q_fft_power_mae)),
                 t(c(m_fft_power_median, o_fft_power_median, q_fft_power_median)),
                 t(c(m_fft_power_sd, o_fft_power_sd, q_fft_power_sd)),
                 t(c(m_fft_power_mad, o_fft_power_mad, q_fft_power_mad)),
                 t(c(m_fft_phase_rmse, o_fft_phase_rmse, q_fft_phase_rmse)),
                 t(c(m_fft_phase_mae, o_fft_phase_mae, q_fft_phase_mae)),
                 t(c(m_fft_phase_median, o_fft_phase_median, q_fft_phase_median)),
                 t(c(m_fft_phase_sd, o_fft_phase_sd, q_fft_phase_sd)),
                 t(c(m_fft_phase_mad, o_fft_phase_mad, q_fft_phase_mad)),
                 t(c(m_ac_10_pearson, o_ac_10_pearson, q_ac_10_pearson)),
                 t(c(m_ac_10_rmse, o_ac_10_rmse, q_ac_10_rmse)),
                 t(c(m_ac_10_mae, o_ac_10_mae, q_ac_10_mae)),
                 t(c(m_ac_10_median, o_ac_10_median, q_ac_10_median)),
                 t(c(m_ac_10_sd, o_ac_10_sd, q_ac_10_sd)),
                 t(c(m_ac_10_mad, o_ac_10_mad, q_ac_10_mad)),
                 t(c(m_ac_20_pearson, o_ac_20_pearson, q_ac_20_pearson)),
                 t(c(m_ac_20_rmse, o_ac_20_rmse, q_ac_20_rmse)),
                 t(c(m_ac_20_mae, o_ac_20_mae, q_ac_20_mae)),
                 t(c(m_ac_20_median, o_ac_20_median, q_ac_20_median)),
                 t(c(m_ac_20_sd, o_ac_20_sd, q_ac_20_sd)),
                 t(c(m_ac_20_mad, o_ac_20_mad, q_ac_20_mad)),
                 t(c(m_ac_30_pearson, o_ac_30_pearson, q_ac_30_pearson)),
                 t(c(m_ac_30_rmse, o_ac_30_rmse, q_ac_30_rmse)),
                 t(c(m_ac_30_mae, o_ac_30_mae, q_ac_30_mae)),
                 t(c(m_ac_30_median, o_ac_30_median, q_ac_30_median)),
                 t(c(m_ac_30_sd, o_ac_30_sd, q_ac_30_sd)),
                 t(c(m_ac_30_mad, o_ac_30_mad, q_ac_30_mad)),
                 t(c(m_ac_50_pearson, o_ac_50_pearson, q_ac_50_pearson)),
                 t(c(m_ac_50_rmse, o_ac_50_rmse, q_ac_50_rmse)),
                 t(c(m_ac_50_mae, o_ac_50_mae, q_ac_50_mae)),
                 t(c(m_ac_50_median, o_ac_50_median, q_ac_50_median)),
                 t(c(m_ac_50_sd, o_ac_50_sd, q_ac_50_sd)),
                 t(c(m_ac_50_mad, o_ac_50_mad, q_ac_50_mad)),
                 t(c(m_ac_70_pearson, o_ac_70_pearson, q_ac_70_pearson)),
                 t(c(m_ac_70_rmse, o_ac_70_rmse, q_ac_70_rmse)),
                 t(c(m_ac_70_mae, o_ac_70_mae, q_ac_70_mae)),
                 t(c(m_ac_70_median, o_ac_70_median, q_ac_70_median)),
                 t(c(m_ac_70_sd, o_ac_70_sd, q_ac_70_sd)),
                 t(c(m_ac_70_mad, o_ac_70_mad, q_ac_70_mad)),
                 t(c(m_ac_10_pearson, o_ac_10_pearson, q_ac_10_pearson)),
                 t(c(m_ac_100_rmse, o_ac_100_rmse, q_ac_100_rmse)),
                 t(c(m_ac_100_mae, o_ac_100_mae, q_ac_100_mae)),
                 t(c(m_ac_100_median, o_ac_100_median, q_ac_100_median)),
                 t(c(m_ac_100_sd, o_ac_100_sd, q_ac_100_sd)),
                 t(c(m_ac_100_mad, o_ac_100_mad, q_ac_100_mad)),
                 t(c(m_ac_150_pearson, o_ac_150_pearson, q_ac_150_pearson)),
                 t(c(m_ac_150_rmse, o_ac_150_rmse, q_ac_150_rmse)),
                 t(c(m_ac_150_mae, o_ac_150_mae, q_ac_150_mae)),
                 t(c(m_ac_150_median, o_ac_150_median, q_ac_150_median)),
                 t(c(m_ac_150_sd, o_ac_150_sd, q_ac_150_sd)),
                 t(c(m_ac_150_mad, o_ac_150_mad, q_ac_150_mad)))
colnames(results) <- c('Magnitudes', 'Axes1', 'Axes2', 'Axes3', 'Axes1', 'Axes2', 'Axes3')
rownames(results) <- c('RMSE', 'MAE', 'Median', 'sd', 'MAD', 'CCF', 'COR_P', 'COR_K', 'COR_S', 'DTW', 
                       'FFT_Power_RMSE', 'FFT_Power_MAE', 'FFT_Power_Median', 'FFT_Power_sd', 'FFT_Power_mad', 
                       'FFT_Phase_RMSE', 'FFT_Phase_MAE', 'FFT_Phase_Median', 'FFT_Phase_sd', 'FFT_Phase_mad', 
                       'AC_10_COR_P', 'AC_10_RMSE', 'AC_10_MAE', 'AC_10_Median', 'AC_10_SD', 'AC_10_MAD',
                       'AC_20_COR_P', 'AC_20_RMSE', 'AC_20_MAE', 'AC_20_Median', 'AC_20_SD', 'AC_20_MAD',
                       'AC_30_COR_P', 'AC_30_RMSE', 'AC_30_MAE', 'AC_30_Median', 'AC_30_SD', 'AC_30_MAD',
                       'AC_50_COR_P', 'AC_50_RMSE', 'AC_50_MAE', 'AC_50_Median', 'AC_50_SD', 'AC_50_MAD',
                       'AC_70_COR_P', 'AC_70_RMSE', 'AC_70_MAE', 'AC_70_Median', 'AC_70_SD', 'AC_70_MAD',
                       'AC_100_COR_P', 'AC_100_RMSE', 'AC_100_MAE', 'AC_100_Median', 'AC_100_SD', 'AC_100_MAD',
                       'AC_150_COR_P', 'AC_150_RMSE', 'AC_150_MAE', 'AC_150_Median', 'AC_150_SD', 'AC_150_MAD')
formatted <- matrix(format(results, digits=2, nsmall=2, drop0trailing=FALSE), nrow=dim(results)[[1]])
colnames(formatted) <- colnames(results)
rownames(formatted) <- rownames(results)
print(results)
write.table(x = formatted, file = 'results/results.csv', quote = F, sep=',', row.names = T, col.names = T)
