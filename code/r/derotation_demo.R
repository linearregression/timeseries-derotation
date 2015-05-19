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

# do fft + derotation
original_fft_power <- lapply(d, function(di) apply(di, MARGIN = 2, FUN = function(x)Mod(fft(x))[1:length(x)/2]))
original_fft_phase <- lapply(d, function(di) apply(di, MARGIN = 2, FUN = function(x)Arg(fft(x))[1:length(x)/2]))
derotated_fft_power <- apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)Mod(fft(x))[1:length(x)/2])
derotated_fft_phase <- apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)Arg(fft(x))[1:length(x)/2])
magnitues_fft_power <- lapply(magnitudes, function(x)abs(fft(x))[1:length(x)/2])
magnitues_fft_phase <- lapply(magnitudes, function(x)Arg(fft(x))[1:length(x)/2])

# FFT POWER: compare freq of axes with axes and derotated axes
svg('results/magnitues_fft_power.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(magnitues_fft_power), type='l', ylab='Magnitudes acceleration power spectrum', xlab='Frequency [Hz]')
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
svg('results/magnitues_fft_phase.svg', bg=bg, height=height, width=width)
  matplot(x=seq(0,49.5,0.25), data.frame(magnitues_fft_phase), type='l', ylab='Magnitudes acceleration phase spectrum', xlab='Frequency [Hz]')
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

o_mad <- apply(X = d[[1]]-d[[2]], MARGIN = 2, FUN=mad)
q_mad <- apply(X = derotated$derotated-d[[2]], MARGIN = 2, FUN=mad)
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

o_dtw <- lapply(1:3, function(i)dtw(d[[2]][,i], d[[1]][,i])$distance)
q_dtw <- lapply(1:3, function(i)dtw(d[[2]][,i], derotated$derotated[,i])$distance)
m_dtw <- dtw(magnitudes[[1]], magnitudes[[2]])$distance

o_fft_power_rmse <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_fft_power_rmse <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_fft_power_rmse <- sqrt(1/length(magnitues_fft_power[[1]])*sum((magnitues_fft_power[[1]]-magnitues_fft_power[[2]])**2))

o_fft_power_mae <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_fft_power_mae <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_fft_power_mae <- mean(abs(magnitues_fft_power[[1]]-magnitues_fft_power[[2]]))

o_fft_power_median <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_fft_power_median <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_fft_power_median <- median(abs(magnitues_fft_power[[1]]-magnitues_fft_power[[2]]))

o_fft_power_sd <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=sd)
q_fft_power_sd <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=sd)
m_fft_power_sd <- sd((magnitues_fft_power[[1]]-magnitues_fft_power[[2]]))

o_fft_power_mad <- apply(X = original_fft_power[[1]]-original_fft_power[[2]], MARGIN = 2, FUN=mad)
q_fft_power_mad <- apply(X = derotated_fft_power-original_fft_power[[2]], MARGIN = 2, FUN=mad)
m_fft_power_mad <- mad((magnitues_fft_power[[1]]-magnitues_fft_power[[2]]))

o_fft_phase_rmse <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_fft_phase_rmse <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_fft_phase_rmse <- sqrt(1/length(magnitues_fft_phase[[1]])*sum((magnitues_fft_phase[[1]]-magnitues_fft_phase[[2]])**2))

o_fft_phase_mae <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_fft_phase_mae <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_fft_phase_mae <- mean(abs(magnitues_fft_phase[[1]]-magnitues_fft_phase[[2]]))

o_fft_phase_median <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_fft_phase_median <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_fft_phase_median <- median(abs(magnitues_fft_phase[[1]]-magnitues_fft_phase[[2]]))

o_fft_phase_sd <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=sd)
q_fft_phase_sd <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=sd)
m_fft_phase_sd <- sd((magnitues_fft_phase[[1]]-magnitues_fft_phase[[2]]))

o_fft_phase_mad <- apply(X = original_fft_phase[[1]]-original_fft_phase[[2]], MARGIN = 2, FUN=mad)
q_fft_phase_mad <- apply(X = derotated_fft_phase-original_fft_phase[[2]], MARGIN = 2, FUN=mad)
m_fft_phase_mad <- mad((magnitues_fft_phase[[1]]-magnitues_fft_phase[[2]]))



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
                 t(c(m_fft_phase_mad, o_fft_phase_mad, q_fft_phase_mad)))
colnames(results) <- c('Magnitudes', 'Axes1', 'Axes2', 'Axes3', 'Axes1', 'Axes2', 'Axes3')
rownames(results) <- c('RMSE', 'MAE', 'Median', 'sd', 'MAD', 'CCF', 'COR_P', 'COR_K', 'COR_S', 'DTW', 'FFT_Power_RMSE', 'FFT_Power_MAE', 'FFT_Power_Median', 'FFT_Power_sd', 'FFT_Power_mad', 'FFT_Phase_RMSE', 'FFT_Phase_MAE', 'FFT_Phase_Median', 'FFT_Phase_sd', 'FFT_Phase_mad')
formatted <- matrix(format(results, digits=2, nsmall=2, drop0trailing=FALSE), nrow=dim(results)[[1]])
colnames(formatted) <- colnames(results)
rownames(formatted) <- rownames(results)
print(results)
write.table(x = formatted, file = 'results/results.csv', quote = F, sep=',', row.names = T, col.names = T)
