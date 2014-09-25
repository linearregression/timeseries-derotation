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
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend)
dev.off()
# compare axes
svg('results/axis1_no_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,1])), type='l', ylab='Acceleration axis 1 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()
svg('results/axis2_no_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,2])), type='l', ylab='Acceleration axis 2 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()
svg('results/axis3_no_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(d, function(x)x[,3])), type='l', ylab='Acceleration axis 3 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()

# derotate axes
derotated <- residuum(d[[1]],d[[2]])

# compare axes with derotated axes
svg('results/axis1_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,1])), type='l', ylab='Acceleration axis 1 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()
svg('results/axis2_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,2])), type='l', ylab='Acceleration axis 2 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()
svg('results/axis3_quat.svg', bg=bg, height=height, width=width)
matplot(x=seq(0,1990,10), data.frame(lapply(list(derotated$derotated, d[[2]]), function(x)x[,3])), type='l', ylab='Acceleration axis 3 [m/s²]', xlab='Time [ms]', col=1:2)
legend(bg='white',"topleft", legend = c('Device 1', 'Device 2'), col=1:2, pch=1, cex=0.75) # optional legend
dev.off()

# do fft + derotation
d_fft <- lapply(d, function(di) apply(di, MARGIN = 2, FUN = function(x)abs(fft(x))[1:length(x)/2]))
derotated_fft <- apply(X = derotated$derotated, MARGIN = 2, FUN=function(x)abs(fft(x))[1:length(x)/2])
magnitudes_fft <- lapply(magnitudes, function(x)abs(fft(x))[1:length(x)/2])

# compare freq of axes with axes and derotated axes
matplot(data.frame(magnitudes_fft), type='l')
matplot(data.frame(lapply(list(d_fft[[2]],d_fft[[1]]), function(x)x[,1])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)
matplot(data.frame(lapply(list(d_fft[[2]],derotated_fft), function(x)x[,1])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)
matplot(data.frame(lapply(list(d_fft[[2]],d_fft[[1]]), function(x)x[,2])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)
matplot(data.frame(lapply(list(d_fft[[2]],derotated_fft), function(x)x[,2])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)
matplot(data.frame(lapply(list(d_fft[[2]],d_fft[[1]]), function(x)x[,3])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)
matplot(data.frame(lapply(list(d_fft[[2]],derotated_fft), function(x)x[,3])), type='l', ylab='Axis 1 [m/s²]', xlab='Frequency [ms]', col=1:2)

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
o_fft_rmse <- apply(X = d_fft[[1]]-d_fft[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
q_fft_rmse <- apply(X = derotated_fft-d_fft[[2]], MARGIN = 2, FUN=function(col)sqrt(1/length(col)*sum(col**2)))
m_fft_rmse <- sqrt(1/length(magnitudes_fft[[1]])*sum((magnitudes_fft[[1]]-magnitudes_fft[[2]])**2))
o_fft_mae <- apply(X = d_fft[[1]]-d_fft[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
q_fft_mae <- apply(X = derotated_fft-d_fft[[2]], MARGIN = 2, FUN=function(col)mean(abs(col)))
m_fft_mae <- mean(abs(magnitudes_fft[[1]]-magnitudes_fft[[2]]))
o_fft_median <- apply(X = d_fft[[1]]-d_fft[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
q_fft_median <- apply(X = derotated_fft-d_fft[[2]], MARGIN = 2, FUN=function(col)median(abs(col)))
m_fft_median <- median(abs(magnitudes_fft[[1]]-magnitudes_fft[[2]]))

# results in df and file
results <- rbind(t(c(m_rmse, o_rmse, q_rmse)),
                 t(c(m_mae, o_mae, q_mae)),
                 t(c(m_median, o_median, q_median)),
                t(c(m_ccf, o_ccf, q_ccf)),
                t(c(m_pearson, o_pearson, q_pearson)),
                t(c(m_kendall, o_kendall, q_kendall)),
                t(c(m_spearman, o_spearman, q_spearman)),
                t(c(m_dtw, o_dtw, q_dtw)),
                t(c(m_fft_rmse, o_fft_rmse, q_fft_rmse)),
                t(c(m_fft_mae, o_fft_mae, q_fft_mae)),
                t(c(m_fft_median, o_fft_median, q_fft_median)))
colnames(results) <- c('Magnitudes', 'Axes1', 'Axes2', 'Axes3', 'Axes1', 'Axes2', 'Axes3')
rownames(results) <- c('RMSE', 'MAE', 'Median', 'CCF', 'COR_P', 'COR_K', 'COR_S', 'DTW', 'FFT_RMSE', 'FFT_MAE', 'FFT_Median')
formatted <- matrix(format(results, digits=2, nsmall=2, drop0trailing=FALSE), nrow=dim(results)[[1]])
colnames(formatted) <- colnames(results)
rownames(formatted) <- rownames(results)
write.table(x = formatted, file = 'results/results.csv', quote = F, sep=',', row.names = T, col.names = T)
