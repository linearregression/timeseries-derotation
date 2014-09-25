% Rainhard Findling
% u'smile Lab, University of Applied Sciences Upper Austria
% 04/2014
% 
% Demo of derotating timeseries using residuum in octave/Matlab
% 

% load demo data
X = load('../../demo_data/X1.csv')(1:200,:);
Y = load('../../demo_data/Y1.csv')(1:200,:);

% remove gravity
X = X - mean(X);
Y = Y - mean(Y);

% look at initial timeseries
figure; plot([X(:,1)'; Y(:,1)']')
title('initial: axes 1')
figure; plot([X(:,2)'; Y(:,2)']')
title('initial: axes 2')
figure; plot([X(:,3)'; Y(:,3)']')
title('initial: axes 3')

% derotate 
[E,P] = residuum(X',Y');
P = P';

% write derotated data to file
mkdir('results');
dlmwrite('results/X_derotated.csv', P, ' ');

% look at result timeseries
figure; plot([P(:,1)'; Y(:,1)']')
title('derotated: axes 1')
figure; plot([P(:,2)'; Y(:,2)']')
title('derotated: axes 2')
figure; plot([P(:,3)'; Y(:,3)']')
title('derotated: axes 3')
