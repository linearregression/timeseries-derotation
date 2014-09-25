function [E,P] = residuum( X, Y )

%correlation matrix
R=X*(Y');
%matrix holding all rotations
F=[R(1,1)+R(2,2)+R(3,3), R(2,3)-R(3,2), R(3,1)-R(1,3), R(1,2)-R(2,1);
R(2,3)-R(3,2), R(1,1)-R(2,2)-R(3,3), R(1,2)+R(2,1), R(1,3)+R(3,1);
R(3,1)-R(1,3), R(1,2)+R(2,1), -R(1,1)+R(2,2)-R(3,3), R(2,3)+R(3,2);
R(1,2)-R(2,1), R(1,3)+R(3,1), R(2,3)+R(3,2), -R(1,1)-R(2,2)+R(3,3) ];
%compute eigenvector evvmax of largest eigenvalue ev
[V,D] = eig(F);
ev = D(1,1); evvmax = V(:,1); %first ev and evvmax
for i=2:4
	if D(i,i)>ev
		ev = D(i,i); evvmax = V(:,i); %remember largest ev and evvmax
	end
end
[E,P]=reser(X,Y,evvmax); %compute error and optimal P=UX

end

function [E,P] = reser( X, Y, u )
	[n,m] = size(X);
	E=0; P=[];
	for k=1:m
		y = rotquat([0; X(:,k)],u); %rotate the x vectors
		P=[P y(2:4)]; %store in P=UX
		E = E + norm( Y(:,k) - P(:,k) )^ 2; %sum of norms of differences
	end
	E = E/m;
end
