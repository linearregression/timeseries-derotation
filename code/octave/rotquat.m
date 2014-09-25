function upuc = rotquat( p, u )
	uc = [u(1); -u(2:4)];
	up = qmul(u,p);
	upuc = qmul(up,uc);
end
