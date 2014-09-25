function u = quat( phi, axis )
	u = [cos(phi/2); sin(phi/2)*normc([axis(1); axis(2); axis(3)])];
end
