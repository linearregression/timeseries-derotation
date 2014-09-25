function pq = qmul( p, q )
	a0 = p(1); a = p(2:4);
	b0 = q(1); b = q(2:4);
	pq = [a0*b0-dot(a,b); a0*b + b0*a + cross(a,b)];
end


