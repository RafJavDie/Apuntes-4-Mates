def divide(f,fs):
	s = len(fs);
	R = f.parent() # Recupera anillo polinómico
	qs = [R(0)] * s
	r = R(0)
	p = f
	print(p.lt())
	while not p.is_zero():
		i = 0
		divisionocurred = False
		while i < s and not divisionocurred:
			if fs[i].lt().divides(p.lt()):
				qs[i] += p.lt() / fs[i].lt()
				p -= R((p.lt() / fs[i].lt())) * fs[i]
				divisionocurred = True
			else:
				i = i+1
		if not divisionocurred:
			r += p.lt()
			p -= p.lt()
	return (qs,r)