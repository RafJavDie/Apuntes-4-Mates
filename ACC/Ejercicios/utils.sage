def divide(f,fs):
	s = len(fs);
	R = f.parent() # Recupera anillo polinómico
	qs = [R(0)] * s
	r = R(0)
	p = f
	while not p.is_zero():
		i = 0
		divisionocurred = False
		while i < s and not divisionocurred:
			if fs[i].lt().divides(p.lt()):
				qs[i] += p.lt() / fs[i].lt()
				p -= (p.lt() // fs[i].lt()) * fs[i]
				divisionocurred = True
			else:
				i = i+1
		if not divisionocurred:
			r += p.lt()
			p -= p.lt()
	return (qs,r)

def syzygy(f,g):
	l = lcm(f.lm(), g.lm())
	return (l // f.lt()) * f - (l // g.lt()) * g)

def syzyplus(g):
	return [syzygy(g[i],g[j]) for i in range(0,len(g)-1)]

def dival(f,fs):
	return [divide(f,fs)[1] for f in fs]

def nexpaso(g):
	lista1 = dival(syzyplus(g),g);
	lista2 = list(set(lista1).union(g))
	lista3 = [x for x in lista2 if x != 0]
	return(lista3)

def grobi(g):
	h=g
	while h != nexpaso(h):
		h = nexpaso(h)
	return(h)