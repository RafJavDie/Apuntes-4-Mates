#include <iostream>
#include <random>
#include <map>
#include <primesieve.hpp>

const int N = 1500-2; // tamaño de listas
const int Nexperimentos = 10000;   // Número de experimentos
bool es_gilbreath(std::vector<int> seq);

int main() {
	std::vector<int> primos;
	primos.reserve(N);
	primesieve::generate_n_primes(N, 0, &primos);
	std::default_random_engine generator;
	std::uniform_int_distribution<int> distribution(1,N-1);
	int exitos = 0;
	for (int k=0; k < Nexperimentos; ++k) {
		std::vector<int> seq = {2,3};
		seq.reserve(N+seq.size());
		for (int i=0; i<N; ++i) {
			int n = distribution(generator);
			int salto = primos[n]-primos[n-1];
			seq.push_back(seq.back()+salto);
		}
		if (es_gilbreath(seq))
			++exitos;
	}

	std::cout<< exitos << '/' << Nexperimentos << std::endl;
	return 0;
}

bool es_gilbreath(std::vector<int> seq) {
	for (int i = 0; i < N; ++i) {
		for (int j = 0; j < N-i-1; ++j)
			seq[j] = abs(seq[j]-seq[j+1]);
		if (seq[0] != 1)
			return false;
	}
	return true;
}
