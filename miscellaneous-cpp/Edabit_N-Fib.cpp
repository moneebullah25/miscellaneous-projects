#include <iostream>
#include <map>

//unsigned int fib(int seq, int index, std::map<int, unsigned int>* map = {})
//{
//	if ((*map).find(index) != (*map).end())
//		return (*map)[index];
//	
//	if (seq == 1)
//		(*map)[index] = fib(index - 1, seq, map);
//	else if (seq == 2)
//		(*map)[index] = fib(index - 1, seq, map) + fib(index - 2, seq, map);
//	else if (seq == 3)
//		(*map)[index] = fib(index - 1, seq, map) + fib(index - 2, seq, map) + fib(index - 3, seq, map);
//
//	/*for (int i = 0; i < seq; i++)
//	{
//		(*map)[index] = fib(index - i + 1, seq, map);
//	}*/
//
//	return (*map)[index];
//}

//unsigned int bonacci(int seq, int index) {
	
	/*if (seq == 2)
	{
		if (index < 2)
			return index;
		return bonacci(seq, index - 1) + bonacci(seq, index - 2);
	}*/
	/*unsigned int x = 0;
	for (int i = 0; i < seq; i++)
	{
		if (i < seq - 2) return 0;
		else if (i < seq - 1) return 1;
		x += bonacci(seq, index - (i + 1));
	}
	return x;*/
	
//}

unsigned int fib(int n, int k, std::map<int, unsigned int>* map = {})
{
	if ((*map).find(n) != (*map).end())
		return (*map)[n];
	
	/*if (n < 48)
		(*map)[n] = fib(n - 1, k, map) + fib(n - 2, k, map);*/
	
	for (int i = 0; i < n; i++)
	{
		if (k < n) return 1;
		(*map)[n] += fib(n - (i + 1), k, map);
	}
	
}

int main()
{
	std::map<int, unsigned int> map = {};
	std::cout << fib(2, 10, &map) << std::endl;
	/*std::cout << bonacci(1, 10) << std::endl;
	std::cout << bonacci(3, 10) << std::endl;
	std::cout << bonacci(4, 10) << std::endl;
	std::cout << bonacci(5, 10) << std::endl;
*/
	std::cin.get();
}