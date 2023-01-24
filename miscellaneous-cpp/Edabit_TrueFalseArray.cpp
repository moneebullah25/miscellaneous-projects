//#include <iostream>
//#include <vector>
//#include <map>
//
//bool tfArr(std::vector<float> arr) {
//	if (arr[4] + arr[2] == arr[5])
//		return true;
//	return false;
//}
//
//// Memorization for Fib function
//
//
//unsigned int fib(int &&n, std::map<int, unsigned int>* map = {})
//{
//	if ((*map).find(n) != (*map).end())
//		return (*map)[n];
//	if (n < 48)
//		(*map)[n] = fib(n - 1, map) + fib(n - 2, map);
//	else 
//	return (*map)[n];
//}
//
//int main()
//{
//	std::map<int, unsigned int> map{ { 0, 0 }, { 1, 1 } };
//	std::cout << fib(7, &map) << std::endl;
//	std::cout << fib(6, &map) << std::endl;
//	std::cout << fib(8, &map) << std::endl;
//	std::cout << fib(50, &map) << std::endl;
//
//
//	std::cin.get();
//}