//#include <iostream>
//#include <vector>
//#include <algorithm>
//#include <functional>
//
//int main(){
//
//	std::vector<int> num = { 5, 3, 4, 1, 2, 8, 7, 6 };
//	//std::sort(num.begin(), num.end(), std::greater<int>());
//	
//	std::sort(num.begin(), num.end(), [](int a, int b){
//		if (a == 1)
//			return false;
//		if (b == 1)
//			return true;
//		return a < b;
//	});
//	for (auto value : num)
//		std::cout << value << std::endl;
//
//	std::cin.get();
//}