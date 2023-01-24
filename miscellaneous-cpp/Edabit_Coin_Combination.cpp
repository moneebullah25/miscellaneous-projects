//#include <iostream>
//#include <vector>
//#include <array>
//#define log(x) std::cout << x << std::endl
//
//int coinsCombinations(int money, std::vector<int> coins) {
//	int count = 0;
//	for (int i = 0; i < coins.size(); i++){
//		if (money % coins[i] == 0){
//			count += 1;
//		}
//		else if (money % coins[i] != 0){
//			for (int j = 0; j < money; j++){
//				if (j * coins.at(i) == money){
//					count += 1;
//				}
//			}
//		}
//		
//	}
//	for (int i = 0; i < coins.size(); i++){
//		for (int x = 0; x < coins.size(); x++){
//			if (x != i){
//				for (int y = 0; y < money; y++){
//					for (int z = 0; z < coins.size(); z++){
//						if (y * coins[x] + coins[i] * z == money)
//							count += 1;
//					}
//				}
//			}
//		}
//	}
//
//	return count;
//	
//}
//
//int main()
//{
//	std::cout << coinsCombinations(4, { 1, 2 }) << std::endl;
//	std::cin.get();
//}
