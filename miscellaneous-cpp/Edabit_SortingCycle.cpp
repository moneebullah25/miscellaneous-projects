//#include <iostream>
//#include <vector>
//#include <algorithm>
//
//// Input will be an array of objects of the form:
//// {operation: "insert", text: string, position: number}
//// {operation: "delete", from: number, length: number}
//
//void cycleLength(std::vector<int>& vec)
//{
//	for (int i = 0; i < vec.size(); i++){
//
//		int iMin = i;
//
//		for (int j = i + 1; j < vec.size(); j++)
//		{
//			if (vec[j] < vec[i]){
//				iMin = j;
//			}
//		}
//		int temp = vec[i];
//		vec[i] = vec[iMin];
//		vec[iMin] = temp;
//	}
//}
//
//int main()
//{
//	std::vector<int> sorted = { 1, 9, 6, 4, 6, 9 };
//	cycleLength(sorted);
//	
//	for (auto value : sorted)
//		std::cout << value << std::endl;
//
//	std::cin.get();
//}