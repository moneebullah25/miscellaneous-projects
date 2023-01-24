//#include <iostream>
//#include <vector>
//#include <algorithm>
//
//std::vector<int> sortNumsAscending(std::vector<int> arr)
//{
//	std::sort(arr.begin(), arr.end(), [](int& a, int& b){return a < b; });
//	return arr;
//}
//
////std::vector<int> sortNumsAscending(std::vector<int> arr,
////	size_t low = 1000000, size_t high = -1000000, int min_index = 0) 
////{
////	int min_index = min_index;
////	int max_index = arr.size() - 1 - min_index;
////	size_t lowest = low; size_t highest = high;
////	int lowest_index = -1, highest_index = -1;
////	for (int i = 0; i < arr.size(); i++)
////	{
////		if (arr[i] < lowest) {
////			lowest = arr[i];
////			lowest_index = i;
////		}
////		else if (arr[i] > highest)
////		{
////			highest = arr[i];
////			highest_index = i;
////		}
////	}
////	if (min_index != highest_index)
////	{
////		int temp_low = arr[min_index];
////		arr[min_index] = arr[lowest_index];
////		arr[lowest_index] = temp_low;
////
////		int temp_high = arr[max_index];
////		arr[max_index] = arr[highest_index];
////		arr[highest_index] = temp_high;
////	}
////	else if ()
////	
////
////}
//
//int main()
//{
//
//}