//#include <iostream>
//
//class Array2D
//{
//private:
//	int ** arr2d; 
//	int * arr1d;
//public:
//	Array2D(int row, int col)
//	{
//		arr2d = new int*[row];
//		arr1d = new int[col];
//		for (int i = 0; i < row; i++)
//			arr2d = &arr1d;
//	}
//	int* operator[](int index_x)
//	{
//		return arr2d[index_x];
//	}
//	
//};
//
//int main(){
//
//	//const int x_axis = 50;
//	//const int y_axis = 5;
//
//	//int arr[x_axis * y_axis];
//	//int** arr2 = new int*[x_axis];
//
//	//for (int i = 0; i < x_axis; i++)
//	//	arr2[i] = &arr[i * y_axis];
//
//	//int count = 1;
//
//	//for (int i = 0; i < x_axis; i++){
//	//	for (int j = 0; j < y_axis; j++){
//	//		arr2[i][j] = count;
//	//		count += 1;
//	//	}
//	//}
//
//	//for (int i = 0; i < x_axis; i++){
//	//	for (int j = 0; j < y_axis; j++)
//	//		std::cout << arr2[i][j] << std::endl;
//
//	//}
//
//
//	//std::cin.get();
//
//	//int** arr2 = new int*[50];
//	//for (int i = 0; i < 50; i++){
//	//	arr2[i] = new int[50];
//	//	for (int j = 0; j < 50; j++){
//	//		arr2[i][j] = 1;
//	//	}
//	//}
//
//	//// To free up memory we have to individually delete each element
//	//for (int i = 0; i < 50; i++)
//	//	delete[] arr2[i];
//
//	//delete[] arr2;
//}