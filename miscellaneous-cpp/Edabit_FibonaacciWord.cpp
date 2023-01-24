//#include <iostream>
//#include <string>
//
////
////std::string generateWord(int n) {
////	// your recursive solution here
////	if (n < 2) return "Invalid";
////	else {
////		char next = 'b'; char prev = 'a';
////		std::string str = "";
////		for (int i = 0; i < n; i++)
////		{
////			str += prev;
////		}
////	}
////}
//
//// Spatans Cypher
//std::string spartansCipher(std::string message, int nSlide) {
//	int cols = int(((float(message.size()) / float(nSlide)) + 1));
//	int *arr = new int[cols * nSlide];
//	int** arr2 = new int*[cols];
//	for (int i = 0; i < cols; i++)
//		arr2[i] = &arr[i * nSlide];
//
//	int count = 0;
//	std::string copy = "";
//
//	for (int i = 0; i < cols; i++){
//		for (int j = 0; j < nSlide; j++){
//			if (count + 1 < message.size()){
//				arr2[i][j] = message[count];
//				count += 1;
//			}
//			else
//				arr2[i][j] = '-';
//		}
//	}
//
//	for (int i = 0; i < nSlide; i++){
//		for (int j = 0; j < cols; j++){
//			copy += arr2[i][j];
//		}
//	}
//
//	for (int i = 0; i < cols; i++)
//		delete[] arr2[i];
//		
//	delete[] arr2;
//	return copy;
//
//	
//	/*int index = 1;
//	std::string str = "";
//	for (int i = 0; i < message.size(); i++)
//	{
//		if (i * 4 < message.size())
//			str += message[i * 4];
//		else if (i == nSlide)
//			str += message[index];
//		else if (((i * 4) - 21) < message.size())
//			str += message[((i * 4) - 21) - 2];
//		else if (message.size() < 2 * message.size())
//			str += message[((i * 4) - 42) - 4];
//	}*/
//}
//
//int main()
//{
//	std::cout << spartansCipher("Mubashir Scytale Code", 6);
//
//}