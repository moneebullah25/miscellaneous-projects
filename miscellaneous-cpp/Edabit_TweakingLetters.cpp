//#include <iostream>
//#include <string>
//#include <vector>
//
//std::string tweakLetters(const std::string& str, std::vector<int> arr)
//{
//	int size = str.size();
//	std::string copy = "";
//	for (int i = 0; i < size; i++)
//	{
//		int char_ascii = (int)str[i] + arr[i];
//		if (char_ascii == 64 || char_ascii == 96)
//			char_ascii += 26;
//		else if (char_ascii == 91 || char_ascii == 123)
//			char_ascii -= 26;
//		copy = copy + char(char_ascii);
//	}
//	return copy;
//}
//
//
//int main()
//{
//	/*char a = 'A';
//	int a_ascii = (int)a;
//
//	char b = char(a_ascii);
//
//	std::cout << a_ascii << std::endl;
//	std::cout << b << std::endl;*/
//
//
//
//	std::cout << tweakLetters("rhino", { 1, 1, 1, 1, 1 }) << std::endl;
//
//
//	std::cin.get();
//
//}