//#include <iostream>
//#include <map>
//
//bool validCreditCard(std::string number) {
//	int size = number.size();
//	int weight;
//	if (size % 2 == 0) weight = 2;
//	else weight = 1;
//	int sum = 0;
//	for (int i = 0; i < size; i++)
//	{
//		int integer = int(number[i]) - int('0');
//		if (integer * weight > 9)
//			sum += (integer * weight) - 9;
//		else
//			sum += integer * weight;
//		if (weight == 2)
//			weight = 1;
//		else
//			weight = 2;
//	}
//	return (sum % 10 == 0) ? true : false;
//}
//
//int main()
//{
//	std::cout << validCreditCard("378282246310005") << std::endl;
//
//	std::cin.get();
//}