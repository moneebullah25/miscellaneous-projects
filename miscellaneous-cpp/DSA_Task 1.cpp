//#include <iostream>
//#include <string>
//
//int decimalToBinary(int decimal)
//{
//	int x = 0;
//	int n = 1;
//	while (decimal)
//	{
//		x = x + (n * (decimal % 2));
//		n = n * 10;
//		decimal = decimal / 2;
//	}
//	return x;
//}
//
//double getLength(){
//	double result;
//	std::cout << "Enter Length : ";
//	std::cin >> result;
//	return result;
//}
//
//double getWidth(){
//	double result;
//	std::cout << "Enter Width : ";
//	std::cin >> result;
//	return result;
//}
//
//double getArea(const double& len, const double& wid){
//	return len * wid;
//}
//
//void displayData(const double& len, const double& wid,
//	const double& area){
//	std::cout << "Length : " << len << "\n";
//	std::cout << "Width : " << wid << "\n";
//	std::cout << "Area : " << area << "\n";
//}
//
//int main(void) {
//	std::cout << "Muneeb Ullah F2020376037" << std::endl;
//	int question = 0;
//	std::cout << "Enter Question : "; std::cin >> question;
//	switch (question){
//	case 1:
//		{
//			int n = 10;
//			int sum = 0;
//			while (n > 0){
//				std::cout << n << " ";
//			}
//			sum = sum + n;
//			std::cout << "Sum : " << sum << std::endl;
//		}
//	case 2:
//		{
//			int n1 = 9, n2 = 12;
//			while (n1 != n2) {
//				if (n1 > n2)
//					n1 -= n2;
//				else
//					n2 -= n1;
//			}
//			std::cout << "HCF : " << n1 << std::endl;
//		}
//	case 3:
//		{
//			int range = 100;
//			for (int i = 2; i < range + 1; i++){
//				int index = 0;
//				for (int j = 1; j <= i; j++){
//					if (i % j == 0)
//						index += 1;
//				}
//				if (index == 2)
//					std::cout << i << std::endl;
//			}
//
//		}
//	case 4:
//		{
//			int factorial = 4;
//			int result = 1;
//			for (int i = 1; i < factorial + 1; i++){
//				result = result * i;
//			}
//			std::cout << "Factorial of " << factorial <<
//				" = " << result << std::endl;
//		}
//	case 5:
//		{
//			int x = 9;
//			std::cout << "Binary of " << x << " = " <<
//				decimalToBinary(x) << std::endl;
//
//
//		}
//	case 6:
//		{
//			double length = getLength();
//			double width = getWidth();
//			double area = getArea(length, width);
//			displayData(length, width, area);
//		}
//	}
//}
