//#include <iostream>
//
//struct Cow{
//	int age; 
//	int legs;
//	char gender;
//	bool is_butchered;
//};
//
//class base
//{
//public:
//	base(): v(10) {}
//private:
//	int v;
//};
//
//int main()
//{
//	base b;
//	// C Style Conversion 
//	//int b_intv = *((int*)&b);
//
//	// However C++ Style Conversion Doesn't allow
//	// int b_intv = static_cast<int*>(&b);
//	int b_intv = *(reinterpret_cast<int*>(&b));
//	//std::cout << b_intv << std::endl;
//
//	Cow cow;
//	cow.age = 2; cow.legs = 4;
//	cow.gender = 'm'; cow.is_butchered = true;
//	
//	Cow* cowptr = &cow;
//	int* intptr = reinterpret_cast<int*>(&cow);
//	std::cout << *intptr << std::endl; // Age
//	intptr++;
//	std::cout << *intptr << std::endl; // Legs
//	intptr++;
//	std::cout << *(reinterpret_cast<char*>(intptr)) << std::endl; // Gender
//	char* charptr = reinterpret_cast<char*>(intptr);
//	charptr++;
//	std::cout << *(reinterpret_cast<bool*>(charptr)) << std::endl; // Is_Butchered
//
//
//
//	std::cin.get();
//
//}