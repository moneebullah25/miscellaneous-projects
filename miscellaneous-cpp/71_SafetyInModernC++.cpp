//#include <iostream>
//
//class SmartPointer 
//{
//private:
//	int* ptr;
//public:
//	explicit SmartPointer(int* addr = nullptr){ ptr = addr; }
//	~SmartPointer(){ delete (ptr); }
//	int& operator*() { return *ptr; }
//};
//
//int main()
//{
//	int* x = new int();
//	SmartPointer sm(x);
//	*sm = 20;
//
//	std::cout << *sm << std::endl;
//	sm.~SmartPointer();
//	std::cin.get();
//}