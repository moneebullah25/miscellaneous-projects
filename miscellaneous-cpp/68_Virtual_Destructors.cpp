//#include <iostream>
//
//class base
//{
//public:
//	base(){ std::cout << "base Constructor\n"; }
//	virtual ~base(){ std::cout << "base Destructor\n"; }
//};
//
//class derived: public base
//{
//public:
//	derived(){ std::cout << "derived Constructor\n"; }
//	~derived() override { std::cout << "derived Destructor\n"; }
//};
//
//int main()
//{
//	base* b = new base;
//	delete b;
//	std::cout << "------------------------\n";
//	derived* d = new derived;
//	delete d;
//	std::cout << "------------------------\n";
//	base* bd = new derived;
//	delete bd;
//
//
//	std::cin.get();
//}