//#include <iostream>
//#include <exception>
//
//#define log(x) std::cout << x << std::endl
//
////NOTES:
////0. dynamic_cast is used at run time to find out correct down - cast.
////1 : Need at least one virtual function in base class.
////2 : If the cast is successful, dynamic_cast returns a value of type new_type.
////3 : If the cast fails and new_type is a pointer type, it returns a null pointer of that type.
////4 : If the cast fails and new_type is a reference type, it throws an exception that matches a handler of type std::bad_cast.
//
//class Entity{
//protected:
//	int x, y;
//public:
//	virtual void print(){ log("Entity"); }
//};
//
//class Player : public Entity{
//protected:
//	int power;
//	int health;
//public:
//	void print(){ log("Player"); }
//};
//
//class Enemy : public Entity{
//protected:
//	int health;
//	int damage;
//public:
//	void print(){ log("Enemy"); }
//};
//
//int main()
//{
//	Player p;
//	Entity* player = new Player();
//	Enemy* e1 = new Enemy();
//	Player* pl = dynamic_cast<Player*>(player);
//
//	if (pl == nullptr)
//		log("Dynamic Casting Failed");
//	else
//		log("Dynamic Casting Successfull");
//
//	try{
//		Enemy& r = dynamic_cast<Enemy&>(p);
//	}
//	catch (std::exception& e){
//		log(e.what());
//	}
//
//
//	std::cin.get();
//}
//
//
////BOTTOM LINE :
////1. work only on polymorphic base class (at least one virtual function in base class) because it uses this information to decide about wrong down - cast.
////2. it is used for up - cast(D to B) and down - cast(B to D), but it is mainly used for correct downcast.
////3. using this cast has run time overhead, because it checks object types at run time using RTTI(Run Time Type Information).
////4. if we are sure that we will never cast to wrong object then we should always avoid this cast and use static_cast.