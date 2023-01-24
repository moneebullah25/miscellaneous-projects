//#include <iostream>
//
//struct Vector2{
//	float x, y;
//};
//
//class String
//{
//private:
//	char* m_Buffer;
//	size_t size;
//public:
//	String(const char* string){
//		size = strlen(string);
//		m_Buffer = new char[size + 1];
//		memcpy(m_Buffer, string, size);
//		m_Buffer[size] = '\0';
//	}
//	String(const String& str){
//		std::cout << "Copied Succesfully" << std::endl;
//		size = str.size;
//		m_Buffer = new char[size + 1];
//		memcpy(m_Buffer, str.m_Buffer, size + 1);
//	}
//
//	~String(){
//		delete[] m_Buffer;
//	}
//	
//	char& operator[](size_t index){
//		return m_Buffer[index];
//	}
//
//	friend std::ostream& operator<<(std::ostream& stream, const String& string);
//};
//
//std::ostream& operator<<(std::ostream& stream, const String& string)
//{
//	stream << string.m_Buffer;
//	return stream;
//}
//
//void PrintString(const String& str){
//	std::cout << str << std::endl;
//
//}
//
//int main()
//{
//	int x = 5;
//	char* y = (char*)&x;
//	std::cout << *y << std::endl;
//	std::cin.get();
//
//	Vector2* a = new Vector2;
//	Vector2* b = a;
//
//	String name = "MUNEEB ULLAH";
//	String name2 = name;
//
//	name2[1] = 'O';
//
//	PrintString(name); PrintString(name2);
//	
//	
//	std::cout << name << std::endl;
//	std::cout << name2 << std::endl;
//	
//	std::cin.get();
//
//}