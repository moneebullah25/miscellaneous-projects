#include "MyString.h"

MyString::MyString()
{
	size = 0;
	m_Buffer = new char;
	m_Buffer = '\0';
}

MyString::MyString(const char* string){
	size = strlen(string);
	m_Buffer = new char[size + 1];
	memcpy(m_Buffer, string, size);
	m_Buffer[size] = '\0';
}

MyString::MyString(const MyString& str){
	std::cout << "Copied Succesfully" << std::endl;
	size = str.size;
	m_Buffer = new char[size + 1];
	memcpy(m_Buffer, str.m_Buffer, size + 1);
}


char& MyString::operator[](size_t index){
	if (index >= size)
		std::out_of_range;
	return m_Buffer[index];
}

MyString& MyString::operator=(const MyString& str)
{
	if (this == &str)
		return *this;

	this->MyString::MyString(str);
	return *this;
}

std::ostream& operator<<(std::ostream& stream, const MyString& string)
{
	stream << string.m_Buffer;
	return stream;
}

MyString::~MyString(){
	delete[] m_Buffer;
}