#pragma once

#include <iostream>

class MyString
{
private:
	char* m_Buffer;
	size_t size;
public:
	MyString();
	MyString(const char* string);
	MyString(const MyString& str);
	char& operator[](size_t index);
	MyString& operator=(const MyString& str);
	friend std::ostream& operator<<(std::ostream& stream, const MyString& string);
	~MyString();
};