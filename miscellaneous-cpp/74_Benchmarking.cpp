//#include <iostream>
//#include <chrono>
//#include <array>
//#include <memory>
//
//class Timer{
//public:
//	Timer()
//	{
//		m_StartTimepoint = std::chrono::high_resolution_clock::now();
//	}
//
//	~Timer()
//	{
//		auto endTimepoint = std::chrono::high_resolution_clock::now();
//		auto start = std::chrono::time_point_cast<std::chrono::milliseconds>(m_StartTimepoint).time_since_epoch().count();
//		auto stop = std::chrono::time_point_cast<std::chrono::milliseconds>(endTimepoint).time_since_epoch().count();
//		auto duration = stop - start;
//		auto seconds = duration * 0.001;
//		std::cout << duration << "ms (" << 
//			seconds << "s) \n";
//	}
//
//private:
//	std::chrono::time_point<std::chrono::high_resolution_clock> m_StartTimepoint;
//};
//
//int main()
//{
//	size_t value = 0;
//
//	std::cout << "Billions Loop\n";
//	{
//		Timer timer;
//		for (size_t i = 0; i < 1000000000; i++)
//			value += 2;
//	}
//
//	struct Vector2{ float x, y; };
//
//	std::cout << "Shared Ptr\n";
//	{
//		Timer timer;
//		std::array<std::shared_ptr<Vector2>, 1000> sharedPtrs;
//		for (int i = 0; i < sharedPtrs.size(); i++)
//			sharedPtrs[i] = std::make_shared<Vector2>();
//	}
//
//	std::cout << "New Shared Ptr\n";
//	{
//		Timer timer;
//		std::array<std::shared_ptr<Vector2>, 1000> sharedPtrs;
//		for (int i = 0; i < sharedPtrs.size(); i++)
//			sharedPtrs[i] = std::make_shared<Vector2>(new Vector2());
//	}
//
//	std::cout << "Unique Ptr\n";
//	{
//		Timer timer;
//		std::array<std::unique_ptr<Vector2>, 1000> unique_ptr;
//		for (int i = 0; i < unique_ptr.size(); i++)
//			unique_ptr[i] = std::make_unique<Vector2>();
//	}
//
//	std::cout << value << std::endl;
//
//	__debugbreak();
//}