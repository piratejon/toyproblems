CPPFLAGS=-lm -lcrypt -O2 -std=c++11 -pipe -I/home/js/include

default:
	$(CXX) $(CPPFLAGS) -DONLINE_JUDGE 100.cpp -o 100

check: test-100.cpp 100.cpp
	$(CXX) $(CPPFLAGS) 100.cpp test-100.cpp -o test-100
	./test-100
