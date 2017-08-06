all: punter
	port=9026 make run

punter: punter.cpp
	g++ -std=c++11 -Wall punter.cpp -g3 -o punter

run:
	rm -f log.txt
	lamduct --game-port ${port} --log-level 3 --client-instance-logfile log.txt ./punter 2>&1 | tee capture.txt
