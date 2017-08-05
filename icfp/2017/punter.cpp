/*
 * punter.cpp
 * lamdbapunter@z10x.com
 * 2017-08-05
 * In which I attempt to participate in ICFP Contest 2017
 *    ... with a procedural language ;-)
 *
 * Copyright 2017 Jonathan Stone
 * */
#include <iostream>
#include <cstdio>
#include <string>

#include "json.hpp"
using json = nlohmann::json;

const char DEFAULTNAME[] = "happy punter";

std::string tag_line_length(std::string const & line) {
  std::stringstream ss;
  ss << line.length() << ':' << line;
  return ss.str();
}

std::string json_from_stdin() {
  std::string str_length, str_json;
  int i, length;
  char c;

  // get numbers until colon
  while (1) {
    c = getchar();
    if (c == ':') {
      break;
    } else if (c >= '0' && c <= '9') {
      str_length += c;
    }
  }

  length = 0;
  std::stringstream strstr(str_length);
  strstr >> length;

  for (i = 0; i < length; i += 1) {
    str_json += getchar();
  }

  return str_json;
}

void send_message(const json & msg) {
  std::string msg_str = msg.dump();
  msg_str += '\n';
  msg_str = tag_line_length(msg_str);
  std::cerr << "Sending message: " << msg_str << std::endl;
  std::cout << msg_str;
  std::cout.flush();
}

void start_handshake(const std::string & name) {
  json msg = {
    {"me", name}
  };
  send_message(msg);
}

void reply_ready(int punter, const json & state) {
  json message = {
    {"ready", punter},
    {"state", state}
  };
  send_message(message);
}

void reply_move(int punter, const json & state) {
  json move;
  move["state"] = state;
  move["move"]["pass"]["punter"] = punter;
  send_message(move);
}

int main(int arfc, char ** arfv) {
  std::string name, response_str;

  int state_punter;

  if (arfc == 2) {
    name = arfv[1];
  } else {
    name = DEFAULTNAME;
  }

  std::cerr << "Launching with name: " << name << std::endl;

  // do the handshake
  start_handshake(name);
  response_str = json_from_stdin();
  std::cerr << "Handshake response: " << response_str << std::endl;
  auto response = json::parse(response_str);
  std::cerr << "Parsed response: " << response << std::endl;
  std::cerr << "Server called us " << response["you"] << std::endl;

  response_str = json_from_stdin();
  std::cerr << "Received server message: " << response_str << std::endl;
  response = json::parse(response_str);
  std::cerr << "Parsed server message: " << response << std::endl;

  try {
    state_punter = response.at("punter");
    response.at("punters");
    response.at("map");

    std::cerr << "Replying with READY" << std::endl;
    reply_ready(state_punter, response);
  } catch (std::out_of_range) {
    // get these things from state
    try {
      json moves = response.at("move");

      state_punter = response.at("state").at("punter");
      std::cerr << "Replying with MOVE + STATE" << std::endl;
      reply_move(state_punter, response.at("state"));
    } catch (std::out_of_range) {
      try {
        json stop = response.at("stop");
        std::cerr << "Moves: " << stop.at("moves") << std::endl;
        std::cerr << "Scores: " << stop.at("scores") << std::endl;
      } catch (std::out_of_range) {
        // must be a timeout?
        std::cerr << "how did we even get down here :(" << std::endl;
      }
    }
  }

  std::cout.flush();
  std::cerr.flush();

  return 0;
}

