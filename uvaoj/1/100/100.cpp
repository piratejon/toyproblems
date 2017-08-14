#ifndef ONLINE_JUDGE
#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#endif // ONLINE_JUDGE

#include <list>
#include <map>

// stores n => steps
// in the example given, 22 takes 16 steps to get to 1
std::map<int, int> solved;

int operate(const int input) {
  if (input == 1) {
    return 0;
  } else if (input % 2 == 0) {
    return input / 2;
  }

  return (3 * input) + 1;
}

int operate_loop(const int input) {
  int i, n = input;

  for (i = 1; n != 1; i += 1) {
    n = operate(n);
  }

  return i;
}

int loop_with_cache(const int input, int & i) {
  int n = input, j;

  std::list<int> unsolved;

  std::map<int, int>::iterator it;

  for (j = 0, i = 0; n != 1; j += 1, i += 1) {
    it = solved.find(n);
    if (it != solved.end()) {
      j = it->second;
      break;
    } else {
      unsolved.push_back(n);
      n = operate(n);
    }
  }

  while (unsolved.size() > 0) {
    solved[unsolved.back()] = j + unsolved.size();
    unsolved.pop_back();
  }

  return solved[input];
}

#ifndef ONLINE_JUDGE
TEST_CASE("produces the next number", "[produce]") {
  REQUIRE(operate(22) == 11);
  REQUIRE(operate(11) == 34);
  REQUIRE(operate(34) == 17);
  REQUIRE(operate(1) == 0);
}

TEST_CASE("count to 1", "[loop]") {
  int n = 22, i = 0;
  for (i = 1; n != 1; i += 1) {
    n = operate(n);
  }
  REQUIRE(i == 16);
}

TEST_CASE("looping driver", "[loop]") {
  REQUIRE(16 == operate_loop(22));
}

TEST_CASE("loop with cache", "[loop]") {
  int i;
  REQUIRE(solved.end() == solved.find(22));
  REQUIRE(16 == loop_with_cache(22, i));
  REQUIRE(solved.end() != solved.find(22));
  REQUIRE(16 == solved.find(22)->second);
  REQUIRE(15 == i);
  REQUIRE(16 == loop_with_cache(22, i));
  REQUIRE(0 == i);
  REQUIRE(16 == loop_with_cache(22, i));
  REQUIRE(0 == i);
  REQUIRE(17 == loop_with_cache(44, i));
  REQUIRE(1 == i);
}
#endif // ONLINE_JUDGE

#ifdef ONLINE_JUDGE
int main(int arfc, char ** arfv) {
  return 0;
}
#endif // ONLINE_JUDGE
