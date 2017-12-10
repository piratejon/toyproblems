#include <iostream>
#include <cstdlib>

#include "KnotNode.h"
#include "Knot.h"

int main(int arfc, char ** arfv) {

  int knotLength = 5;

  if (arfc == 2) {
    knotLength = std::atoi(arfv[1]);
  }
  std::cout << "Building knot of length " << knotLength << "\n";

  Knot<int> k;
  int n;
  while (std::cin >> n) {
    k.append(n);
    std::cout << "appended " << n << "\n";
  }

  std::cout << k;

  return 0;
}
