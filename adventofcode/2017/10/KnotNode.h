#if ! defined(_JS_AOC_KNOTNODE)
#define _JS_AOC_KNOTNODE

#include <iostream>

template <typename T>
class KnotNode {
  public:
    T t;
    KnotNode * next, * prev;

    KnotNode<T>(T t) {
      this->t = t;
      this->next = this;
      this->prev = this;
    }

    friend std::ostream& operator<<(std::ostream& os, const KnotNode<T>& kn) {
      os << kn.t;
      return os;
    }
};
#endif // _JS_AOC_KNOTNODE
