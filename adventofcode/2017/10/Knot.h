#if ! defined(_JS_AOC_KNOT)
#define _JS_AOC_KNOT

#include <iostream>

#include "KnotNode.h"

template <typename T>
class Knot {
  // circular LL with extra "hashing" methods
  public:
    KnotNode<T> * head;
    int length;

    Knot<T>() {
      head = NULL;
      length = 0;
    }

    ~Knot<T>() {
      if (length > 0) {
        Knot<T>::const_iterator i = this->begin();
        do {
          KnotNode<T> * n = &(*i);
          ++ i;
          delete n;
        } while(i != this->end());
      }
    }

    KnotNode<T> * append(T t) {
      KnotNode<T> * newnode = new KnotNode<T>(t);

      length += 1;

      if (!head) {
        head = newnode;
      }

      newnode->prev = head->prev;
      newnode->next = head;
      newnode->prev->next = newnode;
      head->prev = newnode;

      return newnode;
    }

    friend std::ostream& operator<< (std::ostream& os, Knot<T>& k) {
      if (k.length > 0) {
        Knot<T>::const_iterator i = k.begin();
        do {
          os << "iter'd: " << *i << "\n";
          ++ i;
        } while(i != k.end());
      }
      return os;
    }

    friend std::istream& operator>>(std::istream& is, Knot<T>& k) {
      T t;
      is >> t;
      k.append(t);
      std::cout << "appended " << t;
      return is;
    }

    class const_iterator {
      public:
        typedef const_iterator self_type;
        typedef KnotNode<T> value_type;
        typedef value_type& reference;
        typedef value_type* pointer;
        typedef int difference_type;
        typedef std::forward_iterator_tag iterator_category;
        const_iterator(pointer ptr) : ptr_(ptr) {}
        self_type operator++() {
          self_type i = *this;
          if (ptr_) {
            ptr_ = ptr_->next;
          }
          return i;
        }
        self_type operator--() {
          self_type i = *this;
          if (ptr_) {
            ptr_ = ptr_->prev;
          }
          return i;
        }
        // self_type operator++(int junk) { ptr_ = ptr_->next; return *this; }
        reference operator*() { return *ptr_; }
        const pointer operator->() { return ptr_; }
        bool operator==(const self_type& rhs) { return ptr_ == rhs.ptr_; }
        bool operator!=(const self_type& rhs) { return ptr_ != rhs.ptr_; }
      private:
        pointer ptr_;
    };

    const_iterator begin() {
      return const_iterator(head);
    }

    const_iterator end() {
      return const_iterator(head);
    }
};

#endif // _JS_AOC_KNOT
