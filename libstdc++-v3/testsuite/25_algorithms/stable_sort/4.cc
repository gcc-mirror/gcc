// { dg-options "-pedantic" }
// { dg-do compile }

#include <algorithm>

/* This type is reduced from QTypedArrayData::iterator which has an implicit
 * conversion to its pointer type and a difference type of int.
 * The expression Iter() + ptrdiff_t(0) is ambiguous with -pedantic because it
 * could either convert the RHS to int and use Iter::operator+(int)
 * or it could convert the LHS to pointer and use built-in pointer arithmetic.
 */
struct Iter
{
  struct value_type { bool operator<(value_type) const; };
  typedef value_type* pointer;
  typedef value_type& reference;
  typedef std::random_access_iterator_tag iterator_category;
  typedef int difference_type;

  reference operator*() const;
  pointer operator->() const;

  reference operator[](difference_type) const;

  Iter& operator++();
  Iter& operator--();
  Iter operator++(int);
  Iter operator--(int);

  Iter& operator+=(difference_type);
  Iter& operator-=(difference_type);

  Iter operator+(difference_type) const;
  Iter operator-(difference_type) const;

  difference_type operator-(Iter) const;

  operator pointer() const; // XXX this causes the ambiguity

  bool operator==(Iter) const;
  bool operator!=(Iter) const;

  bool operator<(Iter) const;
};

Iter operator+(Iter::difference_type, Iter);

int main()
{
  std::stable_sort(Iter(), Iter());
}
