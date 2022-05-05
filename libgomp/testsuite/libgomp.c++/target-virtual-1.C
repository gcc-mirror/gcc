/* { dg-do run } */
/* Check that classes with virtual member functions works,
   when using it as declared type. */
struct base {
    float data [100];

    base() = default;
    virtual ~base() = default;
};

struct derived : public base {
    int scalar, array[5];

    derived() = default;
    void do_work ()
    {
      int error = 0;
      #pragma omp target map (tofrom: this[:1], error)
      {
	if (scalar != 42 || this->array[0] != 123 || array[4] != 555)
	  error = 1;
	if (data[0] != 333 || data[99] != -3)
	  error = 1;
	this->scalar = 99;
	array[0] = 5;
	array[4] = -4;
	this->data[0] = 11;
	this->data[99] = 99;
      }
      if (error)
	__builtin_abort ();
      if (data[0] != 11 || data[99] != 99)
	__builtin_abort ();
      if (scalar != 99 || array[0] != 5 || array[4] != -4)
	__builtin_abort ();
    }   
};

int
main ()
{
  struct derived x;
  x.data[0] = 333;
  x.data[99] = -3;
  x.scalar = 42;
  x.array[0] = 123;
  x.array[4] = 555;
  x.do_work ();
  return 0;
}
