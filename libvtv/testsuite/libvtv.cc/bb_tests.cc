// { dg-do run }
struct base
{
  int total;
  virtual void add (int i) { total += i; }
  virtual void sub (int i) { total -= i; }
  virtual void init (void) { total = 73; }
};

struct derived : public base
{
  int total;
  virtual void add (int i) { total += 10 * i; }
  virtual void sub (int i) { total -= 2 * i; }
  virtual void init (void) { total = 0; }
};

bool
get_cond_value (int x)
{
  if ((x % 3) > 0)
    return true;
  else
    return false;

  return false;
}

int
main (int argc, char **argv)
{
  base *a;
  bool cond_value = get_cond_value (10);
  int x;

  if (cond_value)
    a = new base ();
  else
    a = new derived ();

  cond_value = get_cond_value (47);
  x = 0;
  if (!cond_value)
    x = 17;

  a->init ();

  for ( ; x < 10; ++x)
    {
      a->add(50);
      a->sub(25);
    }
}
