// { dg-do compile }

#define STR1(X) #X
#define STR2(X) STR1(X)
#define LABEL(X) STR2(__USER_LABEL_PREFIX__) X

struct Iter
{
  int *cursor;

  void ctor (int *cursor_) asm (LABEL ("_ZN4IterC1EPi"));
  int *point () const asm (LABEL ("_ZNK4Iter5pointEv"));
};

#pragma acc routine
void Iter::ctor (int *cursor_)
{
  cursor = cursor_;
}

#pragma acc routine
int *Iter::point () const
{
  return cursor;
}

void apply (int (*fn)(), Iter out) asm (LABEL ("_ZN5Apply5applyEPFivE4Iter"));

#pragma acc routine
void apply (int (*fn)(), struct Iter out)
{ *out.point() = fn (); }

extern "C" void __gxx_personality_v0 ()
{
}
