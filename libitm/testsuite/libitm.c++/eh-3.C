// A handler cannot do the reverse of a transaction-safety conversion.
// { dg-do run }
// { dg-options "-fgnu-tm" }

extern "C" void abort();

void g() {}

int main()
{
  try { throw g; }
  catch (void (*p)() transaction_safe) { abort(); }
  catch (...) { }
}
