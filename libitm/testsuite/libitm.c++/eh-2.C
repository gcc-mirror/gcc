// A handler can involve a transaction-safety conversion.
// { dg-do run }
// { dg-options "-fgnu-tm" }

void g() transaction_safe {}
int main()
{
  try { throw g; }
  catch (void (*p)()) { }
}
