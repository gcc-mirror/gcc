// Test case for PR127:
// gcj dumps core on method invocation on a primitive type 

class PR127 
{
  void f() 
  {
    int i;
    i.f();
  }
}
