// It is legal to reference "this" from an enclosing type, or an instance 
// field from an enclosing type, in a super constructor call.

public class SuperConstr
{
  SuperConstr (Object x, Outer y) {}
}

class Outer
{
  Object x;
  
  class Sub extends SuperConstr
  {
    Sub()
    {
      super(x, Outer.this);
    }
  }
}
