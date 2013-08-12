// Small test case from povray, see if it reproduces.

#include <stdio.h>

class POVMS_MessageReceiver
{

private:
   int x;
   class Handler
   {
      public:
         virtual void print() = 0;
   };
protected:
   template<class T> class MemberHandler : public Handler
   {
   public:
      MemberHandler(T *xx)
      {
        x = xx;
      }
         
      ~MemberHandler() {}
    
      void print()
      {
         printf("In print\n");
      }
  private:
      T *x;
   };

private:
   struct HandlerNode
   {
      Handler *handler;
   };

   HandlerNode *receiver;
public:
   POVMS_MessageReceiver(int xx) : x(xx) {}
   ~POVMS_MessageReceiver() {}

   void foo(int *xx);
   void try_call();
};

void POVMS_MessageReceiver::foo(int *xx)
{
   receiver = new HandlerNode;

   receiver->handler = new MemberHandler<int>(xx);
}

void POVMS_MessageReceiver::try_call()
{
   receiver->handler->print();
}


int main()
{
   int loc = 34;
   POVMS_MessageReceiver a_test(100);

   a_test.foo(&loc);
   a_test.try_call();
}



