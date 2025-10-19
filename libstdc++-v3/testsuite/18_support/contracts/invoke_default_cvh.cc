// { dg-options "-fcontracts -fcontract-evaluation-semantic=observe" }
// { dg-do run { target c++26 } }

#include <contracts>
#include <testsuite_hooks.h>

bool custom_called = false;


void handle_contract_violation(const std::contracts::contract_violation& v)
{
  invoke_default_contract_violation_handler(v);
  custom_called = true;
}

void f(int i) pre (i>10) {};

int main()
{
  f(0);
  VERIFY(custom_called);
}
// { dg-output "contract violation in function void f.int. at .*(\n|\r\n|\r)" }
