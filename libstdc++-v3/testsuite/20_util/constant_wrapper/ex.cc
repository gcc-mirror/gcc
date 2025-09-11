// { dg-do compile { target c++26 } }
#include <type_traits>
#include <iostream>

#include <testsuite_hooks.h>

constexpr auto
initial_phase(auto quantity_1, auto quantity_2)
{ return quantity_1 + quantity_2; }

constexpr auto
middle_phase(auto tbd)
{ return tbd; }

constexpr void
final_phase(auto gathered, auto available)
{
  if constexpr (gathered == available)
    std::cout << "Profit!\n";
}

void
impeccable_underground_planning()
{
  auto gathered_quantity = middle_phase(initial_phase(std::cw<42>, std::cw<13>));
  static_assert(gathered_quantity == 55);
  auto all_available = std::cw<55>;
  final_phase(gathered_quantity, all_available);
}

void
deeply_flawed_underground_planning()
{
  constexpr auto gathered_quantity = middle_phase(initial_phase(42, 13));
  constexpr auto all_available = 55;
  final_phase(gathered_quantity, all_available);  // { dg-error "required from here" }
}

// { dg-prune-output "'gathered' is not a constant expression" }
