// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file basic_multimap_example.cpp
 * A basic example showing how to use multimaps.
 */

/**
 * This example shows how to use "multimaps" in the context of a simple
 * bank account application. Each customer holds a bank account
 * (or more than one) which holds some balance.
 */

#include <iostream>
#include <string>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace __gnu_pbds;

// A simple hash functor.
// hash could serve instead of this functor, but it is not yet
// standard everywhere.
struct string_hash
{
  size_t
  operator()(const string& r_s) const
  {
    size_t ret = 0;
    string::const_iterator b = r_s.begin();
    string::const_iterator e = r_s.end();
    while (b != e)
      {
	ret *= 5;
	ret += static_cast<size_t>(*(b++));
      }
    return ret;
  }
};

int main()
{
  // Each customer is identified by a string.
  typedef string customer;

  // Each account is identified by an unsigned long.
  typedef unsigned long account_id;

  // The balance in the account is a floating point.
  typedef float balance_t;

  /*
   *  This is the data structure type used for storing information
   *  about accounts. In this case the primary key is the customer,
   *  and the secondary key is the account id.
   *
   *  A hash-based container maps each customer to a list-based
   *  container that maps each account to the balance it holds.
   *
   *  Note that we could use any combination of primary and secondary
   *  associative-containers. In this case we choose a hash-based
   *  container for the primary keys, since we do not need to store
   *  customers in a sorted order; we choos a list-based container for
   *  the secondary keys, since we expect that the average number of
   *  accounts per customer will be small.
   */
  typedef
    cc_hash_table<
    customer,
    list_update<
    account_id,
    balance_t>,
    string_hash>
    accounts_t;

  // This object will hold all information.
  accounts_t acc;

  // Customer "a" opens empty account 12.
  acc["a"][12] = 0;

  // Customer "a" deposits 45 into account 12.
  acc["a"][12] += 45;

  // Customer "b" opens account 13 with balance 12.3.
  acc["b"][13] = 12.3;

  // Customer "c" opens empty account 14.
  acc["c"][14] = 0;

  // Customer "a" opens account 160 with balance 142.
  // Note that "a" already holds account 12.
  acc["a"][160] = 142;

  // Verify the number of accounts that "a" holds.
  accounts_t::point_const_iterator it = acc.find("a");
  assert(it != acc.end());
  assert(it->second.size() == 2);

  // The beginning of the month has arrived. We need to give a 3%
  // interest to all accounts with a positive balance.

  // First we loop over all customers.
  accounts_t::iterator cust_it;
  for (cust_it = acc.begin(); cust_it != acc.end(); ++cust_it)
    {
      // For each customer, we loop over the customer's accounts.
      accounts_t::mapped_type::iterator it;
      for (it = cust_it->second.begin(); it != cust_it->second.end(); ++it)
	if (it->second > 0)
	  it->second *= 1.03;
    }

  // Customer "a" closes all accounts.
  acc.erase("a");

  // The bank now has only 2 customers.
  assert(acc.size() == 2);

  return 0;
}

