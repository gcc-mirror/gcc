// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// Cody
#include "internal.hh"

namespace Cody {

void Packet::Destroy ()
{
  switch (cat)
    {
    case STRING:
      // Silly scope destructor name rules
      using S = std::string;
      string.~S ();
      break;

    case VECTOR:
      using V = std::vector<std::string>;
      vector.~V ();
      break;

    default:;
    }
}

void Packet::Create (Packet &&t)
{
  cat = t.cat;
  code = t.code;
  request = t.request;
  switch (cat)
    {
    case STRING:
      new (&string) std::string (std::move (t.string));
      break;

    case VECTOR:
      new (&vector) std::vector<std::string> (std::move (t.vector));
      break;

    default:
      integer = t.integer;
      break;
    }
}

}
