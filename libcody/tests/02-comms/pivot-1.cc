
// Test resolver pivot

// RUN:<<HELLO 1 TEST IDENT ;
// RUN:<<MODULE-REPO ;
// RUN:<<HELLO 1 TEST IDENT
// RUN: $subdir$stem | ezio -p OUT1 $test |& ezio -p ERR1 $test
// OUT1-NEXT:HELLO 1 default ;
// OUT1-NEXT:PATHNAME cmi.cache ;
// OUT1-NEXT:ERROR 'already connected
// OUT1-NEXT:$EOF
// ERR1-NEXT:resolver is handler
// ERR1-NEXT:$EOF

// RUN:<<MODULE-REPO ;
// RUN:<<HELLO 1 TEST IDENT ;
// RUN:<<MODULE-REPO
// RUN: $subdir$stem | ezio -p OUT2 $test |& ezio -p ERR2 $test
// OUT2-NEXT:ERROR 'not connected
// OUT2-NEXT:HELLO 1 default ;
// OUT2-NEXT:PATHNAME cmi.cache
// OUT2-NEXT:$EOF
// ERR2-NEXT:resolver is handler
// ERR2-NEXT:$EOF

// RUN-END:
#include "cody.hh"
#include <iostream>

using namespace Cody;

class Handler : public Resolver
{
  virtual Handler *ConnectRequest (Server *s, unsigned ,
				   std::string &, std::string &)
  {
    ErrorResponse (s, "unexpected connect call");
    return nullptr;
  }
};

Handler handler;

class Initial : public Resolver
{
  virtual Handler *ConnectRequest (Server *s, unsigned v,
				   std::string &agent, std::string &ident)
  {
    Resolver::ConnectRequest (s, v, agent, ident);
    return &handler;
  }
};

Initial initial;

int main (int, char *[])
{
  Server server (&initial, 0, 1);

  while (int e = server.Read ())
    if (e != EAGAIN && e != EINTR)
      break;

  server.ProcessRequests ();
  if (server.GetResolver () == &handler)
    std::cerr << "resolver is handler\n";
  else if (server.GetResolver () == &initial)
    std::cerr << "resolver is initial\n";
  else
    std::cerr << "resolver is surprising\n";

  server.PrepareToWrite ();
  while (int e = server.Write ())
    if (e != EAGAIN && e != EINTR)
      break;
}
