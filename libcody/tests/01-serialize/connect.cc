
// Test client connection handshake
// RUN: <<HELLO 1 TESTING
// RUN: $subdir$stem | ezio -p OUT $test |& ezio -p ERR $test
// RUN-END:

// OUT-NEXT:^HELLO {:[0-9]+} TEST IDENT$
// OUT-NEXT:$EOF

// ERR-NEXT:Code:{:[0-9]+}$
// ERR-NEXT:Version:1$
// ERR-NEXT:$EOF


// Cody
#include "cody.hh"
// C++
#include <iostream>

using namespace Cody;

int main (int, char *[])
{
  Client client (0, 1);

  auto token = client.Connect ("TEST", "IDENT");

  std::cerr << "Code:" << token.GetCode () << '\n';
  std::cerr << "Version:" << token.GetInteger () << '\n';
}
