
// Test client message round tripping
/*
  RUN: <<HELLO 1 TESTING ;
  RUN: <<PATHNAME REPO ;
  RUN: <<PATHNAME biz/bar ;
  RUN: <<PATHNAME blob ;
  RUN: <<BOOL FALSE ;
  RUN: << BOOL TRUE ;
  RUN: << PATHNAME foo ;
  RUN: <<OK
*/
// RUN: $subdir$stem | ezio -p OUT $test |& ezio -p ERR $test
// RUN-END:

/*
  OUT-NEXT:^HELLO {:[0-9]+} TEST IDENT ;$
  OUT-NEXT:^MODULE-REPO ;
  OUT-NEXT:^MODULE-EXPORT bar ;
  OUT-NEXT:^MODULE-IMPORT foo ;
  OUT-NEXT:^INCLUDE-TRANSLATE baz.frob ;
  OUT-NEXT:^INCLUDE-TRANSLATE ./corge ;
  OUT-NEXT:^INCLUDE-TRANSLATE ./quux ;
  OUT-NEXT:^MODULE-COMPILED bar
*/
// OUT-NEXT:$EOF

// ERR-NEXT:Code:1$
// ERR-NEXT:Integer:1$
// ERR-NEXT:Code:5$
// ERR-NEXT:String:REPO$
// ERR-NEXT:Code:5$
// ERR-NEXT:String:biz/bar$
// ERR-NEXT:Code:5$
// ERR-NEXT:String:blob$
// ERR-NEXT:Code:4$
// ERR-NEXT:Integer:0$
// ERR-NEXT:Code:4$
// ERR-NEXT:Integer:1$
// ERR-NEXT:Code:5$
// ERR-NEXT:String:foo
// ERR-NEXT:Code:3$
// ERR-NEXT:Integer:
// ERR-NEXT:$EOF


// Cody
#include "cody.hh"
// C++
#include <iostream>

using namespace Cody;

int main (int, char *[])
{
  Client client (0, 1);

  client.Cork ();
  if (client.Connect ("TEST", "IDENT").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.ModuleRepo ().GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.ModuleExport ("bar").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.ModuleImport ("foo").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.IncludeTranslate ("baz.frob").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.IncludeTranslate ("./corge").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.IncludeTranslate ("./quux").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";
  if (client.ModuleCompiled ("bar").GetCode () != Client::PC_CORKED)
    std::cerr << "Not corked!\n";

  auto result = client.Uncork ();
  for (auto iter = result.begin (); iter != result.end (); ++iter)
    {
      std::cerr << "Code:" << iter->GetCode () << '\n';
      switch (iter->GetCategory ())
	{
	case Packet::INTEGER:
	  std::cerr << "Integer:" << iter->GetInteger () << '\n';
	  break;
	case Packet::STRING:
	  std::cerr << "String:" << iter->GetString () << '\n';
	  break;
	case Packet::VECTOR:
	  {
	    auto const &v = iter->GetVector ();
	    for (unsigned ix = 0; ix != v.size (); ix++)
	      std::cerr << "Vector[" << ix << "]:" << v[ix] << '\n';
	  }
	  break;
	}
    }
}
