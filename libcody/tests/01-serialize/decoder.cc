// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// RUN: <<bob 'frob dob''\nF\_b\20\61\\'
// RUN: $subdir$stem |& ezio $test
// CHECK-NEXT: ^line:0 word:0 'bob'
// CHECK-NEXT: ^line:0 word:1 'frob dob$
// CHECK-OPTION: matchSpace
// CHECK-NEXT: ^F b a\'$
// CHECK-NEXT: $EOF

/* RUN: <<line-1 word:1 ;
   RUN: <<'line 2' ;
   RUN: <<
*/
// RUN: $subdir$stem |& ezio -p CHECK2 $test
// CHECK2-NEXT: line:0 word:0 'line-1'
// CHECK2-NEXT: line:0 word:1 'word:1'
// CHECK2-NEXT: line:1 word:0 'line 2'
// CHECK2-NEXT: error:No 
// CHECK2-NEXT: $EOF

// RUN: <<'
// RUN: $subdir$stem |& ezio -p CHECK3 $test
// CHECK3-NEXT: error:Invalid argument
// CHECK3-NEXT: line:0 word:0 '''
// CHECK3-NEXT: $EOF

/* RUN: << ;
   RUN: <<'\g'
*/
// RUN: $subdir$stem |& ezio -p CHECK4 $test
// CHECK4-NEXT: error:No 
// CHECK4-NEXT: error:Invalid argument
// CHECK4-NEXT: line:1 word:0 ''\g''
// CHECK4-NEXT: $EOF

// RUN-END:

// Cody
#include "cody.hh"
// C++
#include <iostream>
// C
#include <cstring>

using namespace Cody;

int main (int, char *[])
{
  Detail::MessageBuffer reader;

  reader.PrepareToRead ();
  while (int e = reader.Read (0))
    if (e != EAGAIN && e != EINTR)
      break;

  std::vector<std::string> words;
  for (unsigned line = 0; !reader.IsAtEnd (); line++)
    {
      if (int e = reader.Lex (words))
	std::cerr << "error:" << strerror (e) << '\n';
      for (unsigned ix = 0; ix != words.size (); ix++)
	{
	  auto &word = words[ix];

	  std::cerr << "line:" << line << " word:" << ix
		    << " '" << word << "'\n";
	}
    }
  return 0;
}
