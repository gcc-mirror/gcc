// 2003-07-04  Petur Runolfsson <peturr02@ru.is>
//
// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>

// Contributed as part of libstdc++/11378.
int main(int argc, char** argv)
{
  using namespace std;
  
  if (argc < 3)
    {
      cerr << "Usage: " << argv[0] << " type iters [chunksize]\n";
      return -1;
    }

  enum { type_stdio, type_iostream } type;
  
  if (!strcmp(argv[1], "stdio"))
    type = type_stdio;
  else if (!strcmp(argv[1], "iostream"))
    type = type_iostream;
  else
    {
      cerr << "Type must be one of {stdio, iostream}\n";
      return -1;
    }

  int iters = atoi(argv[2]);
  if (iters < 1)
    {
      cerr << "Iters must be an positive integer\n";
      return -1;
    }
  
  int chunksize = 1;
  if (argc > 3)
    chunksize = atoi(argv[3]);
  
  if (iters < 1)
    {
      cerr << "Chunksize must be an positive integer\n";
      return -1;
    }
  
  char* chunk = 0;
  if (chunksize > 1)
    {
      chunk = new char[chunksize];
      memset(chunk, 'a', chunksize);
    }

  switch (type)
    {
    case type_stdio:
      {
	FILE* f = fopen("tmp", "w");
	setvbuf(f, 0, _IONBF, 0);
	
	if (chunksize > 1)
	  {
	    for (int i = 0; i < iters; ++i)
	      fwrite_unlocked(chunk, 1, chunksize, f);
	  }
	else
	  {
	    for (int i = 0; i < iters; ++i)
	      putc_unlocked('a', f);
	  }
	
	fclose(f);
	break;
      }

    case type_iostream:
      {
	filebuf f;
	f.pubsetbuf(0, 0);
	
	f.open("tmp", ios_base::out);

	if (chunksize > 1)
	  {
	    for (int i = 0; i < iters; ++i)
	      f.sputn(chunk, chunksize);
	  }
	else
	  {
	    for (int i = 0; i < iters; ++i)
	      f.sputc('a');
	  }
	
	f.close();
	
	break;
      }
    }

  delete[] chunk;
  return 0;
}
