/* 
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988, 1992, 1993 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)
    converted to use iostream library by Per Bothner (bothner@cygnus.com)

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with GCC to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License.  */

#ifdef __GNUG__
#pragma implementation
#endif
#include <PlotFile.h>

/*
 PlotFile implementation module
*/


PlotFile& PlotFile:: cmd(char c)
{ 
  ofstream::put(c); 
  return *this; 
}

PlotFile& PlotFile:: operator<<(const int x)
{ 
#if defined(convex)
  ofstream::put((char)(x>>8)); 
  ofstream::put((char)(x&0377)); 
#else
  ofstream::put((char)(x&0377)); 
  ofstream::put((char)(x>>8)); 
#endif
  return *this; 
}

PlotFile& PlotFile:: operator<<(const char *s)
{ 
  *(ofstream*)this << s;
  return *this;
}


PlotFile& PlotFile:: arc(const int xi, const int yi,
			 const int x0, const int y0,
			 const int x1, const int y1)
{ 
  return cmd('a') << xi << yi << x0 << y0 << x1 << y1; 
}


PlotFile& PlotFile:: box(const int x0, const int y0,
			 const int x1, const int y1)
{ 
  line(x0, y0, x0, y1);
  line(x0, y1, x1, y1);
  line(x1, y1, x1, y0);
  return line(x1, y0, x0, y0);
}

PlotFile& PlotFile:: circle(const int x, const int y, const int r)
{ 
  return cmd('c') << x << y << r; 
}

PlotFile& PlotFile:: cont(const int xi, const int yi)
{ 
  return cmd('n') << xi << yi;
}

PlotFile& PlotFile:: dot(const int xi, const int yi, const int dx,
			 int n, const int* pat)
{ 
  cmd('d') << xi << yi << dx << n;
  while (n-- > 0) *this << *pat++;
  return *this; 
}

PlotFile& PlotFile:: erase()
{ 
  return cmd('e'); 
}

PlotFile& PlotFile:: label(const char* s)
{ 
  return cmd('t') << s << "\n"; 
}

PlotFile& PlotFile:: line(const int x0, const int y0,
			  const int x1, const int y1)
{ 
  return cmd('l') << x0 << y0 << x1 << y1; 
}

PlotFile& PlotFile:: linemod(const char* s)
{ 
  return cmd('f') << s << "\n"; 
}

PlotFile& PlotFile:: move(const int xi, const int yi)
{ 
  return cmd('m') << xi << yi;
}

PlotFile& PlotFile:: point(const int xi, const int yi)
{ 
  return cmd('p') << xi << yi; 
}

PlotFile& PlotFile:: space(const int x0, const int y0,
			   const int x1, const int y1)
{ 
  return cmd('s') << x0 << y0 << x1 << y1; 
}
