/* This is part of libio/iostream, providing -*- C++ -*- input/output.
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

/* 
  a very simple implementation of a class to output unix "plot"
  format plotter files. See corresponding unix man pages for
  more details. 

  written by Doug Lea (dl@rocky.oswego.edu)
  converted to use iostream library by Per Bothner (bothner@cygnus.com)
*/

#ifndef _PlotFile_h
#ifdef __GNUG__
#pragma interface
#endif
#define _PlotFile_h

#include <fstream.h>

/*   
   Some plot libraries have the `box' command to draw boxes. Some don't.
   `box' is included here via moves & lines to allow both possiblilties.
*/

extern "C++" {
class PlotFile : public ofstream
{
protected:
  PlotFile& cmd(char c);
  PlotFile& operator << (const int x);
  PlotFile& operator << (const char *s);
  
public:
  
  PlotFile() : ofstream() { }
  PlotFile(int fd) : ofstream(fd) { }
  PlotFile(const char *name, int mode=ios::out, int prot=0664)
      : ofstream(name, mode, prot) { }
  
//  PlotFile& remove() { ofstream::remove(); return *this; }
  
//  int           filedesc() { return ofstream::filedesc(); }
//  const char*   name() { return File::name(); }
//  void          setname(const char* newname) { File::setname(newname); }
//  int           iocount() { return File::iocount(); }
  
  PlotFile& arc(const int xi, const int yi,
                const int x0, const int y0,
                const int x1, const int y1);
  PlotFile& box(const int x0, const int y0,
                const int x1, const int y1);
  PlotFile& circle(const int x, const int y, const int r);
  PlotFile& cont(const int xi, const int yi);
  PlotFile& dot(const int xi, const int yi, const int dx,
                int n, const int* pat);
  PlotFile& erase(); 
  PlotFile& label(const char* s);
  PlotFile& line(const int x0, const int y0,
                 const int x1, const int y1);
  PlotFile& linemod(const char* s);
  PlotFile& move(const int xi, const int yi);
  PlotFile& point(const int xi, const int yi);
  PlotFile& space(const int x0, const int y0,
                  const int x1, const int y1);
};
} // extern "C++"
#endif
