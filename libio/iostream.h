/*  This is part of libio/iostream, providing -*- C++ -*- input/output.
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

#ifndef _IOSTREAM_H
#ifdef __GNUG__
#pragma interface
#endif
#define _IOSTREAM_H

#include <streambuf.h>

extern "C++" {
class istream; class ostream;
typedef ios& (*__manip)(ios&);
typedef istream& (*__imanip)(istream&);
typedef ostream& (*__omanip)(ostream&);

extern istream& ws(istream& ins);
extern ostream& flush(ostream& outs);
extern ostream& endl(ostream& outs);
extern ostream& ends(ostream& outs);

class ostream : virtual public ios
{
    // NOTE: If fields are changed, you must fix _fake_ostream in stdstreams.C!
    void do_osfx();
  public:
    ostream() { }
    ostream(streambuf* sb, ostream* tied=NULL);
    int opfx() {
	if (!good()) return 0;
	else { if (_tie) _tie->flush(); _IO_flockfile(_strbuf); return 1;} }
    void osfx() { _IO_funlockfile(_strbuf);
		  if (flags() & (ios::unitbuf|ios::stdio))
		      do_osfx(); }
    ostream& flush();
    ostream& put(char c) { _strbuf->sputc(c); return *this; }
#ifdef _STREAM_COMPAT
    /* Temporary binary compatibility.  REMOVE IN NEXT RELEASE. */
    ostream& put(unsigned char c) { return put((char)c); }
    ostream& put(signed char c) { return put((char)c); }
#endif
    ostream& write(const char *s, streamsize n);
    ostream& write(const unsigned char *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& write(const signed char *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& write(const void *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& seekp(streampos);
    ostream& seekp(streamoff, _seek_dir);
    streampos tellp();
    ostream& form(const char *format ...);
    ostream& vform(const char *format, _IO_va_list args);

    ostream& operator<<(char c);
    ostream& operator<<(unsigned char c) { return (*this) << (char)c; }
    ostream& operator<<(signed char c) { return (*this) << (char)c; }
    ostream& operator<<(const char *s);
    ostream& operator<<(const unsigned char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(const signed char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(const void *p);
    ostream& operator<<(int n);
    ostream& operator<<(unsigned int n);
    ostream& operator<<(long n);
    ostream& operator<<(unsigned long n);
#if defined(__GNUC__)
    __extension__ ostream& operator<<(long long n);
    __extension__ ostream& operator<<(unsigned long long n);
#endif
    ostream& operator<<(short n) {return operator<<((int)n);}
    ostream& operator<<(unsigned short n) {return operator<<((unsigned int)n);}
#if _G_HAVE_BOOL
    ostream& operator<<(bool b) { return operator<<((int)b); }
#endif
    ostream& operator<<(double n);
    ostream& operator<<(float n) { return operator<<((double)n); }
#if _G_HAVE_LONG_DOUBLE_IO
    ostream& operator<<(long double n);
#else
    ostream& operator<<(long double n) { return operator<<((double)n); }
#endif
    ostream& operator<<(__omanip func) { return (*func)(*this); }
    ostream& operator<<(__manip func) {(*func)(*this); return *this;}
    ostream& operator<<(streambuf*);
#ifdef _STREAM_COMPAT
    streambuf* ostreambuf() const { return _strbuf; }
#endif
};

class istream : virtual public ios
{
    // NOTE: If fields are changed, you must fix _fake_istream in stdstreams.C!
protected:
    _IO_size_t _gcount;

    int _skip_ws();
  public:
    istream(): _gcount (0) { }
    istream(streambuf* sb, ostream*tied=NULL);
    istream& get(char* ptr, int len, char delim = '\n');
    istream& get(unsigned char* ptr, int len, char delim = '\n')
	{ return get((char*)ptr, len, delim); }
    istream& get(char& c);
    istream& get(unsigned char& c) { return get((char&)c); }
    istream& getline(char* ptr, int len, char delim = '\n');
    istream& getline(unsigned char* ptr, int len, char delim = '\n')
	{ return getline((char*)ptr, len, delim); }
    istream& get(signed char& c)  { return get((char&)c); }
    istream& get(signed char* ptr, int len, char delim = '\n')
	{ return get((char*)ptr, len, delim); }
    istream& getline(signed char* ptr, int len, char delim = '\n')
	{ return getline((char*)ptr, len, delim); }
    istream& read(char *ptr, streamsize n);
    istream& read(unsigned char *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& read(signed char *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& read(void *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& get(streambuf& sb, char delim = '\n');
    istream& gets(char **s, char delim = '\n');
    int ipfx(int need = 0) {
	if (!good()) { set(ios::failbit); return 0; }
	else {
	  _IO_flockfile(_strbuf);
	  if (_tie && (need == 0 || rdbuf()->in_avail() < need)) _tie->flush();
	  if (!need && (flags() & ios::skipws)) return _skip_ws();
	  else return 1;
	}
    }
    int ipfx0() { // Optimized version of ipfx(0).
	if (!good()) { set(ios::failbit); return 0; }
	else {
	  _IO_flockfile(_strbuf);
	  if (_tie) _tie->flush();
	  if (flags() & ios::skipws) return _skip_ws();
	  else return 1;
	}
    }
    int ipfx1() { // Optimized version of ipfx(1).
	if (!good()) { set(ios::failbit); return 0; }
	else {
	  _IO_flockfile(_strbuf);
	  if (_tie && rdbuf()->in_avail() == 0) _tie->flush();
	  return 1;
	}
    }
    void isfx() { _IO_funlockfile(_strbuf); }
    int get() { if (!ipfx1()) return EOF;
		else { int ch = _strbuf->sbumpc();
		       if (ch == EOF) set(ios::eofbit);
		       return ch;
		     } }
    int peek();
    _IO_size_t gcount() { return _gcount; }
    istream& ignore(int n=1, int delim = EOF);
    int sync ();
    istream& seekg(streampos);
    istream& seekg(streamoff, _seek_dir);
    streampos tellg();
    istream& putback(char ch) {
	if (good() && _strbuf->sputbackc(ch) == EOF) clear(ios::badbit);
	return *this;}
    istream& unget() {
	if (good() && _strbuf->sungetc() == EOF) clear(ios::badbit);
	return *this;}
    istream& scan(const char *format ...);
    istream& vscan(const char *format, _IO_va_list args);
#ifdef _STREAM_COMPAT
    istream& unget(char ch) { return putback(ch); }
    int skip(int i);
    streambuf* istreambuf() const { return _strbuf; }
#endif

    istream& operator>>(char*);
    istream& operator>>(unsigned char* p) { return operator>>((char*)p); }
    istream& operator>>(signed char*p) { return operator>>((char*)p); }
    istream& operator>>(char& c);
    istream& operator>>(unsigned char& c) {return operator>>((char&)c);}
    istream& operator>>(signed char& c) {return operator>>((char&)c);}
    istream& operator>>(int&);
    istream& operator>>(long&);
#if defined(__GNUC__)
    __extension__ istream& operator>>(long long&);
    __extension__ istream& operator>>(unsigned long long&);
#endif
    istream& operator>>(short&);
    istream& operator>>(unsigned int&);
    istream& operator>>(unsigned long&);
    istream& operator>>(unsigned short&);
#if _G_HAVE_BOOL
    istream& operator>>(bool&);
#endif
    istream& operator>>(float&);
    istream& operator>>(double&);
    istream& operator>>(long double&);
    istream& operator>>( __manip func) {(*func)(*this); return *this;}
    istream& operator>>(__imanip func) { return (*func)(*this); }
    istream& operator>>(streambuf*);
};

class iostream : public istream, public ostream
{
  public:
    iostream() { }
    iostream(streambuf* sb, ostream*tied=NULL);
};

class _IO_istream_withassign : public istream {
public:
  _IO_istream_withassign& operator=(istream&);
  _IO_istream_withassign& operator=(_IO_istream_withassign& rhs)
    { return operator= (static_cast<istream&> (rhs)); }
};

class _IO_ostream_withassign : public ostream {
public:
  _IO_ostream_withassign& operator=(ostream&);
  _IO_ostream_withassign& operator=(_IO_ostream_withassign& rhs)
    { return operator= (static_cast<ostream&> (rhs)); }
};

extern _IO_istream_withassign cin;
// clog->rdbuf() == cerr->rdbuf()
extern _IO_ostream_withassign cout, cerr;

extern _IO_ostream_withassign clog
#if _G_CLOG_CONFLICT
__asm__ ("__IO_clog")
#endif
;

extern istream& lock(istream& ins);
extern istream& unlock(istream& ins);
extern ostream& lock(ostream& outs);
extern ostream& unlock(ostream& outs);

struct Iostream_init { } ;  // Compatibility hack for AT&T library.

inline ios& dec(ios& i)
{ i.setf(ios::dec, ios::dec|ios::hex|ios::oct); return i; }
inline ios& hex(ios& i)
{ i.setf(ios::hex, ios::dec|ios::hex|ios::oct); return i; }
inline ios& oct(ios& i)
{ i.setf(ios::oct, ios::dec|ios::hex|ios::oct); return i; }
} // extern "C++"

#endif /*!_IOSTREAM_H*/
