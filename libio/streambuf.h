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

#ifndef _STREAMBUF_H
#define _STREAMBUF_H
#ifdef __GNUG__
#pragma interface
#endif

/* #define _G_IO_THROW */ /* Not implemented:  ios::failure */

#define _IO_NEW_STREAMS // new optimizated stream representation

extern "C" {
#include <libio.h>
}
//#include <_G_config.h>
#ifdef _G_NEED_STDARG_H
#include <stdarg.h>
#endif
#ifndef _IO_va_list
#define _IO_va_list char *
#endif

#ifndef EOF
#define EOF (-1)
#endif
#ifndef NULL
#ifdef __GNUG__
#define NULL (__null)
#else
#define NULL (0)
#endif
#endif

#ifndef _IO_wchar_t
#if _G_IO_IO_FILE_VERSION == 0x20001
#define _IO_wchar_t _G_wchar_t
#else
#define _IO_wchar_t short
#endif
#endif

extern "C++" {
class istream; /* Work-around for a g++ name mangling bug. Fixed in 2.6. */
class ostream; class streambuf;

// In case some header files defines these as macros.
#undef open
#undef close

#if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
typedef _IO_off64_t streamoff;
typedef _IO_fpos64_t streampos;
#else
typedef _IO_off_t streamoff;
typedef _IO_fpos_t streampos;
#endif
typedef _IO_ssize_t streamsize;

typedef unsigned long __fmtflags;
typedef unsigned char __iostate;

struct _ios_fields
{ // The data members of an ios.
    streambuf *_strbuf;
    ostream* _tie;
    int _width;
    __fmtflags _flags;
    _IO_wchar_t _fill;
    __iostate _state;
    __iostate _exceptions;
    int _precision;

    void *_arrays; /* Support for ios::iword and ios::pword. */
};

#define _IOS_GOOD	0
#define _IOS_EOF	1
#define _IOS_FAIL	2
#define _IOS_BAD	4

#define _IO_INPUT	1
#define _IO_OUTPUT	2
#define _IO_ATEND	4
#define _IO_APPEND	8
#define _IO_TRUNC	16
#define _IO_NOCREATE	32
#define _IO_NOREPLACE	64
#define _IO_BIN		128

#ifdef _STREAM_COMPAT
enum state_value {
    _good = _IOS_GOOD,
    _eof = _IOS_EOF,
    _fail = _IOS_FAIL,
    _bad = _IOS_BAD };
enum open_mode {
    input = _IO_INPUT,
    output = _IO_OUTPUT,
    atend = _IO_ATEND,
    append = _IO_APPEND };
#endif

class ios : public _ios_fields {
  ios& operator=(ios&);  /* Not allowed! */
  ios (const ios&); /* Not allowed! */
  public:
    typedef __fmtflags fmtflags;
    typedef int iostate;
    typedef int openmode;
    typedef int streamsize;
    enum io_state {
	goodbit = _IOS_GOOD,
	eofbit = _IOS_EOF,
	failbit = _IOS_FAIL,
	badbit = _IOS_BAD };
    enum open_mode {
	in = _IO_INPUT,
	out = _IO_OUTPUT,
	ate = _IO_ATEND,
	app = _IO_APPEND,
	trunc = _IO_TRUNC,
	nocreate = _IO_NOCREATE,
	noreplace = _IO_NOREPLACE,
	bin = _IOS_BIN, // Deprecated - ANSI uses ios::binary.
	binary = _IOS_BIN };
    enum seek_dir { beg, cur, end};
    typedef enum seek_dir seekdir;
    // NOTE: If adding flags here, before to update ios::bitalloc().
    enum { skipws=_IO_SKIPWS,
	   left=_IO_LEFT, right=_IO_RIGHT, internal=_IO_INTERNAL,
	   dec=_IO_DEC, oct=_IO_OCT, hex=_IO_HEX,
	   showbase=_IO_SHOWBASE, showpoint=_IO_SHOWPOINT,
	   uppercase=_IO_UPPERCASE, showpos=_IO_SHOWPOS,
	   scientific=_IO_SCIENTIFIC, fixed=_IO_FIXED,
	   unitbuf=_IO_UNITBUF, stdio=_IO_STDIO
#ifndef _IO_NEW_STREAMS
	   , dont_close=_IO_DONT_CLOSE // Don't delete streambuf on stream destruction
#endif
	   };
    enum { // Masks.
	basefield=dec+oct+hex,
	floatfield = scientific+fixed,
	adjustfield = left+right+internal
    };

#ifdef _IO_THROW
    class failure : public xmsg {
	ios* _stream;
      public:
	failure(ios* stream) { _stream = stream; }
	failure(string cause, ios* stream) { _stream = stream; }
	ios* rdios() const { return _stream; }
    };
#endif

    ostream* tie() const { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }

    // Methods to change the format state.
    _IO_wchar_t fill() const { return _fill; }
    _IO_wchar_t fill(_IO_wchar_t newf)
	{_IO_wchar_t oldf = _fill; _fill = newf; return oldf;}
    fmtflags flags() const { return _flags; }
    fmtflags flags(fmtflags new_val) {
	fmtflags old_val = _flags; _flags = new_val; return old_val; }
    int precision() const { return _precision; }
    int precision(int newp) {
	unsigned short oldp = _precision; _precision = (unsigned short)newp;
	return oldp; }
    fmtflags setf(fmtflags val) {
	fmtflags oldbits = _flags;
	_flags |= val; return oldbits; }
    fmtflags setf(fmtflags val, fmtflags mask) {
	fmtflags oldbits = _flags;
	_flags = (_flags & ~mask) | (val & mask); return oldbits; }
    fmtflags unsetf(fmtflags mask) {
	fmtflags oldbits = _flags;
	_flags &= ~mask; return oldbits; }
    int width() const { return _width; }
    int width(int val) { int save = _width; _width = val; return save; }

#ifdef _IO_THROW
    void _throw_failure() const { throw new ios::failure(this); }
#else
    void _throw_failure() const { }
#endif
    void clear(iostate state = 0) {
	_state = _strbuf ? state : state|badbit;
	if (_state & _exceptions) _throw_failure(); }
    void set(iostate flag) { _state |= flag;
	if (_state & _exceptions) _throw_failure(); }
    void setstate(iostate flag) { _state |= flag; // ANSI
	if (_state & _exceptions) _throw_failure(); }
    int good() const { return _state == 0; }
    int eof() const { return _state & ios::eofbit; }
    int fail() const { return _state & (ios::badbit|ios::failbit); }
    int bad() const { return _state & ios::badbit; }
    iostate rdstate() const { return _state; }
    operator void*() const { return fail() ? (void*)0 : (void*)(-1); }
    int operator!() const { return fail(); }
    iostate exceptions() const { return _exceptions; }
    void exceptions(iostate enable) {
	_exceptions = enable;
	if (_state & _exceptions) _throw_failure(); }

    streambuf* rdbuf() const { return _strbuf; }
    streambuf* rdbuf(streambuf *_s) {
      streambuf *_old = _strbuf; _strbuf = _s; clear (); return _old; }

    static int sync_with_stdio(int on);
    static void sync_with_stdio() { sync_with_stdio(1); }
    static fmtflags bitalloc();
    static int xalloc();
    void*& pword(int);
    void* pword(int) const;
    long& iword(int);
    long iword(int) const;

#ifdef _STREAM_COMPAT
    void unset(state_value flag) { _state &= ~flag; }
    void close();
    int is_open();
    int readable();
    int writable();
#endif

    // Used to initialize standard streams. Not needed in this implementation.
    class Init {
    public:
      Init () { }
    };

  protected:
    inline ios(streambuf* sb = 0, ostream* tie_to = 0);
    inline virtual ~ios();
    inline void init(streambuf* sb, ostream* tie = 0);
};

#if __GNUG__==1
typedef int _seek_dir;
#else
typedef ios::seek_dir _seek_dir;
#endif

// Magic numbers and bits for the _flags field.
// The magic numbers use the high-order bits of _flags;
// the remaining bits are abailable for variable flags.
// Note: The magic numbers must all be negative if stdio
// emulation is desired.

// A streammarker remembers a position in a buffer.
// You are guaranteed to be able to seek back to it if it is saving().
class streammarker : private _IO_marker {
    friend class streambuf;
    void set_offset(int offset) { _pos = offset; }
  public:
    streammarker(streambuf *sb);
    ~streammarker();
    int saving() { return  1; }
    int delta(streammarker&);
    int delta();
};

struct streambuf : public _IO_FILE { // protected??
    friend class ios;
    friend class istream;
    friend class ostream;
    friend class streammarker;
    const void *&_vtable() { return *(const void**)((_IO_FILE*)this + 1); }
  protected:
    static streambuf* _list_all; /* List of open streambufs. */
    _IO_FILE*& xchain() { return _chain; }
    void _un_link();
    void _link_in();
    char* gptr() const
      { return _IO_file_flags & _IO_IN_BACKUP ? _IO_save_base : _IO_read_ptr; }
    char* pptr() const { return _IO_write_ptr; }
    char* egptr() const
      { return _IO_file_flags & _IO_IN_BACKUP ? _IO_save_end : _IO_read_end; }
    char* epptr() const { return _IO_write_end; }
    char* pbase() const { return _IO_write_base; }
    char* eback() const
      { return _IO_file_flags & _IO_IN_BACKUP ? _IO_save_base : _IO_read_base;}
    char* base() const { return _IO_buf_base; }
    char* ebuf() const { return _IO_buf_end; }
    int blen() const { return _IO_buf_end - _IO_buf_base; }
    void xput_char(char c) { *_IO_write_ptr++ = c; }
    int xflags() { return _IO_file_flags; }
    int xflags(int f) {int fl = _IO_file_flags; _IO_file_flags = f; return fl;}
    void xsetflags(int f) { _IO_file_flags |= f; }
    void xsetflags(int f, int mask)
      { _IO_file_flags = (_IO_file_flags & ~mask) | (f & mask); }
    void gbump(int n)
      { _IO_file_flags & _IO_IN_BACKUP ? (_IO_save_base+=n):(_IO_read_ptr+=n);}
    void pbump(int n) { _IO_write_ptr += n; }
    void setb(char* b, char* eb, int a=0);
    void setp(char* p, char* ep)
      { _IO_write_base=_IO_write_ptr=p; _IO_write_end=ep; }
    void setg(char* eb, char* g, char *eg) {
      if (_IO_file_flags & _IO_IN_BACKUP) _IO_free_backup_area(this); 
      _IO_read_base = eb; _IO_read_ptr = g; _IO_read_end = eg; }
    char *shortbuf() { return _shortbuf; }

    int in_backup() { return _flags & _IO_IN_BACKUP; }
    // The start of the main get area:  FIXME:  wrong for write-mode filebuf?
    char *Gbase() { return in_backup() ? _IO_save_base : _IO_read_base; }
    // The end of the main get area:
    char *eGptr() { return in_backup() ? _IO_save_end : _IO_read_end; }
    // The start of the backup area:
    char *Bbase() { return in_backup() ? _IO_read_base : _IO_save_base; }
    char *Bptr() { return _IO_backup_base; }
    // The end of the backup area:
    char *eBptr() { return in_backup() ? _IO_read_end : _IO_save_end; }
    char *Nbase() { return _IO_save_base; }
    char *eNptr() { return _IO_save_end; }
    int have_backup() { return _IO_save_base != NULL; }
    int have_markers() { return _markers != NULL; }
    void free_backup_area();
    void unsave_markers(); // Make all streammarkers !saving().
    int put_mode() { return _flags & _IO_CURRENTLY_PUTTING; }
    int switch_to_get_mode();
    
    streambuf(int flags=0);
  public:
    static int flush_all();
    static void flush_all_linebuffered(); // Flush all line buffered files.
    virtual ~streambuf();
    virtual int overflow(int c = EOF); // Leave public for now
    virtual int underflow(); // Leave public for now
    virtual int uflow(); // Leave public for now
    virtual int pbackfail(int c);
//    virtual int showmany ();
    virtual streamsize xsputn(const char* s, streamsize n);
    virtual streamsize xsgetn(char* s, streamsize n);
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);

    streampos pubseekoff(streamoff o, _seek_dir d, int mode=ios::in|ios::out)
      { return _IO_seekoff (this, o, d, mode); }
    streampos pubseekpos(streampos pos, int mode = ios::in|ios::out)
      { return _IO_seekpos (this, pos, mode); }
    streampos sseekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    streampos sseekpos(streampos pos, int mode = ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    virtual int sync();
    virtual int doallocate();

    int seekmark(streammarker& mark, int delta = 0);
    int sputbackc(char c);
    int sungetc();
    int unbuffered() { return _flags & _IO_UNBUFFERED ? 1 : 0; }
    int linebuffered() { return _flags & _IO_LINE_BUF ? 1 : 0; }
    void unbuffered(int i)
	{ if (i) _flags |= _IO_UNBUFFERED; else _flags &= ~_IO_UNBUFFERED; }
    void linebuffered(int i)
	{ if (i) _flags |= _IO_LINE_BUF; else _flags &= ~_IO_LINE_BUF; }
    int allocate() { // For AT&T compatibility
	if (base() || unbuffered()) return 0;
	else return doallocate(); }
    // Allocate a buffer if needed; use _shortbuf if appropriate.
    void allocbuf() { if (base() == NULL) doallocbuf(); }
    void doallocbuf();
    int in_avail() { return _IO_read_end - _IO_read_ptr; }
    int out_waiting() { return _IO_write_ptr - _IO_write_base; }
    streamsize sputn(const char* s, streamsize n) { return xsputn(s, n); }
    streamsize padn(char pad, streamsize n) { return _IO_padn(this, pad, n); }
    streamsize sgetn(char* s, streamsize n) { return _IO_sgetn(this, s, n); }
    int ignore(int);
    int get_column();
    int set_column(int);
    long sgetline(char* buf, _IO_size_t n, char delim, int putback_delim);
    int sputc(int c) { return _IO_putc(c, this); }
    int sbumpc() { return _IO_getc(this); }
    int sgetc() { return _IO_peekc(this); }
    int snextc() {
	if (_IO_read_ptr >= _IO_read_end && __underflow(this) == EOF)
	  return EOF;
	else return _IO_read_ptr++, sgetc(); }
    void stossc() { if (_IO_read_ptr < _IO_read_end) _IO_read_ptr++; }
    int vscan(char const *fmt0, _IO_va_list ap, ios* stream = NULL);
    int scan(char const *fmt0 ...);
    int vform(char const *fmt0, _IO_va_list ap);
    int form(char const *fmt0 ...);
#if 0 /* Work in progress */
    int column();  // Current column number (of put pointer). -1 is unknown.
    void column(int c);  // Set column number of put pointer to c.
#endif
    virtual streamsize sys_read(char* buf, streamsize size);
    virtual streamsize sys_write(const char*, streamsize);
    virtual streampos sys_seek(streamoff, _seek_dir);
    virtual int sys_close();
    virtual int sys_stat(void*); // Actually, a (struct stat*)
#if _G_IO_IO_FILE_VERSION == 0x20001
    virtual int showmanyc();
    virtual void imbue(void *);
#endif
};

// A backupbuf is a streambuf with full backup and savepoints on reading.
// All standard streambufs in the GNU iostream library are backupbufs.

class filebuf : public streambuf {
  protected:
    void init();
  public:
    static const int openprot; // Non-ANSI AT&T-ism:  Default open protection.
    filebuf();
    filebuf(int fd);
    filebuf(int fd, char* p, int len);
#if !_IO_UNIFIED_JUMPTABLES
    static filebuf *__new();
#endif
    ~filebuf();
    filebuf* attach(int fd);
    filebuf* open(const char *filename, const char *mode);
    filebuf* open(const char *filename, ios::openmode mode, int prot = 0664);
    virtual int underflow();
    virtual int overflow(int c = EOF);
    int is_open() const { return _fileno >= 0; }
    int fd() const { return is_open() ? _fileno : EOF; }
    filebuf* close();
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    streamsize xsputn(const char* s, streamsize n);
    streamsize xsgetn(char* s, streamsize n);
    virtual int sync();
  protected: // See documentation in filebuf.C.
//    virtual int pbackfail(int c);
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
    /* System's idea of pointer */
    char* file_ptr() { return eGptr(); }
    // Low-level operations (Usually invoke system calls.)
    virtual streamsize sys_read(char* buf, streamsize size);
    virtual streampos sys_seek(streamoff, _seek_dir);
    virtual streamsize sys_write(const char*, streamsize);
    virtual int sys_stat(void*); // Actually, a (struct stat*)
    virtual int sys_close();
#if 0
    virtual uflow;
    virtual showmany;
#endif
};

inline void ios::init(streambuf* sb, ostream* tie_to) {
		_state = sb ? ios::goodbit : ios::badbit; _exceptions=0;
		_strbuf=sb; _tie = tie_to; _width=0; _fill=' ';
#ifdef _IO_NEW_STREAMS
		_flags=ios::skipws|ios::dec;
#else
		_flags=ios::skipws|ios::dec|ios::dont_close;
#endif
		_precision=6; _arrays = 0; }

inline ios::ios(streambuf* sb, ostream* tie_to) { init(sb, tie_to); }

inline ios::~ios() {
#ifndef _IO_NEW_STREAMS
    if (!(_flags & (unsigned int)ios::dont_close)) delete rdbuf();
#endif
    if (_arrays) delete [] _arrays;
}
} // extern "C++"
#endif /* _STREAMBUF_H */
