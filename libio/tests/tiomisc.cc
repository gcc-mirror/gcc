/* Random regression tests etc. */

#include <fstream.h>
#include <stdio.h>
#include <strstream.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <assert.h>

#define BUF_SIZE 4096

void
test1 ()
{
   fstream f;
   char    buf[BUF_SIZE];

   f.setbuf( buf, BUF_SIZE );
}

void
test2 ( )
{
   char string[BUF_SIZE];
   ostrstream s( string, BUF_SIZE );

   s << "Bla bla bla " << 55 << ' ' << 3.23 << '\0' << endl;
   cout << "Test2: " << string << endl;
}


/* Test case from Joe Buck <jbuck@Synopsys.COM>. */

class special_ofstream : public ofstream {
public:
	special_ofstream() : ofstream() {}
	special_ofstream(int fd) : ofstream(fd) {}
	special_ofstream(const char *name, int mode=ios::out, int prot=0664) {
		open(name,mode,prot);
	}
	void open(const char *name, int mode=ios::out, int prot=0664);
};

void special_ofstream::open(const char* name, int mode, int prot) {
	if (strcmp(name, "<cout>") == 0) {
		rdbuf()->attach(1);
	}
	else if (strcmp(name, "<cerr>") == 0) {
		rdbuf()->attach(2);
		setf(unitbuf);
	}
	else ofstream::open(name,mode,prot);
}

void
test3 ()
{
	{
		special_ofstream o("<cout>");
		o << "Hello\n";
		// o is destructed now.  This should not close cout
	}
	{
		special_ofstream o("<cout>");
		o << "Line 2\n";
	}
}

void
getline_test1 ()
{
  char buf[1000];
  char data[] = "#include <iostream.h>\n#include <fstream.h>\n";
  istrstream infile(data, strlen(data));
  infile.getline(buf,1000);
  infile.getline(buf,1000);

  cout << buf << '\n';
}

// test istream::getline on readng overlong lines.
void
getline_test2 ()
{
  char data[] = "Line one.\nline 2.\n";
  char line[100];
  istrstream strin(data, strlen(data));
  strin.getline(line, 10);
  cout << "line: " << line << ", count: " << strin.gcount () << "\n";
}

void
getline_test3 ()
{
  char data[] = "123456789\nabcdefghijkl.\n";
  char line[10];
  istrstream strin(data, strlen(data));
  strin.getline(line, 10);
  cout << "line: " << line << ", count: " << strin.gcount () << "\n";
  strin.getline(line, 10);
  cout << "line: " << line << ", count: " << strin.gcount () << "\n";
  assert (!strin.good());
  strin.clear ();
  strin.getline(line, 10);
  cout << "line: " << line << ", count: " << strin.gcount () << "\n";
}

class A : private ostream
{
public:
  A(streambuf* s);
  ostream::flush;
};
A::A(streambuf* s)
: ostream(s)
{
}

void
flush1_test()
{
  A os(cout.rdbuf());
  os.flush();
}

void
reread_test ()
{  // This is PR 5486.
   int tag_char;
   char *fname = "Makefile";
   int mode = O_RDONLY;
   filebuf file_p; 

   int fd = ::open(fname, mode, 0666);
   file_p.attach(fd); 

   istream d_istream(&file_p);

   // Read a character from the stream, save it and put it back.
   tag_char = d_istream.get();
   int save_char = tag_char;
   d_istream.putback((char) tag_char);

   // Uncomment then next statement and the next get will be EOF.
   streampos pos = d_istream.tellg();  

   // Re-read the first character
   tag_char = d_istream.get();

   cout << "reread_test: " << (char)save_char << " " << (char)tag_char << "\n";
   cout.flush();

}

void *danger_pointer;
void operator delete (void *p) throw()
{
  if (p)
    {
      if (p == danger_pointer)
	fprintf (stderr, "maybe deleted\n");

      free (p);
    }
}

struct my_ostream: virtual public ios, public ostream
{
  my_ostream (ostream &s): ios (s.rdbuf()) { }
};

void
test_destroy ()
{
  ofstream fstr ("foo.dat");
  my_ostream wa (fstr);

  /* Check that sure wa.rdbuf() is only freed once. */
  danger_pointer = wa.rdbuf ();

  wa << "Hi there" << endl;
#ifdef _IO_NEW_STREAMS
  fprintf (stderr, "maybe deleted\n");
#endif
}

/* Submitted by Luke Blanshard <luke@cs.wisc.edu>.

   In certain circumstances, the library will write past the end of the
   buffer it has allocated for a file:  You must read from the file,
   exactly enough bytes that the read pointer is at the end of the
   buffer.  Then you must write to the file, at the same place you just
   finished reading from.

   "Your patch looks great, and you're welcome to use the test code for any  
   purpose whatever.  I hereby renounce my implicit copyright on it."  */

void
test_read_write_flush ()
{
    fstream f;
    char buf[8192];

    for ( int index=0; index < sizeof buf; ++index )
        buf[index] = (index+1)&63? 'x' : '\n';

    f.open( "foo.dat", ios::in|ios::out|ios::trunc );
    f.write( buf, sizeof buf );

    f.seekg( 0, ios::beg );
    f.read( buf, sizeof buf );

//    f.seekp( sizeof buf, ios::beg );	// Present or absent, bug still happens.
    f.write( "a", 1 );

    if ( f.rdbuf()->_IO_write_ptr > f.rdbuf()->_IO_buf_end )
        cerr << "test_read_write_flush: it's broken.\n";
    else
        cout << "test_read_write_flush: the problem isn't showing itself.\n";
}

int main( )
{
  test1 ();
  test2 ();
  test3 ();
  getline_test1 ();
  getline_test2 ();
  getline_test3 ();
  flush1_test ();
  reread_test ();
  test_destroy ();
  test_read_write_flush ();
  return 0;
}
