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
 * a few tests for streams
 *
 */

#include <stream.h>
#include <fstream.h>
#ifndef _OLD_STREAMS
#include <strstream.h>
#include "unistd.h"
#endif
#include <SFile.h>
#include <PlotFile.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

class record
{
public:
  char c; int i; double d;
};

ostream& operator<<(ostream& s, record& r)
{
  return(s << "(i = " << r.i << " c = " << r.c << " d = " << r.d << ")");
}

void t1()
{
  char ch;

  assert(cout.good());
  assert(cout.writable());
  assert(cout.is_open());
  cout << "Hello, world via cout\n";
  assert(cerr.good());
  assert(cerr.writable());
  assert(cerr.is_open());
  cerr << "Hello, world via cerr\n";

  assert(cin.good());
  assert(cin.readable());
  assert(cin.is_open());

  cout << "enter a char:";  cin >> ch;
  cout.put('c');  cout.put(' ');  cout.put('=');  cout.put(' ');
  cout.put('"');  cout.put(ch);    cout << '"';  cout << char('\n');
  assert(cin.good());
  assert(cout.good());
}

void t2()
{
  int i;
  short h;
  long l;
  float f;
  double d;
  char s[100];

  cout << "enter three integers (short, int, long):";  
  cin >> h; cin >> i;   
  // cin.scan("%ld", &l);
  cin >> l;
  cout << "first  = " << h << " via dec = " << dec(h, 8) << "\n";
  cout << "second = " << i << form(" via form = %d = 0%o", i, i);
  cout.form(" via cout.form = %d = 0x%x\n", i, i);
  cout << "third  = " << l  << " via hex = " << hex(l) << "\n";
  assert(cin.good());
  assert(cout.good());

  cout << "enter a float then a double:";  cin >> f; cin >> d;
  cout << "first  = " << f << "\n";
  cout << "second = " << d << "\n";
  assert(cin.good());
  assert(cout.good());

  cout << "enter 5 characters separated with spaces:";  cin >> s;
  cout << "first  = " << s << "\n";
  cin.get(s, 100);
  cout << "rest   = " << s << "\n";

  assert(cin.good());

  cin.width(10);
  cin >> s;
  cin.clear();
  cout << "A 10-character buffer: " << s << endl;

  assert(cout.good());

}

void t3()
{
  char ch;
  cout << "\nMaking streams sout and sin...";
#ifdef _OLD_STREAMS
  ostream sout("streamfile", io_writeonly, a_create);
#else
  ofstream sout("streamfile");
#endif
  assert(sout.good());
  assert(sout.is_open());
  assert(sout.writable());
  assert(!sout.readable());
  sout << "This file has one line testing output streams.\n";
  sout.close();
  assert(!sout.is_open());
#ifdef _OLD_STREAMS
  istream sin("streamfile", io_readonly, a_useonly);
#else
  ifstream sin("streamfile");
#endif
  assert(sin.good());
  assert(sin.is_open());
  assert(!sin.writable());
  assert(sin.readable());
  cout << "contents of file:\n";
  while(sin >> ch) cout << ch;
  sin.close();
  assert(!sin.is_open());
}


void t4()
{
  char s[100];
  char ch;
  int i;

  cout << "\nMaking File tf ... "; 
#ifdef _OLD_STREAMS
  File tf("tempfile", io_readwrite, a_create);
#else
  fstream tf("tempfile", ios::in|ios::out|ios::trunc);
#endif
  assert(tf.good());
  assert(tf.is_open());
  assert(tf.writable());
  assert(tf.readable());
  strcpy(s, "This is the first and only line of this file.\n");
#ifdef _OLD_STREAMS
  tf.put(s);
  tf.seek(0);
#else
  tf << s;
  tf.rdbuf()->seekoff(0, ios::beg);
#endif
  tf.get(s, 100);
  assert(tf.good());
  cout << "first line of file:\n" << s << "\n";
  cout << "next char = ";
  tf.get(ch);
  cout << (int)ch;
  cout.put('\n');
  assert(ch == 10);
  strcpy(s, "Now there is a second line.\n");
  cout << "reopening tempfile, appending: " << s;
#ifdef _OLD_STREAMS
  tf.open(tf.name(), io_appendonly, a_use);
#else
  tf.close();
  tf.open("tempfile", ios::app);
#endif
  assert(tf.good());
  assert(tf.is_open());
  assert(tf.writable());
  assert(!tf.readable());
#ifdef _OLD_STREAMS
  tf.put(s);
  assert(tf.good());
  tf.open(tf.name(), io_readonly, a_use);
#else
  tf << s;
  assert(tf.good());
  tf.close();
  tf.open("tempfile", ios::in);
#endif
  tf.raw();
  assert(tf.good());
  assert(tf.is_open());
  assert(!tf.writable());
  assert(tf.readable());
  cout << "First 10 chars via raw system read after reopen for input:\n";
  read(tf.filedesc(), s, 10);
  assert(tf.good());
  for (i = 0; i < 10; ++ i)
    cout.put(s[i]);
  lseek(tf.filedesc(), 5, 0);
  cout << "\nContents after raw lseek to pos 5:\n";
  while ( (tf.get(ch)) && (cout.put(ch)) );
#ifdef _OLD_STREAMS
  tf.remove();
#else
  tf.close();
  unlink("tempfile");
#endif
  assert(!tf.is_open());
}

void t5()
{
  record r;
  int i;
  cout << "\nMaking SFile rf...";
#ifdef _OLD_STREAMS
  SFile rf("recfile", sizeof(record), io_readwrite, a_create);
#else
  SFile rf("recfile", sizeof(record), ios::in|ios::out|ios::trunc);
#endif
  assert(rf.good());
  assert(rf.is_open());
  assert(rf.writable());
  assert(rf.readable());
  for (i = 0; i < 10; ++i)
  {
    r.c = i + 'a';
    r.i = i;
    r.d = (double)(i) / 1000.0;
    rf.put(&r);
  }
  assert(rf.good());
  cout << "odd elements of file in reverse order:\n";
  for (i = 9; i >= 0; i -= 2)
  {
    rf[i].get(&r);
    assert(r.c == i + 'a');
    assert(r.i == i);
    cout << r << "\n";
  }
  assert(rf.good());
#ifdef _OLD_STREAMS
  rf.remove();
#else
  rf.close();
  unlink("recfile");
#endif
  assert(!rf.is_open());
}

void t6()
{
  cout << "\nMaking PlotFile pf ...";
  static const char plot_name[] = "plot.out";
  PlotFile pf(plot_name);
  assert(pf.good());
  assert(pf.is_open());
  assert(pf.writable());
  assert(!pf.readable());
  pf.move(10,10);
  pf.label("Test");
  pf.circle(300,300,200);
  pf.line(100, 100, 500, 500);
  assert(pf.good());
#ifdef _OLD_STREAMS
  cout << "(You may delete or attempt to plot " << pf.name() << ")\n";
#else
  cout << "(You may delete or attempt to plot " << plot_name << ")\n";
#endif
}

void t7()
{
  char ch;
  static char t7_line1[] = "This is a string-based stream.\n";
  static char t7_line2[] = "With two lines.\n";
  char mybuf[60];
  char *bufp;
#ifdef _OLD_STREAMS
  cout << "creating string-based ostream...\n";
  ostream strout(60, mybuf);
#else
  cout << "creating ostrstream...\n";
  ostrstream strout(mybuf, 60);
#endif
  assert(strout.good());
  assert(strout.writable());
  strout << t7_line1 << t7_line2 << ends;
  assert(strout.good());
  cout << "with contents:\n";
  bufp = strout.str();
  assert(bufp == mybuf);
  strout.rdbuf()->freeze(0); /* Should be a no-op */
  cout << mybuf;
#ifdef _OLD_STREAMS
  cout << "using it to create string-based istream...\n";
  istream strin(strlen(mybuf), mybuf);
#else
  cout << "using it to create istrstream...\n";
  istrstream strin(mybuf, strlen(mybuf));
#endif
  assert(strin.good());
  assert(strin.readable());
  cout << "with contents:\n";
#ifndef _OLD_STREAMS
  char line[100];
  strin.getline(line, 100);
  int line1_len = strlen(t7_line1);
  assert(strin.tellg() == line1_len);
  int line_len = strin.gcount();
  assert(line_len == line1_len);
  cout.write(line, line1_len - 1);
  cout << endl;
#endif
  while (strin.get(ch)) cout.put(ch);

  strstream str1;
  strstream str2;
  str1 << "Testing string-based stream using strstream.\n";
  str1.seekg(0);
  for (;;) {
      int i = str1.get();
      if (i == EOF)
	  break;
      str2 << (char)i;
  }
  str2 << ends;
  cout << str2.str();

  // This should make it overflow.
  strout << t7_line1;
  assert (strout.bad());
}

void t8()
{
#ifdef _OLD_STREAMS
  cout << "\nThe following file open should generate error message:";
  cout.flush();
  File ef("shouldnotexist", io_readonly, a_useonly);
#else
  ifstream ef("shouldnotexist");
#endif
  assert(!ef.good());
  assert(!ef.is_open());
}

void t9()
{
  char ch;
  static char ffile_name[] = "ftmp";
  {
      cout << "\nMaking filebuf streams fout and fin...";
      filebuf foutbuf;
#ifdef _OLD_STREAMS
      foutbuf.open(ffile_name, output);
#else
      foutbuf.open(ffile_name, ios::out);
#endif
      ostream fout(&foutbuf);
      assert(fout.good());
      assert(fout.is_open());
      assert(fout.writable());
      assert(!fout.readable());
      fout << "This file has one line testing output streams.\n";
#ifdef _OLD_STREAMS
      fout.close();
      assert(!fout.is_open());
#endif
  }
  filebuf finbuf;
#ifdef _OLD_STREAMS
  finbuf.open(ffile_name, input);
#else
  finbuf.open(ffile_name, ios::in);
#endif
  istream fin(&finbuf);
  assert(fin.good());
  assert(fin.is_open());
  assert(!fin.writable());
  assert(fin.readable());
  cout << "contents of file:\n";
  while(fin >> ch) cout << ch;
#ifndef _OLD_STREAMS
  cout << '\n';
#endif
  fin.close();
  assert(!fin.is_open());
}

void t10()
{
    int fileCnt = 3;
    char *file_name_pattern = "ftmp%d";
    char current_file_name[50];
    ifstream inFile;
    ofstream outFile;
    char c;
    int i;
    
    cout << '\n';

    // Write some files.
    for (i=0; i < fileCnt; i++)   {
	sprintf(current_file_name, file_name_pattern, i);
	outFile.open(current_file_name, ios::out);
		
	if ( outFile.fail() )
	    cerr << "File " << current_file_name
		<< " can't be opened for output" << endl;
	else {
	    outFile << "This is line 1 of " << current_file_name << '\n';
	    outFile << "This is line 2 of " << current_file_name << endl;
	    outFile.close();
	}
    }

    // Now read the files back in, and write then out to cout.
    for (i=0; i < fileCnt; i++)   {
	sprintf(current_file_name, file_name_pattern, i);
	inFile.open(current_file_name, ios::in);
		

	if ( inFile.fail() )
	    cerr << "File " << current_file_name 
		<< " can't be opened for input" << endl;
	else {
	    while ( inFile.get (c))
		cout << c;
	    cout << endl;
	    inFile.close();
	}
    }
}

// Test form

void t11()
{
    int count1, count2;
    cout.form("%.2f+%.2f = %4.3e\n%n", 5.5, 6.25, 5.5+6.25, &count1);
    char *text = "Previous line has12345";
    char text_length_to_use = strlen(text) - 5;
    count2 = cout.rdbuf()->form("%-*.*s%3g characters\n",
				text_length_to_use + 1, 
				text_length_to_use,
				text,
				(double)(count1-1));
    cout.form("%-*.*s%+d characters\n%n",
	      text_length_to_use + 1, text_length_to_use, text,
	      count2-1, &count1);
    assert(count1 == 33);
}

static void
show_int (long val)
{
  cout.setf(ios::showbase);
  cout << dec; cout.width (8); cout << val << "(dec) = ";
  cout << hex; cout.width (8); cout << (0xFFFF & val) << "(hex) = ";
  cout << oct; cout.width (8);
  cout << (0xFFFF & val) << "(oct) [showbase on]\n";
  cout.unsetf(ios::showbase);
  cout << dec; cout.width (8); cout << val << "(dec) = ";
  cout << hex; cout.width (8); cout << (0xFFFF & val) << "(hex) = ";
  cout << oct; cout.width (8);
  cout << (0xFFFF & val) << "(oct) [showbase off]\n";
}

void
t12 ()
{
  ios::fmtflags old_flags = cout.setf(ios::showpos);
  int fill = cout.fill('_');
  cout.unsetf(ios::uppercase);
  cout.setf(ios::internal, ios::adjustfield);
  show_int(34567);
  show_int(-34567);
  cout.setf(ios::right, ios::adjustfield);
  show_int(0);
  cout.setf(ios::uppercase);
  cout.unsetf(ios::showpos);
  show_int(34567);
  cout.setf(ios::left, ios::adjustfield);
  show_int(-34567);
  cout.fill(fill);
  show_int(0);
  cout.setf(old_flags,
	    ios::adjustfield|ios::basefield
	    |ios::showbase|ios::showpos|ios::uppercase);
}

main(int argc, char **argv)
{
 if (argc > 1 && strncmp(argv[1], "-b", 2) == 0) {
     streambuf *sb = cout.rdbuf();
     streambuf *ret;
     int buffer_size = atoi(&argv[1][2]);
     if (buffer_size == 0)
	 ret = sb->setbuf(NULL, 0);
     else
	 ret = sb->setbuf(new char[buffer_size], buffer_size);
     if (ret != sb)
	 cerr << "Warning: cout.rdbuf()->setbuf failed!\n";
  }
  t1();
  t2();
  t3();
  t4();
  t5();
  t6();
  t7();
  t9();
  t8();
  t10();
  t11();
  t12();

  cout << "Final names & states:\n";
#ifdef _OLD_STREAMS
  cout << "cin:      " << cin.name()  << "\t" << cin.rdstate() << "\n";
  cout << "cout:     " << cout.name() << "\t" << cout.rdstate() << "\n";
  cout << "cerr:     " << cerr.name() << "\t" << cerr.rdstate() << "\n";
#else
  cout << "cin:      " << "(stdin)"  << "\t" << cin.rdstate() << "\n";
  cout << "cout:     " << "(stdout)" << "\t" << cout.rdstate() << "\n";
  cout << "cerr:     " << "(stderr)" << "\t" << cerr.rdstate() << "\n";
#endif
  cout << "\nend of test.\n";
}
