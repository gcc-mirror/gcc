#include <vector.h>
#include <iostream.h>
#include <algo.h>

main ()
{
  cout << "Fill of C array:\n";
  char x[50];
  fill (x, x+50, '/');
  fill (x+1, x+49, '*');
  copy (x, x+50, ostream_iterator<char>(cout));

  cout << "\nFill of vector<char>:\n";

  vector<char> cvec;
  cvec.insert (cvec.begin(), 50, '/');
  fill (cvec.begin()+1, cvec.end()-1, '-');
  copy (cvec.begin(), cvec.end(), ostream_iterator<char>(cout));
  cout << endl;
}
