#include <gcj/cni.h>

#include "longfield.h"
#include <java/lang/System.h>
#include <java/io/PrintStream.h>

void
longfield::doitc ()
{
  java::io::PrintStream *ps = java::lang::System::out;

  ps->println(lval);
  ps->println(bval);
  ps->println(sval);
}
