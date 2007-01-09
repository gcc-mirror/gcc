#include <config.h>

#include <java/util/concurrent/atomic/AtomicLong.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jboolean
java::util::concurrent::atomic::AtomicLong::VMSupportsCS8 ()
{
  // FIXME
  return false;
}
