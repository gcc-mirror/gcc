#include <config.h>

#include <gnu/java/lang/management/VMOperatingSystemMXBeanImpl.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jdouble
gnu::java::lang::management::VMOperatingSystemMXBeanImpl::getSystemLoadAverage ()
{
#ifdef HAVE_GETLOADAVG
  double avg[1];
  int nos = getloadavg(avg, 1);
  if (nos == 1)
    return avg[0];
  else
#endif
    return -1;
}
