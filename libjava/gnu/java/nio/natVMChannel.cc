#include <config.h>

#include <gnu/java/nio/VMChannel.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

void
gnu::java::nio::VMChannel::setBlocking (jint, jboolean)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::setBlocking (jint, jboolean) not implemented"));
}


jint
gnu::java::nio::VMChannel::read (jint, ::java::nio::ByteBuffer *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::read (jint, ::java::nio::ByteBuffer *) not implemented"));
}


jlong
gnu::java::nio::VMChannel::readScattering (jint, JArray< ::java::nio::ByteBuffer *> *, jint, jint)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::readScattering (jint, JArray< ::java::nio::ByteBuffer *> *, jint, jint) not implemented"));
}


jint
gnu::java::nio::VMChannel::write (jint, ::java::nio::ByteBuffer *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::write (jint, ::java::nio::ByteBuffer *) not implemented"));
}


jlong
gnu::java::nio::VMChannel::writeGathering (jint, JArray< ::java::nio::ByteBuffer *> *, jint, jint)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::writeGathering (jint, JArray< ::java::nio::ByteBuffer *> *, jint, jint) not implemented"));
}


void
gnu::java::nio::VMChannel::initIDs ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::nio::VMChannel::initIDs () not implemented"));
}
