// natEcosProcess.cc - Native side of eCos processes.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// The configury system needs this file to exist, since we can't
// really conditionally link files (an autoconf bug).  To avoid having
// an empty translation unit, we make a single method native.  FIXME.

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/ConcreteProcess.h>

void
java::lang::ConcreteProcess::destroy (void)
{
}
