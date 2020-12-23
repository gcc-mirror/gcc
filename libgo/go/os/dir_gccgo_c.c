/* Copyright 2020 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <dirent.h>

#include "runtime.h"

unsigned char direntType (struct dirent *)
  __asm__ (GOSYM_PREFIX "os.direntType");

unsigned char
direntType (struct dirent *p __attribute__((unused)))
{
#ifndef HAVE_STRUCT_DIRENT_D_TYPE
  return 'U';
#else
  switch (p->d_type)
    {
#ifdef DT_BLK
    case DT_BLK:
      return 'B';
#endif
#ifdef DT_CHR
    case DT_CHR:
      return 'C';
#endif
#ifdef DT_DBF
    case DT_DBF:
      // Database record file.
      // Treat as regular file.
      return 'R';
#endif
#ifdef DT_DIR
    case DT_DIR:
      return 'D';
#endif
#ifdef DT_FIFO
    case DT_FIFO:
      return 'F';
#endif
#ifdef DT_LNK
    case DT_LNK:
      return 'L';
#endif
#ifdef DT_REG
    case DT_REG:
      return 'R';
#endif
#ifdef DT_SOCK
    case DT_SOCK:
      return 'S';
#endif
    default:
      return 'U';
    }
#endif /* HAVE_DIRENT_D_TYPE */
}
