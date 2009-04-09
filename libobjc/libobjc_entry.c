/* GNU Objective C Runtime DLL Entry
   Copyright (C) 1997, 2009 Free Software Foundation, Inc.
   Contributed by Scott Christley <scottc@net-community.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include <windows.h>

/*
  DLL entry function for Objective-C Runtime library
  This function gets called everytime a process/thread attaches to DLL
  */
WINBOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call,
        LPVOID lpReserved)
{
  switch(ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }
  return TRUE;
}

/*
  This section terminates the list of imports under GCC. If you do not
  include this then you will have problems when linking with DLLs.
  */
asm (".section .idata$3\n" ".long 0,0,0,0,0,0,0,0");
