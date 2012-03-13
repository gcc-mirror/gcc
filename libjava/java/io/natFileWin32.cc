// natFileWin32.cc - Native part of File class for Win32.

/* Copyright (C) 1998, 1999, 2002, 2003, 2012 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <stdio.h>
#include <string.h>

#undef STRICT

#include <java/io/File.h>
#include <java/io/IOException.h>
#include <java/util/Vector.h>
#include <java/lang/String.h>
#include <java/io/FilenameFilter.h>
#include <java/io/FileFilter.h>
#include <java/lang/System.h>

// Java timestamps are milliseconds since the UNIX epoch (00:00:00 UTC on 
// January 1, 1970) while Win32 file-times are 100-nanosecond intervals
// since the Win32 epoch (00:00:00 UTC on January 1, 1601). The following
// constant represents the number of milliseconds to be added to a
// Java timestamp to base it on the Win32 epoch.
// 
// There were 369 years between 1601 and 1970, including 89 leap years
// (since 1700, 1800 and 1900 were not leap years):
//
// (89*366 + 280*365) days * 86400 seconds/day = 11644473600 seconds
//
#define WIN32_EPOCH_MILLIS 11644473600000LL

jboolean
java::io::File::access (jint query)
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  JvAssert (query == READ || query == WRITE || query == EXISTS
	    || query == EXEC);

  // FIXME: Is it possible to differentiate between existing and reading?
  // If the file exists but cannot be read because of the secuirty attributes
  // on an NTFS disk this wont work (it reports it can be read but cant)
  // Could we use something from the security API?
  DWORD attributes = GetFileAttributes (canon);
  // FIXME: handle EXEC
  if (query == EXEC)
    return false;
  if ((query == EXISTS) || (query == READ))
    return (attributes == 0xffffffff) ? false : true;
  else
    return ((attributes != 0xffffffff) &&
      ((attributes & FILE_ATTRIBUTE_READONLY) == 0)) ? true : false;
}

jboolean
java::io::File::stat (jint query)
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  JvAssert (query == DIRECTORY || query == ISFILE);

  DWORD attributes = GetFileAttributes (canon);
  if (attributes == 0xffffffff)
    return false;

  if (query == DIRECTORY)
    return attributes & FILE_ATTRIBUTE_DIRECTORY ? true : false;
  else
    return attributes & FILE_ATTRIBUTE_DIRECTORY ? false : true;
}

jlong
java::io::File::attr (jint query)
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  JvAssert (query == MODIFIED || query == LENGTH);

  WIN32_FIND_DATA info;
  HANDLE sHandle;
  if ( ( sHandle = FindFirstFile( canon, &info)) == INVALID_HANDLE_VALUE)
    return 0;
  
  FindClose( sHandle);
  
  if (query == LENGTH)
    return ((long long)info.nFileSizeHigh) << 32 
           | (unsigned long long)info.nFileSizeLow;
  else 
    {
      // The file time as returned by Windows is in terms of the number
      // of 100-nanosecond intervals since 00:00:00 UTC, January 1, 1601.
      return (((((long long)info.ftLastWriteTime.dwHighDateTime) << 32) 
               | ((unsigned long long)info.ftLastWriteTime.dwLowDateTime)) 
              - WIN32_EPOCH_MILLIS*10000LL) / 10000LL;
    }
}

jstring
java::io::File::getCanonicalPath (void)
{
  JV_TEMP_STRING_WIN32 (cpath, path);
  
  // If the filename is blank, use the current directory.
  LPCTSTR thepath = cpath.buf();
  if (*thepath == 0)
    thepath = _T(".");

  LPTSTR unused;
  TCHAR buf2[MAX_PATH];
  if(!GetFullPathName(thepath, MAX_PATH, buf2, &unused))
    throw new IOException (JvNewStringLatin1 ("GetFullPathName failed"));

  return _Jv_Win32NewString (buf2);
}

jboolean
java::io::File::isAbsolute (void)
{
  // See if the path represents a Windows UNC network path.
  if (path->length () > 2
      && (path->charAt (0) == '\\') && (path->charAt (1) == '\\'))
    return true;

  // Note that the path is not an absolute path even if it starts with
  // a '/' or a '\' because it lacks a drive specifier.

  if (path->length() < 3)
    return false;
  // Hard-code A-Za-z because Windows (I think) can't use non-ASCII
  // letters as drive names.
  if ((path->charAt(0) < 'a' || path->charAt(0) > 'z')
      && (path->charAt(0) < 'A' || path->charAt(0) > 'Z'))
    return false;
  return (path->charAt(1) == ':'
    && (path->charAt(2) == '/' || path->charAt(2) == '\\'));
}

void java::io::File::init_native () 
{
  maxPathLen = MAX_PATH;
  caseSensitive = false;
}

jobjectArray
java::io::File::performList (java::io::FilenameFilter *filter, 
           java::io::FileFilter *fileFilter, 
           java::lang::Class *clazz)
{
  jstring canon = getCanonicalPath();
  if (! canon)
    return NULL;

  int len = canon->length();
  TCHAR buf[len + 5];
  
  JV_TEMP_STRING_WIN32(canonstr, canon);
  
  _tcscpy(buf, canonstr);
  if (buf[len - 1] == _T('\\'))
    _tcscpy (&buf[len], _T("*.*"));
  else
    _tcscpy (&buf[len], _T("\\*.*"));

  WIN32_FIND_DATA data;
  HANDLE handle = FindFirstFile (buf, &data);
  if (handle == INVALID_HANDLE_VALUE)
    return NULL;

  java::util::Vector *vec = new java::util::Vector ();

  do
    {
      if (_tcscmp (data.cFileName, _T(".")) &&
        _tcscmp (data.cFileName, _T("..")))
        {
          jstring name = _Jv_Win32NewString (data.cFileName);

          if (filter && !filter->accept(this, name))
            continue;
          if (clazz == &java::io::File::class$)
            {
              java::io::File *file = new java::io::File (this, name);
              if (fileFilter && !fileFilter->accept(file))
                continue;
              vec->addElement (file);
            }
          else
            vec->addElement (name);
        }
    }
  while (FindNextFile (handle, &data));

  if (GetLastError () != ERROR_NO_MORE_FILES)
    return NULL;

  FindClose (handle);

  jobjectArray ret = JvNewObjectArray (vec->size(), clazz, NULL);
  vec->copyInto (ret);
  return ret;
}

jboolean
java::io::File::setFilePermissions (jboolean enable,
				    jboolean ownerOnly,
				    jint permissions)
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  DWORD attrs = GetFileAttributes (canon);
  if (attrs != INVALID_FILE_ATTRIBUTES)
    {
      // FIXME: implement
      return false;
    }
  else
    return false;
}

jboolean
java::io::File::performMkdir (void)
{
  JV_TEMP_STRING_WIN32 (cpath, path);
  return (CreateDirectory(cpath, NULL)) ? true : false;
}

jboolean
java::io::File::performRenameTo (File *dest)
{
  JV_TEMP_STRING_WIN32 (pathFrom, path);
  JV_TEMP_STRING_WIN32 (pathTo, dest->path);
  return (MoveFile(pathFrom, pathTo)) ? true : false;
}

jboolean
java::io::File::performDelete ()
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  DWORD attributes = GetFileAttributes (canon);
  if (attributes == 0xffffffff)
    return false;

  if (attributes & FILE_ATTRIBUTE_DIRECTORY)
    return (RemoveDirectory (canon)) ? true : false;
  else
    return (DeleteFile (canon)) ? true : false;
}

jboolean java::io::File::performCreate (void) 
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  HANDLE h = CreateFile (canon, 0, 0, NULL, CREATE_NEW, 
                         FILE_ATTRIBUTE_NORMAL, NULL);
  if (h != INVALID_HANDLE_VALUE)
    {
      CloseHandle (h);
      return true;
    }
  else
    {
      if (GetLastError () == ERROR_ALREADY_EXISTS)
        return false;
      else
        throw new IOException (JvNewStringLatin1 ("CreateFile failed"));
    }
}

jboolean java::io::File::performSetReadOnly ()
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  DWORD attrs = GetFileAttributes (canon);
  if (attrs != INVALID_FILE_ATTRIBUTES)
    {
      if (SetFileAttributes (canon, attrs | FILE_ATTRIBUTE_READONLY) != 0)
        return true;
      else
        return false;
    }
  else
    return false;
}

jboolean java::io::File::performSetLastModified (jlong time)
{
  JV_TEMP_STRING_WIN32 (canon, getCanonicalPath());
  if (!canon)
    return false;

  FILETIME modTime;
  long long mTime100ns = ((long long) time        /* Ha! */
                          + WIN32_EPOCH_MILLIS) * 10000LL;
  
  modTime.dwLowDateTime = (DWORD) mTime100ns;
  modTime.dwHighDateTime = (DWORD) (mTime100ns >> 32);

  jboolean retVal = false;
  HANDLE h = CreateFile (canon, FILE_WRITE_ATTRIBUTES, 
                         FILE_SHARE_READ | FILE_SHARE_WRITE, 
                         NULL, OPEN_EXISTING, 0, NULL);

  if (h != INVALID_HANDLE_VALUE)
    {
      if (SetFileTime (h, NULL, &modTime, &modTime) != 0)
        retVal = true;

      CloseHandle (h);
    }

  return retVal;
}

JArray<java::io::File*>* java::io::File::performListRoots ()
{
  DWORD drivesBitmap = GetLogicalDrives ();
  DWORD mask;

  // Possible drive letters are from ASCII 'A'-'Z'.
  int numDrives = 0;
  mask = 1;
  for (int i = 0; i < 26; i++)
    {
      if ((drivesBitmap & mask) != 0)
        numDrives++;
      mask <<= 1;
    }

  JArray<java::io::File *> *roots
    = reinterpret_cast <JArray<java::io::File *>*> 
        (JvNewObjectArray (numDrives, &java::io::File::class$, NULL));

  ::java::io::File **rootsArray = elements (roots);

  char aDriveRoot[] = {'A', ':', '\\', '\0'};
  mask = 1;
  for (int i = 0, j = 0; i < 26; i++)
    {
      if ((drivesBitmap & mask) != 0)
        {
          rootsArray[j] 
            = new ::java::io::File (JvNewStringLatin1 (aDriveRoot));
          j++;
        }
      mask <<= 1;
      aDriveRoot[0]++;
    }

  return roots;
}
