***   Some random stuff for testing libU77.  Should be done better.  It's
*     hard to test things where you can't guarantee the result.  Have a
*     good squint at what it prints, though detected errors will cause 
*     starred messages.

      implicit none
      integer i, j, k, ltarray (9), idat (3), count, rate, count_max,
     +     pid, mask
      real tarray1(2), tarray2(2), r1, r2, sum
      intrinsic getpid, getuid, getgid, ierrno, gerror,
     +     fnum, isatty, getarg, access, unlink, fstat,
     +     stat, lstat, getcwd, gmtime, hostnm, etime, chmod,
     +     chdir, fgetc, fputc, system_clock, second, idate, secnds,
     +     time, ctime, fdate, ttynam, date_and_time
      external lenstr
      integer lenstr
      logical l
      character gerr*80, c*1
      character ctim*25, line*80, lognam*20, wd*100, line2*80, ddate*8,
     +     ttime*10, zone*5
      integer fstatb (13), statb (13)
      integer *2 i2zero
      integer values(8)

      ctim = ctime(time())
      WRITE (6,'(A/)') '1 GNU libU77 test at: ' // ctim
      write (6,'(A,I3,'', '',I3)')
     +     ' Logical units 5 and 6 correspond (FNUM) to'
     +     // ' Unix i/o units ', fnum(5), fnum(6)
      if (lnblnk('foo ').ne.3 .or. len_trim('foo ').ne.3) then
        print *, 'LNBLNK or LEN_TRIM failed'
        call exit(1)
      end if
      l= isatty(6)
      line2 = ttynam(6)
      if (l) then
        line = 'and 6 is a tty device (ISATTY) named '//line2
      else
        line = 'and 6 isn''t a tty device (ISATTY)'
      end if
      write (6,'(1X,A)') line(:lenstr(line))

      pid = getpid()
      WRITE (6,'(A,I10)') ' Process id (GETPID): ', pid
      WRITE (6,'(A,I10)') ' User id (GETUID): ', GETUID ()
      WRITE (6,'(A,I10)') ' Group id (GETGID): ', GETGID ()
      WRITE (6, *) 'If you have the `id'' program, the following call'
      write (6, *) 'of SYSTEM should agree with the above:'
      call flush(6)
      CALL SYSTEM ('echo " " `id`')
      call flush
      lognam = 'blahblahblah'
      call getlog (lognam)
      write (6,*) 'Login name (GETLOG): ', lognam
      call umask(0, mask)
      write(6,*) 'UMASK returns', mask
      call umask(mask)

      ctim = fdate()
      write (6,*) 'FDATE returns: ', ctim
      j=time()
      call ltime (j, ltarray)
      write (6,'(1x,a,9i4)') 'LTIME returns:', ltarray
      call gmtime (j, ltarray)
      write (6,'(1x,a,9i4)') 'GMTIME returns:', ltarray
      call system_clock(count)  ! omitting optional args
      call system_clock(count, rate, count_max)
      write(6,*) 'SYSTEM_CLOCK returns: ', count, rate, count_max
      call date_and_time(ddate)  ! omitting optional args
      call date_and_time(ddate, ttime, zone, values)
      write(6, *) 'DATE_AND_TIME returns: ', ddate, ' ', ttime, ' ',
     +     zone, ' ', values

      write (6,*) 'Sleeping for 1 second (SLEEP) ...'
      call sleep (1)

c consistency-check etime vs. dtime for first call
      r1 = etime (tarray1)
      if (r1.ne.tarray1(1)+tarray1(2))
     +     write (6,*) '*** ETIME didn''t return sum of the array: ',
     +     r1, ' /= ', tarray1(1), '+', tarray1(2)
      r2 = dtime (tarray2)
      if (abs (r1-r2).gt.1.0) write (6,*)
     +     'Results of ETIME and DTIME differ by more than a second:',
     +     r1, r2
      call sgladd (sum, tarray1(1), tarray1(2))
      if (r1 .ne. sum)
     +     write (6,*) '*** ETIME didn''t return sum of the array: ',
     +     r1, ' /= ', tarray1(1), '+', tarray1(2)
      call sgladd (sum, tarray2(1), tarray2(2))
      if (r2 .ne. sum)
     +     write (6,*) '*** DTIME didn''t return sum of the array: ',
     +     r2, ' /= ', tarray2(1), '+', tarray2(2)
      write (6, '(A,3F10.3)')
     +     ' Elapsed total, user, system time (ETIME): ',
     +     r1, tarray1

c now try to get times to change enough to see in etime/dtime
      write (6,*) 'Looping until clock ticks at least once...'
      do i = 1,1000
      do j = 1,1000
      end do
      r2 = dtime (tarray2)
      if (tarray2(1) .ne. 0. .or. tarray2(2) .ne. 0.) exit
      end do
      r1 = etime (tarray1)
      call sgladd (sum, tarray1(1), tarray1(2))
      if (r1 .ne. sum)
     +     write (6,*) '*** ETIME didn''t return sum of the array: ',
     +     r1, ' /= ', tarray1(1), '+', tarray1(2)
      call sgladd (sum, tarray2(1), tarray2(2))
      if (r2 .ne. sum)
     +     write (6,*) '*** DTIME didn''t return sum of the array: ',
     +     r2, ' /= ', tarray2(1), '+', tarray2(2)
      write (6, '(A,3F10.3)')
     +     ' Differences in total, user, system time (DTIME): ',
     +     r2, tarray2
      write (6, '(A,3F10.3)')
     +     ' Elapsed total, user, system time (ETIME): ',
     +     r1, tarray1
      write (6, *) '(Clock-tick detected after ', i, ' 1K loops.)'

      call idate (i,j,k)
      call idate (idat)
      write (6,*) 'IDATE d,m,y: ',idat
      print *,  '... and the VXT version: ', i,j,k
      call time(line(:8))
      print *, 'TIME: ', line(:8)
      write (6,*) 'SECNDS(0.0) returns: ',secnds(0.0)
      write (6,*) 'SECOND returns: ', second()
      call dumdum(r1)
      call second(r1)
      write (6,*) 'CALL SECOND returns: ', r1
      i = getcwd(wd)
      if (i.ne.0) then
        call perror ('*** getcwd')
      else
        write (6,*) 'Current directory is "'//wd(:lenstr(wd))//'"'
      end if
      call chdir ('.',i)
      if (i.ne.0) write (6,*) '***CHDIR to ".": ', i
      i=hostnm(wd)
      if(i.ne.0) then
        call perror ('*** hostnm')
      else
        write (6,*) 'Host name is ', wd(:lenstr(wd))
      end if
      i = access('/dev/null ', 'rw')
      if (i.ne.0) write (6,*) '***Read/write ACCESS to /dev/null: ', i
      write (6,*) 'Creating file "foo" for testing...'
      open (3,file='foo',status='UNKNOWN')
      rewind 3
      call fputc(3, 'c',i)
      call fputc(3, 'd',j)      
      if (i+j.ne.0) write(6,*) '***FPUTC: ', i
C     why is it necessary to reopen?  (who wrote this?)
C     the better to test with, my dear!  (-- burley)
      close(3)
      open(3,file='foo',status='old')
      call fseek(3,0,0,*10)
      go to 20
 10   write(6,*) '***FSEEK failed'
 20   call fgetc(3, c,i)
      if (i.ne.0) write(6,*) '***FGETC: ', i
      if (c.ne.'c') write(6,*) '***FGETC read the wrong thing: ',
     +     ichar(c)
      i= ftell(3)
      if (i.ne.1) write(6,*) '***FTELL offset: ', i
      call chmod ('foo', 'a+w',i)
      if (i.ne.0) write (6,*) '***CHMOD of "foo": ', i
      i = fstat (3, fstatb)
      if (i.ne.0) write (6,*) '***FSTAT of "foo": ', i
      i = stat ('foo', statb)
      if (i.ne.0) write (6,*) '***STAT of "foo": ', i
      write (6,*) '  with stat array ', statb
      if (statb(5).ne.getuid () .or. statb(6).ne.getgid() .or. statb(4)
     +     .ne. 1) write (6,*) '*** FSTAT uid, gid or nlink is wrong'
      do i=1,13
        if (fstatb (i) .ne. statb (i))
     +       write (6,*) '*** FSTAT and STAT don''t agree on '// '
     +       array element ', i, ' value ', fstatb (i), statb (i)
      end do
      i = lstat ('foo', fstatb)
      do i=1,13
        if (fstatb (i) .ne. statb (i))
     +       write (6,*) '*** LSTAT and STAT don''t agree on '// '
     +       array element ', i, ' value ', fstatb (i), statb (i)
      end do

C     in case it exists already:
      call unlink ('bar',i)
      call link ('foo ', 'bar ',i)
      if (i.ne.0)
     +     write (6,*) '***LINK "foo" to "bar" failed: ', i
      call unlink ('foo',i)
      if (i.ne.0) write (6,*) '***UNLINK "foo" failed: ', i
      call unlink ('foo',i)
      if (i.eq.0) write (6,*) '***UNLINK "foo" again: ', i
      call gerror (gerr)
      i = ierrno()
      write (6,'(A,I3,A/1X,A)') ' The current error number is: ',
     +     i,
     +     ' and the corresponding message is:', gerr(:lenstr(gerr))
      write (6,*) 'This is sent to stderr prefixed by the program name'
      call getarg (0, line)
      call perror (line (:lenstr (line)))
      call unlink ('bar')
      WRITE (6,*) 'You should see exit status 1'
      CALL EXIT(1)
 99   END

      integer function lenstr (str)
C     return length of STR not including trailing blanks, but always
C     return >0
      character *(*) str
      if (str.eq.' ') then
        lenstr=1
      else
        lenstr = lnblnk (str)
      end if
      end
* just make sure SECOND() doesn't "magically" work the second time.
      subroutine dumdum(r)
      r = 3.14159
      end
* do an add that is most likely to be done in single precision.
      subroutine sgladd(sum,left,right)
      implicit none
      real sum,left,right
      sum = left+right
      end
