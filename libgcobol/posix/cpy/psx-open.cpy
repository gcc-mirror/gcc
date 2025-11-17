        >> PUSH source format
        >>SOURCE format is fixed

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This file is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in November 2025.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * >>DEFINE O_ACCMODE AS 3

        >>DEFINE O_APPEND      AS     1024
      * >>DEFINE O_ASYNC                   not used by open(2)
        >>DEFINE O_CLOEXEC     AS   524288
        >>DEFINE O_CREAT       AS       64
        >>DEFINE O_DIRECT      AS    16384 
        >>DEFINE O_DIRECTORY   AS    65536
        >>DEFINE O_DSYNC       AS     4096
        >>DEFINE O_EXCL        AS      128
        >>DEFINE O_LARGEFILE   AS    32768
        >>DEFINE O_NOATIME     AS   262144
        >>DEFINE O_NOCTTY      AS      256
        >>DEFINE O_NOFOLLOW    AS   131072
        >>DEFINE O_NONBLOCK    AS     2048
        >>DEFINE O_PATH        AS  2097152
        >>DEFINE O_RDONLY      AS        0
        >>DEFINE O_RDWR        AS        2
        >>DEFINE O_SYNC        AS  1052672
        >>DEFINE O_TMPFILE     AS  4194304 + O_DIRECTORY
        >>DEFINE O_TRUNC       AS      512
        >>DEFINE O_WRONLY      AS        1


      * >>DEFINE S_IFBLK  AS 24576
      * >>DEFINE S_IFCHR  AS  8192
      * >>DEFINE S_IFDIR  AS 16384
      * >>DEFINE S_IFIFO  AS  4096
      * >>DEFINE S_IFLNK  AS 40960
      * >>DEFINE S_IFMT   AS 61440
      * >>DEFINE S_IFREG  AS 32768
      * >>DEFINE S_IFSOCK AS 49152
        
        >>DEFINE S_IRGRP  AS    32
        >>DEFINE S_IROTH  AS     4
        >>DEFINE S_IRUSR  AS   256
        >>DEFINE S_IRWXG  AS    56
        >>DEFINE S_IRWXO  AS     7
        >>DEFINE S_IRWXU  AS   448
        >>DEFINE S_ISGID  AS  1024
        >>DEFINE S_ISUID  AS  2048
        >>DEFINE S_ISVTX  AS   512
        >>DEFINE S_IWGRP  AS    16
        >>DEFINE S_IWOTH  AS     2
        >>DEFINE S_IWUSR  AS   128
        >>DEFINE S_IXGRP  AS     8
        >>DEFINE S_IXOTH  AS     1
        >>DEFINE S_IXUSR  AS    64

        >> POP source format

