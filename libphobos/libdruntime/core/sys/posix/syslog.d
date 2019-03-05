/**
 * D header file for POSIX system logger API.
 * (http://pubs.opengroup.org/onlinepubs/007904875/basedefs/syslog.h.html)
 *
 * Copyright: Copyright Adil Baig 2013.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Adil Baig
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Adil Baig 2013.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.syslog;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):

extern (C) nothrow @nogc:

version (CRuntime_Glibc)
{
    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,  /* log the pid with each message */
        LOG_CONS   = 0x02,  /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,  /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,  /* don't delay open */
        LOG_NOWAIT = 0x10,  /* don't wait for console forks: DEPRECATED */
        LOG_PERROR = 0x20,  /* log to stderr as well */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (Darwin)
{
    //http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/osfmk/sys/syslog.h

    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,     /* log the pid with each message */
        LOG_CONS   = 0x02,  /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,  /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,  /* don't delay open */
        LOG_NOWAIT = 0x10,  /* don't wait for console forks: DEPRECATED */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (FreeBSD)
{
    //http://fxr.watson.org/fxr/source/sys/syslog.h

    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,    /* log the pid with each message */
        LOG_CONS   = 0x02,    /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,    /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,    /* don't delay open */
        LOG_NOWAIT = 0x10,    /* don't wait for console forks: DEPRECATED */
        LOG_PERROR = 0x20,    /* log to stderr as well */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */
        LOG_NTP    = (12<<3), /* NTP subsystem */
        LOG_SECURITY = (13<<3), /* security subsystems (firewalling, etc.) */
        LOG_CONSOLE  = (14<<3), /* /dev/console output */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (NetBSD)
{
    //http://fxr.watson.org/fxr/source/sys/syslog.h

    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,    /* log the pid with each message */
        LOG_CONS   = 0x02,    /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,    /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,    /* don't delay open */
        LOG_NOWAIT = 0x10,    /* don't wait for console forks: DEPRECATED */
        LOG_PERROR = 0x20,    /* log to stderr as well */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */
        LOG_NTP    = (12<<3), /* NTP subsystem */
        LOG_SECURITY = (13<<3), /* security subsystems (firewalling, etc.) */
        LOG_CONSOLE  = (14<<3), /* /dev/console output */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (DragonFlyBSD)
{
    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,    /* log the pid with each message */
        LOG_CONS   = 0x02,    /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,    /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,    /* don't delay open */
        LOG_NOWAIT = 0x10,    /* don't wait for console forks: DEPRECATED */
        LOG_PERROR = 0x20,    /* log to stderr as well */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */
        LOG_NTP    = (12<<3), /* NTP subsystem */
        LOG_SECURITY = (13<<3), /* security subsystems (firewalling, etc.) */
        LOG_CONSOLE  = (14<<3), /* /dev/console output */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (Solaris)
{
    //http://pubs.opengroup.org/onlinepubs/007904875/basedefs/syslog.h.html

    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID = 0x01,     /* log the pid with each message */
        LOG_CONS   = 0x02,  /* log on the console if errors in sending */
        LOG_NDELAY = 0x08,  /* don't delay open */
        LOG_NOWAIT = 0x10,  /* don't wait for console forks: DEPRECATED */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
else version (CRuntime_UClibc)
{
    //PRIORITY
    enum {
        LOG_EMERG = 0,     /* system is unusable */
        LOG_ALERT = 1,     /* action must be taken immediately */
        LOG_CRIT  = 2,     /* critical conditions */
        LOG_ERR   = 3,     /* error conditions */
        LOG_WARNING = 4,   /* warning conditions */
        LOG_NOTICE  = 5,   /* normal but significant condition */
        LOG_INFO    = 6,   /* informational */
        LOG_DEBUG   = 7,   /* debug-level messages */
    };

    //OPTIONS
    enum {
        LOG_PID    = 0x01,  /* log the pid with each message */
        LOG_CONS   = 0x02,  /* log on the console if errors in sending */
        LOG_ODELAY = 0x04,  /* delay open until first syslog() (default) */
        LOG_NDELAY = 0x08,  /* don't delay open */
        LOG_NOWAIT = 0x10,  /* don't wait for console forks: DEPRECATED */
        LOG_PERROR = 0x20,  /* log to stderr as well */
    };

    //FACILITY
    enum {
        LOG_KERN   = (0<<3),  /* kernel messages */
        LOG_USER   = (1<<3),  /* random user-level messages */
        LOG_MAIL   = (2<<3),  /* mail system */
        LOG_DAEMON = (3<<3),  /* system daemons */
        LOG_AUTH   = (4<<3),  /* security/authorization messages */
        LOG_SYSLOG = (5<<3),  /* messages generated internally by syslogd */
        LOG_LPR    = (6<<3),  /* line printer subsystem */
        LOG_NEWS   = (7<<3),  /* network news subsystem */
        LOG_UUCP   = (8<<3),  /* UUCP subsystem */
        LOG_CRON   = (9<<3),  /* clock daemon */
        LOG_AUTHPRIV = (10<<3), /* security/authorization messages (private), */
        LOG_FTP    =  (11<<3), /* ftp daemon */

        /* other codes through 15 reserved for system use */
        LOG_LOCAL0 = (16<<3), /* reserved for local use */
        LOG_LOCAL1 = (17<<3), /* reserved for local use */
        LOG_LOCAL2 = (18<<3), /* reserved for local use */
        LOG_LOCAL3 = (19<<3), /* reserved for local use */
        LOG_LOCAL4 = (20<<3), /* reserved for local use */
        LOG_LOCAL5 = (21<<3), /* reserved for local use */
        LOG_LOCAL6 = (22<<3), /* reserved for local use */
        LOG_LOCAL7 = (23<<3), /* reserved for local use */

        LOG_NFACILITIES = 24,  /* current number of facilities */
    };

    int LOG_MASK(int pri) { return 1 << pri; }        /* mask for one priority */
    int LOG_UPTO(int pri) { return (1 << (pri+1)) - 1; }  /* all priorities through pri */

    void openlog (const char *, int __option, int __facility);
    int  setlogmask (int __mask);
    void syslog (int __pri, const char *__fmt, ...);
    void closelog();
}
