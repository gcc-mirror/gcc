#if _G_HAVE_IO_FILE_OPEN
  return (filebuf*)_IO_file_open (this, filename, posix_mode, prot,
				  read_write, 0);
#else
#endif
#if _G_IO_IO_FILE_VERSION == 0x20001
  return (filebuf*)_IO_file_fopen(this, filename, mode, 0);
#else
#endif
