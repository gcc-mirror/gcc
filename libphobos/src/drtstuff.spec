%rename startfile startfile_orig
*startfile: %(startfile_orig) drtbegin.o%s

%rename endfile endfile_orig
*endfile: %(endfile_orig) drtend.o%s
