MT_CFLAGS = `case "$(CXXFLAGS)" in *-pthread* ) echo -D_PTHREADS ;; esac`
