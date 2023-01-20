(define-module (tuile gumac)
  #:export
  (
   gumac-write
   gumac-puts
   gumac-hookbeg
   gumac-hookend
   gumac-hookesc
   gumac-sethook
   gumac-sethookbeg
   gumac-sethookend
   gumac-sethookesc
   gumac-seteater
   gumac-multihook
   gumac-ifilename
   gumac-ilinenumber
   gumac-ofilename
   gumac-olinenumber
   gumac-pushinput
   gumac-closeinput
   gumac-pushoutput
   gumac-closeoutput
   gumac-block
   gumac-unblock
   gumac-process-file-to-file
   ))

(load-extension "libgumac" "init_gumac")
