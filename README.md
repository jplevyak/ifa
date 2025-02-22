BUILD

  You need to pull some other software

  1. the Boehm GC and PCRE libraries:

     On Fedora:
        yum install gc gc-devel

     On Ubuntu (or other debian distros):
         apt-get install libgc-dev libpcre++-dev

  2. dparser

     This just need to build and install this. Note: You should enable the
     gc garbage collector (it's an option in the Makefile, or provide as
     an option to make:

        git clone https://github.com/jplevyak/dparser.git
        (cd dparser; sudo make install D_USE_GC=1)

  3. plib

     This is expected to exist in ../plib.

        git clone https://github.com/jplevyak/plib.git
        (cd plib; make USE_GC=1)

  4. ifa

     This is expected to exist in ../ifa.

        git clone https://github.com/jplevyak/ifa.git
        (cd ifa; make)
