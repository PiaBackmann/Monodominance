#################################################    For Martin
#     Makefile for Monodominance Project        #
#                                               #
#        by Pia Backmann        Dezember 2014   #
#################################################
#

# Initialize system dependent vars---------------------------------------

	SHELL                =   /bin/sh
	BASE_DIR = /home/pia/Arbeitsfläche/Haupthypothesen/Monodominanz/Modell/Code/C++/Neu_Review
#------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
CC  = g++
CFLAGS  = -g -Wall -O2 -lc
# -pg statt -g für gnuprof...
# sonst noch: --pedantic
#-I/usr/include/gsl
LDFLAGS = -lm  
#-L/usr/lib/gsl -lgsl -lgslcblas

#--------------------------------------------------------------------------------------------------------


OBJS =  mono_basics.o \
	mono_cluster.o \
	mono_inits.o \
	mono_kills.o \
	mono_main.o \
	mono_routinen.o 
        #neue Zeile: Leerzeichen, backslash \

all:        Monodominance install

Monodominance:        $(OBJS) 
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)
#die obige zeile MUSS mit <tab> anfangen! das executive heisst jetzt: bat ->das macht das "@"

%.o: %.cpp mainwindow.h
	$(CC) $(CFLAGS) -c $< 
#die obige zeile MUSS mit <tab> anfangen!


install:
	
	cp Monodominance ~/

clean:        
	rm -f *.o core
	rm -f $(SRC_DIR)/*.o

.PHONY: all clean

