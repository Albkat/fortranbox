FC = gfortran
FFLAGS = -Wall -g

PROGRAM = random
OBJS = random.o 

all: $(PROGRAM)

$(PROGRAM):$(OBJS)
	$(FC) $(FFLAGS) -o $(PROGRAM) $(OBJS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f $(PROGRAM) $(OBJS)