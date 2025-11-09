FPC = fpc
SRC = example.pas
OUT = example
UNITS = GraphAllegro5Port.pas

# Allegro5 libraries passed to linker via -k
ALLEGRO_LIBS = -k-lallegro -k-lallegro_primitives -k-lallegro_font -k-lallegro_ttf

# Add Pascal bindings path to allegro-pas library
UNIT_PATHS = -Fu. -Fu"${HOME}/fpcupdeluxe/ccr/allegro-pas/src/lib/"

all: $(OUT)

$(OUT): $(SRC) $(UNITS)
	   $(FPC) -Mobjfpc -Scgi -O2 -g -gl $(UNIT_PATHS) $(SRC) -o$(OUT) $(ALLEGRO_LIBS)

clean:
	rm -f $(OUT) *.o *.ppu
