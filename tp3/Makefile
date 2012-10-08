# TP sdl
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
tpsdl: tpsdl.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o tpsdl tpsdl.ml
 
clean::
	rm -f *~ *.o *.cm? tpsdl
 
# FIN
