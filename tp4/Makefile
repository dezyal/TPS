# Makefile generique.
 
# Nom du programme
PRG= tp4
 
# Fichiers ML du programme
ML= main.ml
MLI= # A REMPLIR avec la liste des fichiers .mli !
 
# Les listes de fichiers à produire
CMO=${ML:.ml=.cmo}
CMX=${ML:.ml=.cmx}
CMI=${MLI:.mli=.cmi}
 
# Les compilateurs
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDEP=ocamldep
 
${PRG}: ${CMX}
	${OCAMLOPT} -o $@ ${CMX}
 
${PRG}.byte: ${CMO}
	${OCAMLC} -o ${CMO}
 
.SUFFIXES: .ml .mli .cmo .cmx .cmi
 
.ml.cmx:
	${OCAMLOPT} -c $<
 
.ml.cmo:
	${OCAMLC} -c $<
 
.mli.cmi:
	${OCAMLC} -c $<
 
clean::
	rm -f *.cm? *.o *~
	rm -f ${PRG} ${PRG}.byte
 
fullclean:: clean
	rm -f .depend
 
# Creation du fichier de dependances
# Il faut appeller make depend avant de recompiler votre programme
# Si vous utilisez le GNU make (standard sous linux) il faudra inclure
# le fichier. BSD make (bmake sur vos rack) inclut automatiquement le
# fichier .depend s'il existe
#depend: .depend
#.depend: ${ML} ${MLI}
#	rm -f .depend
#	${OCAMLDEP} ${ML} ${MLI} > .depend
 
# Inclusion du fichier de dependance (GNU Make)
# GNU Make "reconstruira" le fichier .depend s'il n'existe pas.
#include .depend
 
# Il faut toujours un (au moins) saut de ligne en fin de fichier,
# c'est pour ca que l'on finit toujours les Makefile par un
# commentaire.
# END
