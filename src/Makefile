# ============================================================================# 
#                                                                               
#                                                                               
# ============================================================================# 

L_GMP = -L./GHC/local_lib

PROG = Main 

#language = -fglasgow-exts  -fallow-undecidable-instances       \
#           -fallow-overlapping-instances                       \
#           -fno-warn-overlapping-patterns  -fwarn-unused-binds \
#           -fwarn-unused-matches  -fwarn-unused-imports  
# ============================================================================# 
all: RS_mso.o 
	ghc -fglasgow-exts -H32M -hidir T -odir T -o $(PROG) --make Main
#	ghc -fglasgow-exts -H32M -hidir T -odir T -o $(PROG) --make Main RS_mso.o $(L_GMP)
#	ghc -fglasgow-exts -package lang -H32M -hidir T -odir T -o $(PROG) --make Main RS_mso.o $(L_GMP)

RS_mso.o: 
	gcc -o $@ -c RS_mso.c 
# ============================================================================#
clean : 
	rm -f  *.hi *.o  T/*.hi T/*.o $(PROG)  
# ============================================================================#

ARX_NAME = haskel
#------------------------------------------------------------- 
save :  
	tar --mode=a-x  --ignore-failed-read -cvf $(ARX_NAME).tar  \
	Makefile *.hs *.txt
	gzip -6 $(ARX_NAME).tar 
	(cd ../../A/haskel; mdat) 

# ============================================================================#
  
