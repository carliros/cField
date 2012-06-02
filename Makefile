SRCDERIVED = ./src-generated/
SRC = ./src/
AG = ./ag/
OUT = ./out/

all: uuagc
	ghc -i$(SRC):$(SRCDERIVED) --make -o cFields $(SRC)Main.hs -outputdir $(OUT)

uuagc:
	uuagc --data --catas --semfuns --signatures -P $(AG) $(AG)DataSemFuns.ag
	


