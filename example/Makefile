
LP 	= 	../lipsum/lipsum

MLSRC 	= luaclient.ml

all: 		lipsum luaclient.ml

clean:
		rm -f $(MLSRC)

%.ml: 		%.nw
		$(LP) tangle $@ $< > $@

lipsum: 	FORCE
		@[ -x $(LP) ] || (echo "build the lipsum tool first" && false)
		
FORCE:
