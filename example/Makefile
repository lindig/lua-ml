
LP 	= 	lipsum

MLSRC 	= luaclient.ml

all: 		luaclient.ml

clean:
		rm -f $(MLSRC)

%.ml: 		%.nw
		$(LP) tangle $@ $< > $@

FORCE:
