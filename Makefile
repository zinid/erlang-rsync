all: src

src:
	rebar compile

clean:
	rebar clean

distclean: clean
	rm -f config.status
	rm -f config.log
	rm -rf autom4te.cache
	rm -rf deps
	rm -rf ebin
	rm -f vars.config
	rm -f doc/overview.edoc
	rm -rf doc/*.html
	rm -rf doc/*.css
	rm -rf doc/*.png
	rm -f doc/edoc-info
	rm -f src/rsync.app.src

test: all
	rebar skip_deps=true eunit

doc:
	rebar doc

xref: all
	rebar xref

.PHONY: clean src test distclean doc xref
