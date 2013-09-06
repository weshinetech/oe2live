NITROGEN_REL ?= ~/nitrogen/rel/nitrogen
NITROGEN = site/static/nitrogen

all: get-deps compile js css jade

compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar compile)

get-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar get-deps)

update-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar update-deps)

copy-static:
	@(cp -r lib/nitrogen_core/www/* site/static/nitrogen/)

update: update-deps copy-static compile
	@(echo "*** CONGRATULATIONS ***")
	@(echo "Your Nitrogen installation has been upgraded.")
	@(echo "You may need to manually merge any changes that may have been made to")
	@(echo "configuration files as well as the initialization modules:")
	@(echo "    site/src/nitrogen_sup.erl")
	@(echo "    site/src/nitrogen_PLATFORM.erl")
	@(echo "    site/src/nitrogen_app.erl")
	@(echo "")

upgrade: update

clean:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; ./rebar clean)

js:
	cat /dev/null > $(NITROGEN)/lib.js
	cat $(NITROGEN)/jquery.js >> $(NITROGEN)/lib.js
	cat $(NITROGEN)/jquery-ui.js >> $(NITROGEN)/lib.js
	cat $(NITROGEN)/livevalidation.js >> $(NITROGEN)/lib.js
	cat $(NITROGEN)/nitrogen.min.js >> $(NITROGEN)/lib.js
	cat $(NITROGEN)/bert.min.js >> $(NITROGEN)/lib.js

css:
	cat /dev/null > $(NITROGEN)/lib.css
	cat $(NITROGEN)/nitrogen.css >> $(NITROGEN)/lib.css
	cat $(NITROGEN)/../bootstrap/css/bootstrap.min.css >> $(NITROGEN)/lib.css
	cat $(NITROGEN)/../bootstrap/css/bootstrap-responsive.min.css >> $(NITROGEN)/lib.css
	cat $(NITROGEN)/../css/oe2.css >> $(NITROGEN)/lib.css

jade:
	cd $(NITROGEN)/../../templates/; rm *.html; jade --pretty *.jade
