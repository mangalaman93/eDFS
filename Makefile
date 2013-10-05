# See LICENSE for licensing information.

# relative application sub-directories
APPS = master worker client 
.PHONY: apps $(APPS)

all: apps

apps: $(APPS)
	
$(APPS):
	@echo "** Building $@"
	@$(MAKE) -C $@

# uncommnet to build client and worker apps first, before the master app
# master: client worker 

