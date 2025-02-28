### To compile for all supported plaforms, type "make release"
### To compile for a specific OS (e.g. linux) run "make release-OS" (e.g. "make release-linux"
### If you use make that way, you'll find executables in ../releases

### This makefile can only be run on macos and linux 
###
### This makefile is not actually needed to build the software; it's
### just a set of shortcuts.  To compile on a specific OS of which you
### know the RID, run:
###
### dotnet build -c release -r RID
###
### For instance, to compile for windows, run
### dotnet build win-x64
###
### Currently supported RIDs are linux-x64, osx-x64, win-x64
###
### the program will be compiled in
### ./bin/release/netcoreapp2.x/win-x64/publish/voxlogica

PROJ=GraphLogicA
UNAME := $(shell uname)
VERSION=$(shell cat VERSION.txt)
RELEASENAME=$(PROJ)_$(VERSION)_$(RID)
RELEASE=../releases/$(RELEASENAME)

ifeq ($(VERSION),"") 
		$(error "VERSION is not set; set the release number (e.g. 0.5.1) in VERSION.txt") 
endif

ifeq ($(UNAME),Linux)
CURRENTRID=linux-x64
else
CURRENTRID=win-x64
endif

.PHONY: build clean restore release-linux release-osx release-win publish-internal release

build: *.fs $(PROJ).fsproj
	TERM=xterm DOTNET_CLI_TELEMETRY_OPTOUT=1 dotnet build -c release -r $(CURRENTRID) 

temporary:
	@echo Check your makefile, this is a temporary target!
	@echo To get dotnet warp run: dotnet tool install --global dotnet-warp
	# dotnet warp -r linux-x64
	# mv GraphLogicA GraphLogicA-$(VERSION)-linux
	dotnet warp -r osx-x64
	mv GraphLogicA GraphLogicA-$(VERSION)-osx
	# dotnet warp -r win-x64
	# mv GraphLogicA.exe GraphLogicA-$(VERSION)-win.exe

restore:
	TERM=xterm DOTNET_CLI_TELEMETRY_OPTOUT=1 dotnet restore

publish-internal:
	TERM=xterm DOTNET_CLI_TELEMETRY_OPTOUT=1 dotnet publish -c release -r $(RID)
	mkdir -p $(RELEASE)
	cp -a bin/release/net9.0/$(RID)/publish/* $(RELEASE)
	cp -a BINARY_LICENSE.txt $(RELEASE)
	cd ../releases && rm -f $(RELEASENAME).zip && zip -r $(RELEASENAME).zip $(RELEASENAME)

clean:
	rm -rf bin obj

release-linux:
	make publish-internal RID=linux-x64

release-osx:
	make publish-internal RID=osx-x64

release-win:
	make publish-internal RID=win-x64

release: release-linux release-osx release-win

install-linux: release-linux
	#TODO: this is temporary; should be made portable
	echo sudo "cd /opt && $(PWD)/$(RELEASE)linux-x64.zip"	