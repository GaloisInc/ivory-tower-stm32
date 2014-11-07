
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../../ivory/ivory
	cabal sandbox add-source ../../ivory/ivory-artifact
	cabal sandbox add-source ../../ivory/ivory-hw
	cabal sandbox add-source ../../ivory/ivory-opts
	cabal sandbox add-source ../../ivory/ivory-stdlib
	cabal sandbox add-source ../../ivory/ivory-backend-c
	cabal sandbox add-source ../../tower/tower
	cabal install --dependencies-only

clean:
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
