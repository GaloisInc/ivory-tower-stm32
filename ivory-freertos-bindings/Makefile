
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../../ivory/ivory
	cabal sandbox add-source ../../ivory/ivory-artifact
	cabal install --dependencies-only

clean:
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config

