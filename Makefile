
all: app/Main.hs app/Interactive.hs app/Evaluate.hs src/Guess.hs src/Tester.hs
	stack build
	cp -r .stack-work/install/x86_64-osx/lts-11.10/8.2.2/bin/* .

clean: 
	rm -f game game_interactive