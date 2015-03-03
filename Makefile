.PHONY : test

index.html : README.md header.html footer.html
	cat header.html > index.html
	cat README.md >> index.html
	cat footer.html >> index.html

test :
	doctest -isrc src/HelHUG/Part1.hs
	doctest -isrc src/HelHUG/Part1b.hs
	doctest -isrc src/HelHUG/Part1c.hs
	doctest -isrc src/HelHUG/Part2.hs
	doctest -isrc src/HelHUG/Part3.hs
