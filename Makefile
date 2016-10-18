all: s_poly s_mlton

s_poly: scancom.sml s.sml s-poly.sml
	polyc -o s_poly s-poly.sml

s_mlton: scancom.sml s.sml main.sml s.mlb
	mlton -output s_mlton s.mlb

clean:
	rm -rf s_poly s_mlton 
