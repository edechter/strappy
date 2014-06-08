
import os
from os.path import join, exists
import os.path

import xml.etree.ElementTree as ET
import pydot 
import itertools

def parse_xbif(filename):
	assert(exists(filename))
	tree = ET.parse(filename)
	root = tree.getroot()

	variable_nodes = root[0].findall("./VARIABLE")
	prob_nodes = root[0].findall("./PROBABILITY")

	variables = [v.find("./NAME").text for v in variable_nodes]

	edges = []
	for p in prob_nodes:
		v = p.find("./FOR").text
		pars = p.findall("./GIVEN")
		edges.extend([(v, par.text) for par in pars])

	graph = pydot.graph_from_edges(edges, directed=True)
	fout = open(os.path.splitext(os.path.basename(filename))[0] +".pdf", "w")
	print >>fout, graph.create_pdf()
	fout.close()

	return variables, edges

def parse_uai(fname):
	assert(exists(filename))

	lines = open(filename, "r").readlines()
	nvars = int(lines[1].strip())
	ncliques = int(lines[3].strip())

	edges = []
	for line in lines[4:(ncliques+4)]:
		line = line.strip()
		vs = [int(s) for s in line.split() if s.isdigit()]
		edges.extend(list(itertools.combinations(vs, 2)))

	graph = pydot.graph_from_edges(edges, directed=False)
	fout = open(os.path.splitext(os.path.basename(fname))[0] +".pdf", "w")
	print >>fout, graph.create_pdf()
	fout.close()
	return graph


if __name__ == "__main__":
	filename="uai/rbm_20.uai"

	graph = parse_uai(filename)