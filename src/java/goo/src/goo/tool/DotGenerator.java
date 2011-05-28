package goo.tool;

import java.util.List;

public class DotGenerator {
	private goo.tool.Goo goo;

	public DotGenerator(goo.tool.Goo goo) {
		this.goo = goo;
	}

	String makeNodeAttributes(String shape, String style, List<String> identifiers) {
		String result = "\t{ node [shape=" + shape + ", style=" + style + "] ";
			for(String name : identifiers) {
				result += name + " ";
			}
			return result + "}\n";
	}

	String makeEdges(List<Edge> edges) {
		String result = "";
		for(Edge edge : edges) {
			result += "\t" + edge.source + " -> " + edge.destination + "\n";
		}
		return result;
	}

	public String getDotText() {
		String result = "digraph {\n";
		result += makeNodeAttributes("box", "filled", goo.getFieldNodes());
		result += makeNodeAttributes("ellipse", "filled", goo.getNonVisibleMethodNodes());
		result += makeNodeAttributes("ellipse", "bold", goo.getPublicMethodNodes());
		result += makeEdges(goo.getEdges());
		return result + "}\n";
	}

	public static void main(String args[]) {
		if (args.length != 1) {
			System.err.println("Usage: goo <classname>");
			return;
		}
		try {
			System.out.println(new DotGenerator(new goo.tool.Goo(args[0])).getDotText());
		} catch(InvalidClassName e) {
			System.err.println("Unable to load " + args[0]);
		}
	}
}