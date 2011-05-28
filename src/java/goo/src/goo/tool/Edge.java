package goo.tool;

public class Edge {
    protected String source;
    protected String destination;

    public Edge(String source, String destination) {
        this.source = source;
        this.destination = destination;
    }

    public boolean containsSubstring(String filterText) {
        return source.contains(filterText) || destination.contains(filterText);           
    }
}
