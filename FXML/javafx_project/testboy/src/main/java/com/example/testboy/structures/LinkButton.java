package com.example.testboy.structures;

import java.util.Objects;

public class LinkButton extends GTKWidget{
    private String text;
    private boolean visited;

    public LinkButton(String id, Integer id_hash, String name, double layoutX, double layoutY, String text, boolean visited) {
        super(id, id_hash, makeName(id, id_hash, name), layoutX, layoutY);
        this.text = text;
        this.visited = visited;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public boolean isVisited() {
        return visited;
    }

    public void setVisited(boolean visited) {
        this.visited = visited;
    }

    @Override
    public String gtkHsCode(){
        String hyperlink = super.getName() + " <- Gtk.linkButtonNew "+ "\"" + "\"" + "\n  " ;
        if (!Objects.equals(text, "")) hyperlink = super.getName() + " <- Gtk.linkButtonNewWithLabel " + "\"" + "\""+ " (Just "+ "\""+text+"\""+")\n  ";

        String hyperVisited = "";
        if (visited) hyperVisited = "Gtk.linkButtonSetVisited " + super.getName() + " True\n  ";

        return hyperlink + hyperVisited + "\n  ";
    }
}
