package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;
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
        StringBuilder template = new StringBuilder();
        if(!Objects.equals(text, "")) template.append("${LINKBUTTONNAME} <- Gtk.linkButtonNewWithLabel \"\" (Just \"${TEXT}\")\n  ");
        else template.append("${LINKBUTTONNAME} <- Gtk.linkButtonNew \"\"\n  ");
        if (visited) template.append("Gtk.linkButtonSetVisited ${LINKBUTTONNAME} True\n  ");
        template.append("\n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("LINKBUTTONNAME", super.getName());
        toInsert.put("TEXT", text);

        return StringFormat.format(template.toString(),toInsert);
    }
}
