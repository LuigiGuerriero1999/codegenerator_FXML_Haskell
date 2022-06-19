package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;

public class Label extends GTKWidget{
    private String text;

    public Label(String text, double layoutX, double layoutY) {
        this.text = text;
        setLayoutX(layoutX);
        setLayoutY(layoutY);
    }

    public Label(String id, Integer id_hash, String name, String text, double layoutX, double layoutY) {
        super(id, id_hash, makeName(id,id_hash,name), layoutX, layoutY);
        this.text = text;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String gtkHsCode(){
        String template = "${LABELNAME} <- Gtk.labelNew (Just \"${TEXT}\")\n  \n  ";
        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("LABELNAME", super.getName());
        toInsert.put("TEXT", text);
        return StringFormat.format(template, toInsert);
    }
}
