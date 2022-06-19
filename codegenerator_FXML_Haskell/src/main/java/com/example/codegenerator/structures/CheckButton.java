package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class CheckButton extends GTKWidget{
    private String text;

    public CheckButton(String text) {
        this.text = text;
    }

    public CheckButton(String id, Integer id_hash, String name, double layoutX, double layoutY, String text) {
        super(id, id_hash, makeName(id, id_hash, name), layoutX, layoutY);
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
        String template;
        if (!Objects.equals(text, "")){
            template = "${CHECKBUTTONNAME} <- Gtk.checkButtonNewWithLabel \"${TEXT}\"\n  \n  ";
        }else{
            template = "${CHECKBUTTONNAME} <- Gtk.checkButtonNew\n  \n  ";
        }

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("CHECKBUTTONNAME", super.getName());
        toInsert.put("TEXT", text);

        return StringFormat.format(template,toInsert);
    }
}
