package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class RadioButton extends GTKWidget{
    private String text;

    public RadioButton(String text) {
        this.text = text;
    }

    public RadioButton(String id, Integer id_hash, String name, double layoutX, double layoutY, String text) {
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
        StringBuilder template = new StringBuilder();
        if (!Objects.equals(text, "")) template.append("${RADIOBUTTONNAME} <- Gtk.radioButtonNewWithLabelFromWidget (Nothing::Maybe Gtk.RadioButton) \"${TEXT}\"\n  ");
        else template.append("${RADIOBUTTONNAME} <- Gtk.radioButtonNewFromWidget (Nothing::Maybe Gtk.RadioButton)\n  ");
        template.append("\n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("RADIOBUTTONNAME", super.getName());
        toInsert.put("TEXT", text);

        return StringFormat.format(template.toString(), toInsert);
    }
}
