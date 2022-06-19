package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;

public class Layout extends GTKWidget{
    public Layout(double layoutX, double layoutY) {
        setLayoutX(layoutX);
        setLayoutY(layoutY);
    }
    public Layout(String id, Integer id_hash, String name, double layoutX, double layoutY) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
    }

    @Override
    public String gtkHsCode(){
        String template = "${LAYOUTNAME}  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)\n  \n  ";
        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("LAYOUTNAME", super.getName());
        return StringFormat.format(template, toInsert);
    }
}
