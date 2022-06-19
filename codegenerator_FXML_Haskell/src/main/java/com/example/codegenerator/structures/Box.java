package com.example.codegenerator.structures;


import main.java.com.example.codegenerator.StringFormat;
import com.example.codegenerator.orientation.Orientation;

import java.util.HashMap;
import java.util.Map;

public class Box extends GTKWidget{
    private int spacing;
    private Orientation orientation;

    public Box(String id, Integer id_hash, String name, double layoutX, double layoutY, int spacing, Orientation orientation) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.spacing = spacing;
        this.orientation = orientation;
    }

    public int getSpacing() {
        return spacing;
    }

    public void setSpacing(int spacing) {
        this.spacing = spacing;
    }

    public Orientation getOrientation() {
        return orientation;
    }

    public void setOrientation(Orientation orientation) {
        this.orientation = orientation;
    }

    @Override
    public String gtkHsCode(){
        String template = "${NAME} <- Gtk.boxNew Gtk.${ORIENTATION} ${SPACING} \n  \n";

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("NAME", super.getName());
        toInsert.put("ORIENTATION", orientation);
        toInsert.put("SPACING", spacing);

        return StringFormat.format(template,toInsert);
    }
}